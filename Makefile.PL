#
# Setup Inline::SLang
# - code taken from Makefile.PL in Inline-Ruby-0.02
#   (Neil Watkiss), although all errors should be assumed to
#   be mine and not Neil's
#
# Notes:
#  Have tried to use File::Spec to ensure works across different
#  filesystems, however:
#    a) I don't know the rules for other systems so I can't promise
#       that I've done things correctly (and they're almost certain
#       not sensible as I'm reluctant to cash many results)
#    b) Other parts of the code still contain UNIX-isms
#
# for version 0.21 I have set PDL compilation to be off by default
# since although I can get it to work on my solaris box there are
# serious problems on linux and OS-X
#

eval { require 5.6.0 } or die <<EOD;
############
###
### The Inline::SLang module requires at least Perl version v5.6.0.
### Please upgrade your Perl before installing this module.
###
############
EOD

use strict;

use Config;
##use Data::Dumper;
use ExtUtils::MakeMaker;
use Getopt::Long;
use IO::File;
use File::Spec; # initial try to allow compilatio on non Unix-like filesystems

## Internal routines

my %synonyms;
my %sl_sizes;

# notes:
#  - fills in %synonyms and %sl_sizes arrays
#  - creates stf.h
#
sub parse_slconfig ($) {
    my $code = shift;

    my $fh = IO::File->new( "< $code" )
      or die "Error: unable to read from $code\n";
    while ( <$fh> ) {
	chomp;
	my ( $dummy, $synonym, $base ) = split;
	$synonyms{$synonym} = $base;
    }
    $fh->close;

    # Convert this to code used by _sl_defined_types in SLang.xs
    # - what a lovely mess I'm making
    #   (stf = slang-type-fragment but trying to be sensitive to
    #          filename-challenged filesystems even though other parts
    #          of this script are UNIX-specific)
    #
    my $cfrag = "stf.h";
    unlink $cfrag if -e $cfrag;
    die "Error: Unable to remove $cfrag\n" if -e $cfrag;

    $fh = IO::File->new( "> $cfrag" )
      or die "Error: Unable to create $cfrag\n";
    while ( my ( $synonym, $base ) = each %synonyms ) {
	unless ( exists $sl_sizes{$base} ) {
	    my $size = $synonym;
	    $size =~ s/[^\d]+//g;
	    ##print "size: synonym $synonym -> $size\n";
	    $sl_sizes{$base} = $size / 8;
	}

	my $len = length($synonym);
	$fh->print( <<"EOT");
  arrayref = newAV(); av_extend( arrayref, (I32) 2 );
  av_store( arrayref, 0, newSVpv("$base",0) );
  av_store( arrayref, 1, newSViv( 2 ) );
  (void) hv_store( hashref, "$synonym", $len, newRV_inc( (SV *) arrayref ), 0 );
EOT
    }
    $fh->close;

    if ( $synonyms{Long_Type} eq "Integer_Type" ) {
	$sl_sizes{Long_Type} = $sl_sizes{Integer_Type};
    }
} # sub: parse_slconfig

#
# Usage:
#   $include_flags = add_pdl_support();
#
# Aim:
#   Sets up things to alow Inline::SLang to be compiled with
#   support for PDL.
#
#   The return value is "" if PDL support is not available,
#   otherwise it is the include flags needed to add to the make file
#   to get C code to compile.
#
# Notes:
#   creates topdl.h and toslang.h - they need adding to
#   the clean target of the make file?
#
#   %sl_sizes must have been created before this routine is called,
#   which means parse_slconfig musy have been called
#
sub add_pdl_support () {

  # should I try and enforce a version of PDL?
  eval "use PDL;";

  # since --pdl is on by default I only make it a warning when
  # it cannot be found
  #
  if ( $@ ) {
    print "\nWARNING: PDL support has been disabled as not found on system\n\n";
    return "";
  }

  my $tmp = 1;
  my $pdlver = 0;
  map {
    $pdlver += ($_ * $tmp);
    $tmp *= 100;
  } reverse split( /\./, $PDL::VERSION );
  if ( $pdlver < 20400 ) {
    print <<"EOW";

WARNING:
  Found PDL but its version ($PDL::VERSION) is < 2.4.0 so things
  may not work, such as this build...

EOW
  }

  # warning messages
  print <<'EON';

PLEASE NOTE:
  Found PDL >= 2.4.0 but I should warn you that ...
  PDL support is currently experimental. As of 0.21 of Inline::SLang
  1D arrays/piddles convert okay on 32-bit machines (no testing done on
  64-bit machines). 2D - and higher - arrays/piddles will be converted
  *BUT* are unlikely to do what you expect. 0D piddles just croak.

  Please read the docs...

EON

  require PDL::Core::Dev;

  # work out the mapping from S-Lang datatype to PDL datatype
  #
  # S-Lang numeric types are:
  #   [U]Char_Type
  #   [U]Short_Type
  #   [U]Integer_Type
  #   [U]Long_type
  #   Float_Type
  #   Double_Type
  #
  # PDL types are:
  #   PDL_Byte -- this is unsigned
  #   PDL_Short
  #   PDL_UShort
  #   PDL_Long
  #   PDL_LongLong [maybe]
  #   PDL_Float
  #   PDL_Double
  #
  # So no unsigned char/long, so convert as signed type.
  # Assume that byte <-> char and have a size of 1.
  #
  # the following bit of code delves into the internals somewhat
  # - so is a bit dangerous - and requires a recent PDL (2.4.0)
  require PDL::Types;
  my %pdl_sizes;
  foreach my $ptype ( PDL::Types::typesrtkeys() ) {
    # going to assume unsigned types have the same size as signed types
    my $tmp = PDL::Types::typefld( $ptype, "realctype" );
    $tmp =~ s/^unsigned //;
    $tmp =~ s/\s+//g;
    $tmp .= "size";
    # as a ***HACK*** I am going to assume that a float is 4 bytes wide
    # - this needs re-working
    if ( $tmp eq "floatsize" ) {
      $pdl_sizes{$ptype} = 4;
    } else {
      $pdl_sizes{$ptype} = $Config{$tmp} || die "Error: unable to find size of '$tmp' from Config.pm\n";
    }
  }

  ##use Data::Dumper; print Dumper( \%sl_sizes ), "\n";

  # find out how the S-Lang types map onto PDL ones
  # - can you guess this was done without any thinking?
  #
  my %sl_typemaps;
  my %pl_typemaps;
  if ( $sl_sizes{Short_Type} == $pdl_sizes{PDL_S} ) {
    $sl_typemaps{SHORT} = [ "PDL_S", $pdl_sizes{PDL_S} ];
    $pl_typemaps{PDL_S} = [ "SHORT", $pdl_sizes{PDL_S} ];
  } else {
    print "Errr: need to sort out Short_Type\n";
  }
  if ( $sl_sizes{UShort_Type} == $pdl_sizes{PDL_US} ) {
    $sl_typemaps{USHORT} = [ "PDL_US", $pdl_sizes{PDL_US} ];
    $pl_typemaps{PDL_US} = [ "USHORT", $pdl_sizes{PDL_US} ];
  } else {
    print "Errr: need to sort out UShort_Type\n";
  }
  if ( $sl_sizes{Integer_Type} == $pdl_sizes{PDL_L} ) {
    $sl_typemaps{INT}   = [ "PDL_L", $pdl_sizes{PDL_L} ];
    $sl_typemaps{UINT}  = [ "PDL_L", $pdl_sizes{PDL_L} ];
    $pl_typemaps{PDL_L} = [ "INT", $pdl_sizes{PDL_L} ];
  } else {
    print "Errr: need to sort out Integer_Type\n";
  }
  if ( $sl_sizes{Long_Type} == $pdl_sizes{PDL_L} ) {
    $sl_typemaps{LONG}  = [ "PDL_L", $pdl_sizes{PDL_L} ];
    $sl_typemaps{ULONG} = [ "PDL_L", $pdl_sizes{PDL_L} ];
  } else {
    print "Errr: need to sort out Long_Type\n";
  }
  if ( $sl_sizes{Float_Type} == $pdl_sizes{PDL_F} ) {
    $sl_typemaps{FLOAT} = [ "PDL_F", $pdl_sizes{PDL_F} ];
    $pl_typemaps{PDL_F} = [ "FLOAT", $pdl_sizes{PDL_F} ];
  } else {
    print "Errr: need to sort out Float_Type\n";
  }
  if ( $sl_sizes{Double_Type} == $pdl_sizes{PDL_D} ) {
    $sl_typemaps{DOUBLE} = [ "PDL_D", $pdl_sizes{PDL_D} ];
    $pl_typemaps{PDL_D}  = [ "DOUBLE", $pdl_sizes{PDL_D} ];
  } else {
    print "Errr: need to sort out Double_Type\n";
  }

  # Convert this to code used by sl2pl_array_pdl in sl2pl.c
  # - what a lovely mess I'm making
  my $cfrag = "topdl.h";
  unlink $cfrag if -e $cfrag;
  die "Error: Unable to remove $cfrag\n" if -e $cfrag;

  my $fh = IO::File->new( "> $cfrag" )
    or die "Error: Unable to create $cfrag\n";
  $fh->print(
	     "if ( SLANG_CHAR_TYPE == at->data_type || SLANG_UCHAR_TYPE == at->data_type )\n" .
	     "  { out->datatype = PDL_B; dsize = 1; }\n"
	     );
  while ( my ( $stype, $aref ) = each %sl_typemaps ) {
    $fh->print(
	       "else if ( SLANG_${stype}_TYPE == at->data_type )\n" .
	       "  { out->datatype = $$aref[0]; dsize = $$aref[1]; }\n"
	       );
  }
  $fh->print( "else { croak(\"ERROR: Unable to convert an array of %s to a piddle\",\n" .
	      "SLclass_get_datatype_name(at->data_type)); }\n" );
  $fh->close;

  # ditto for pl2sl_type() in pl2sl.c
  $cfrag = "toslang.h";
  unlink $cfrag if -e $cfrag;
  die "Error: Unable to remove $cfrag\n" if -e $cfrag;

  $fh = IO::File->new( "> $cfrag" )
    or die "Error: Unable to create $cfrag\n";
  $fh->print(
	     "if ( PDL_B == pdl->datatype )\n" .
	     "  { otype = SLANG_UCHAR_TYPE; dsize = 1; }\n"
	     );
  while ( my ( $stype, $aref ) = each %pl_typemaps ) {
    $fh->print(
	       "else if ( $stype == pdl->datatype )\n" .
	       "  { otype = SLANG_" . $$aref[0] . "_TYPE; dsize = $$aref[1]; }\n"
	       );
  }
  $fh->print( "else { croak(\"ERROR: Unable to convert a piddle of type %d to S-Lang\",\n" .
	      "pdl->datatype); }\n" );
  $fh->close;

  return " " . &PDL::Core::Dev::PDL_INCLUDE;

} # sub: add_pdl_support()

## User options

my $gdb   = "";
my $debug = 0;
my $help  = 0;
my $slangdir = "";
my $slanglib = "";
my $slanginc = "";
my $pdl = 1;
$pdl = 0;  # dbg: whilst PDL support is flaky we turn it off by default
my $wall = 0;

## Search path for the S-Lang library

# what directories do we search in for S-Lang?
# (we force the CIAO installation to be first in the list to
#  make it less work if multiple matches are found)
#
my @guess_path =
  (
   [ File::Spec->rootdir(), "usr" ],
   [ File::Spec->rootdir(), "usr", "local" ]
  );

# provide support for CIAO 3.0 users
# (can assume a UNIX-style filesystem)
#
if ( defined $ENV{"ASCDS_INSTALL"} ) {

    # the location of the S-Lang library/include files has changed
    # between CIAO 2.3 and CIAO 3.0: as we don't support pre 3.0
    # versions of CIAO it is not too much of an issue
    #
    my @words = split / /, `cat $ENV{ASCDS_INSTALL}/VERSION`;
    die "Error: unable to read $ENV{ASCDS_INSTALL}/VERSION\n"
      if $#words == -1;

    # if pre v3.0 then we do not add it to the directory list
    my $major = (split /\./, $words[1])[0];
    if ( $major > 2 ) {

      # assume that the location hasn't changed from 3.0
      my @ciao_path = split( "/", $ENV{"ASCDS_INSTALL"} );

      # ugh - missing slang.h in include/
      unshift @guess_path,
        [ "/", @ciao_path[1..$#ciao_path], "ots", "slang" ];
      print "Found CIAO $words[1] installation at $ENV{\"ASCDS_INSTALL\"}.\n\n";

    }
} # if: defined $ASCDS_INSTALL

## Check the user-supplied options

GetOptions(
	   'gdb:s' => \$gdb,
	   'debug' => \$debug,
	   'help!' => \$help,
	   'slangdir:s' => \$slangdir,
	   'slanglib:s' => \$slanglib,
	   'slanginc:s' => \$slanginc,
	   'pdl!' => \$pdl,
	   'wall!' => \$wall,
	   ) or usage();

usage() if $help;

#============================================================================
# What S-Lang interpreter are we going to embed?
#============================================================================

# is this the correct thing to do?
my $ext_stat = $Config{_a};
my $ext_dyn  = "." . $Config{so};

my $libname_stat = "libslang$ext_stat";
my $libname_dyn  = "libslang$ext_dyn";
my $incname      = "slang.h";

# try looking through a set of directories
# - note we're assuming a UNIX filesystem
#
if ( $slangdir ) {
    # not absolutely necessary, but simplifies the logic a bit
    die "Error: -slangdir and -slanglib/inc are mutually exclusive options\n"
	if $slanglib ne "" or $slanginc ne "";

    $slanglib = File::Spec->catdir( $slangdir, "lib" );
    $slanginc = File::Spec->catdir( $slangdir, "include" );

} elsif ( !$slanglib && !$slanginc ) {

    # try and guess the location
    print "Guessing location of S-Lang:\n";
    my @matches;
    foreach my $path ( @guess_path ) {
	push @matches, $path if
	  -e File::Spec->catfile( @$path, "include", $incname ) &&
	    (-e File::Spec->catfile( @$path, "lib", $libname_stat ) ||
	     -e File::Spec->catfile( @$path, "lib", $libname_dyn));
    }
    die "Error: unable to find the S-Lang library/include files\n" .
	"       ($libname_stat/$ext_dyn and $incname)\n" .
	"       in any of the following directories:\n" .
	"       " . join(' ', map { File::Spec->catdir(@$_); } @guess_path ) . "\n"
	    if $#matches == -1;

    if ( $#matches > 0 ) {
	print "\nS-Lang was found in the following locations:\n";
	my $num = 1;
	print "\t$num - " . File::Spec->catdir(@$_)  . "\n" for @matches;
	print "\n";
	my $sel = prompt("Use which?", '1');
	$sel = $matches[$sel-1] if $sel =~ /^\d+$/;
	$slangdir = $sel;
    } else {
	$slangdir = $matches[0];
    }
    $slangdir = File::Spec->catdir( @$slangdir );
    print "  -> Using $slangdir as the location of S-Lang\n\n";
    $slanglib = File::Spec->catdir( $slangdir, "lib" );
    $slanginc = File::Spec->catdir( $slangdir, "include" );

} elsif ( !$slanginc || !$slanglib ) {
    die "Error: -slanginc and -slanglib must both be specified\n";
}

# can we find the necessary files?
#
die "Error: unable to find slang.h in the include directory ($slanginc)\n"
    unless -e File::Spec->catfile( $slanginc, "slang.h" );
die "Error: unable to find libslang[$ext_dyn|$ext_stat] in the library directory ($slanglib)\n"
  unless -e File::Spec->catfile( $slanglib, $libname_dyn ) ||
         -e File::Spec->catfile( $slanglib, $libname_stat );

my $incpath = "-I$slanginc";
my $libpath = "-L$slanglib -lslang";

# now check the S-Lang interpreter for how it was
# compiled (and some information about type synonyms)
# This information could be checked for at compile time
# - ie whenever a piece of code is first evaluated - but
# let's try and save a little time
# We also check for whether support for float & complex
# types are available - we currently die if they aren't,
# although the code could be updated to make such support
# optional (I don't have the time/interest)
#
# format taken from trylink subroutine in PDL/Core/Dev.pm
#
# Since we have Inline available this could have been done as
# an embedded bit of C code. I may move to this approach rather than
# try and roll-my-own compilation.
#
my $in  = "slconfig.c";
my $out = "slconfig";

my $cc = $Config{cc} ||
  die "Err: can not find name of C compiler used to create Perl!\n";
my $exe = $Config{_exe};
$out .= $exe;

unlink $out if -e $out;
die "Error: Unable to delete $out\n" if -e $out;

# doesn't seem to want to work with a list input (or rather I'm
# making a stupid mistake)
#
# we add '-bind_at_load' to stop warnings on OS-X
# (do we need to be more careful about the check for adding this?)
# '-ldl -lm' is added on solaris machines since some users
# seem to need it
#
my %os_opts =
  (
   "darwin" => " -bind_at_load",
   "solaris" => " -ldl -lm",  # when is this needed
);
system(
       "$cc -o $out $in $incpath $libpath" .
       ( exists $os_opts{$^O} ? $os_opts{$^O} : "" )
      ) and
  die "Error: Unable to compile a test program using\n" .
      "       $incpath $libpath\n";
die "Error: did not create $out from $in\n"
  unless -e $out;

my $code = "slconfig.dat";
unlink $code if -e $code;
die "Error: Unable to delete $code\n" if -e $code;

my $minver = "10407";
system( "./$out $minver" ); # this is ***UNIX SPECIFIC***
my $run = $? >> 8;
die "\nError: Unable to run '$out $minver' - error code = [$run]\n"
  if $run;
die "\nError: '$out $minver' did not create '$code'\n"
  unless -e $code;

parse_slconfig $code;

#============================================================================
# PDL support
#============================================================================

my $pdlinc = add_pdl_support if $pdl;
$pdl = 0 if $pdlinc eq "";

#============================================================================
# Finalize, and write the makefile
#============================================================================

my $defs = "-DI_SL_HAVE_PDL=$pdl";
$defs .= " -DI_SL_DEBUG" if $debug;

WriteMakefile(
    $defs         ? (DEFINE => $defs) : (),
    defined $gdb  ? (OPTIMIZE => debug_flag()) : (),
    $wall         ? (CCFLAGS => "-Wall") : (),

    INC			=> $incpath . $pdlinc,
    LIBS		=> $libpath,
    OBJECT 		=> 'SLang.o util.o sl2pl.o pl2sl.o' .
	      ($pdl ? ' pdl.o' : ''),
    NAME		=> 'Inline::SLang',
    VERSION_FROM	=> 'SLang.pm', # finds $VERSION
    PREREQ_PM		=> {
			    'Inline' => 0.42,
                            # not really needed but am too lazy to code around
			    'Test::More' => 0,
			   },
    realclean		=> {
			    # _Inline is a directory *NOT* a file,
			    # how can we tell 'realclean' this?
			    FILES => '_Inline ' . $out . ' slconfig.dat stf.h'
			    . ( $pdl ? ' topdl.h toslang.h' : '') },
    ABSTRACT_FROM       => 'SLang.pod',
    AUTHOR              => 'Doug Burke <djburke@cpan.org>',
);

sub debug_flag {
    return $gdb if $gdb;
    $Config{osname} eq 'MSWin32' ? return '-Zi' : return '-g';
}

sub usage {
    my $paths = join( '', map { "\t$_\n" } @guess_path );

    print <<"END";
Options:
  general:
    --help       this output

  location of the S-Lang library & include files:
    --slangdir=x  looks in x/lib/ and x/include/
   or
    --slanginc=x  location of the S-Lang include file
    --slanglib=x  location of the S-Lang library

   otherwise the following directories are searched:
$paths
  PDL support:
    --pdl     Turn on support for PDL if installed
    --nopdl   Turn off support for PDL (default)

  debugging:
    --gdb     Turn on compiler's debugging flag (use my guess).
    --gdb=x   Pass your own debugging flag, not mine.
    --debug   Turn on diagnostic print statements (a *lot* of
              screen output)

    --wall    Compile with "-Wall" (default is --nowall)

END

    exit 0;
}

# end of Makefile.PL