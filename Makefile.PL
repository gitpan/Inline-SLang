#
# Setup Inline::SLang
# - code taken from Makefile.PL in Inline-Ruby-0.02
#   (Neil Watkiss), although all errors should be assumed to
#   be mine and not Neil's
#

eval { require 5.6.0 } or die <<EOD;
############
###
### The Inline::SLang module requires version v5.6.0 or later of perl
### Please upgrade your Perl before installing this module.
###
############
EOD

# just in case...
print "\nPlease note - this is an alpha release.\n\n";

use strict;

use Config;
##use Data::Dumper;
use ExtUtils::MakeMaker;
use Getopt::Long;
use IO::File;

## User options

my $gdb   = "";
my $debug = 0;
my $help  = 0;
my $slangdir = "";
my $slanglib = "";
my $slanginc = "";
my $pdl = 1;

## Search path for the S-Lang library

# what directories do we search in for S-Lang?
# (we force the CIAO installation to be first in the list to
#  make it less work if multiple matches are found)
#
my @guess_path = qw( /usr /usr/local );

# provide support for CIAO 3.0 users
if ( defined $ENV{"ASCDS_INSTALL"} ) {

    # the location of the S-Lang library/include files has changed
    # between CIAO 2.3 and the developmental CIAO 3.0 version
    #
    my @words = split / /, `cat $ENV{ASCDS_INSTALL}/VERSION`;
    die "Error: unable to read $ENV{ASCDS_INSTALL}/VERSION\n"
      if $#words == -1;

    # if v 2.X then we don't add it to the directory list
    if ( $words[1] !~ /^2\./ ) {

      # assume v >= 3.0 (and that the location hasn't changed from 3.0)
      unshift @guess_path, $ENV{"ASCDS_INSTALL"} . "/ots/slang"; # ugh - missing slang.h in include/
      print "Found CIAO $words[1] installation.\n\n";

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
	   ) or usage();

usage() if $help;

#============================================================================
# What S-Lang interpreter are we going to embed?
#============================================================================

# is this the correct thing to do?
my $ext_stat = $Config{_a};
my $ext_dyn  = ".$Config{so}";

my $libname_stat = "lib/libslang$ext_stat";
my $libname_dyn  = "lib/libslang$ext_dyn";
my $incname      = "include/slang.h";

# try looking through a set of directories
# - note we're assuming a UNIX filesystem
#
if ( $slangdir ) {
    # not absolutely necessary, but simplifies the logic a bit
    die "Error: -slangdir and -slanglib/inc are mutually exclusive options\n"
	if $slanglib ne "" or $slanginc ne "";

    $slanglib = "$slangdir/lib";
    $slanginc = "$slangdir/include";

} elsif ( !$slanglib && !$slanginc ) {

    # try and guess the location
    print "Guessing location of S-Lang:\n";
    my @matches;
    foreach my $path ( @guess_path ) {
	push @matches, $path if
	  -e "$path/$incname" &&
	    (-e "$path/$libname_stat" || -e "$path/$libname_dyn");
    }
    die "Error: unable to find the S-Lang library/include files\n" .
	"       ($libname_stat/$ext_dyn and $incname)\n" .
	"       in any of the following directories:\n" .
	"       " . join(' ', @guess_path ) . "\n"
	    if $#matches == -1;

    if ( $#matches > 0 ) {
	print "\nS-Lang was found in the following locations:\n";
	my $num = 1;
	print "\t$num - $_\n" for @matches;
	print "\n";
	my $sel = prompt("Use which?", '1');
	$sel = $matches[$sel-1] if $sel =~ /^\d+$/;
	$slangdir = $sel;
    } else {
	$slangdir = $matches[0];
    }
    print "Using $slangdir as the location of S-Lang\n\n";
    $slanglib = "$slangdir/lib";
    $slanginc = "$slangdir/include";

} elsif ( !$slanginc || !$slanglib ) {
    die "Error: -slanginc and -slanglib must both be specified\n";
}

# can we find the necessary files?
die "Error: unable to find slang.h in the include directory ($slanginc)\n"
    unless -e "$slanginc/slang.h";
die "Error: unable to find libslang[$ext_dyn|$ext_stat] in the library directory ($slanglib)\n"
    unless -e "$slanglib/libslang$ext_dyn" || -e "$slanglib/libslang$ext_stat";

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
system( "$cc -o $out $incpath $libpath $in" ) and
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

my %synonyms;
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
my %sl_sizes;
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

#============================================================================
# PDL support
#============================================================================

my $pdlinc = "";

if ( $pdl ) {
  # should I try and enforce a version of PDL?
  eval "use PDL;";

  # since --pdl is on by default I only make it a warning when
  # it cannot be found
  #
  if ( $@ ) {
    print "\nWARNING: PDL support has been disabled as not found on system\n\n";
    $pdl = 0;
    last;
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
  PDL support is currently experimental. As of 0.20 of Inline::SLang
  1D arrays/piddles convert okay on 32-bit machines (no testing done on
  64-bit machines). 2D - and higher - arrays/piddles will be converted
  *BUT* are unlikely to do what you expect. 0D piddles just croak.

  Please read the docs...

EON

  require PDL::Core::Dev;
  $pdlinc = " " . &PDL::Core::Dev::PDL_INCLUDE;

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
  # - so is a bit dangerous - and requires a recent PDL (2.4.0?)
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

  # Convert this to code used by sl2pl_array_pdl in util.c
  # - what a lovely mess I'm making
  my $cfrag = "topdl.h";
  unlink $cfrag if -e $cfrag;
  die "Error: Unable to remove $cfrag\n" if -e $cfrag;

  $fh = IO::File->new( "> $cfrag" )
    or die "Error: Unable to create $cfrag\n";
  $fh->print(
	     "if ( at->data_type == SLANG_CHAR_TYPE || at->data_type == SLANG_UCHAR_TYPE )\n" .
	     "  { out->datatype = PDL_B; dsize = 1; }\n"
	     );
  while ( my ( $stype, $aref ) = each %sl_typemaps ) {
    $fh->print(
	       "else if ( at->data_type == SLANG_${stype}_TYPE )\n" .
	       "  { out->datatype = $$aref[0]; dsize = $$aref[1]; }\n"
	       );
  }
  $fh->print( "else { croak(\"ERROR: Unable to convert an array of %s to a piddle\",\n" .
	      "SLclass_get_datatype_name(at->data_type)); }\n" );
  $fh->close;

  # ditto for pl2sl_type() in util.c
  $cfrag = "toslang.h";
  unlink $cfrag if -e $cfrag;
  die "Error: Unable to remove $cfrag\n" if -e $cfrag;

  $fh = IO::File->new( "> $cfrag" )
    or die "Error: Unable to create $cfrag\n";
  $fh->print(
	     "if ( pdl->datatype == PDL_B )\n" .
	     "  { otype = SLANG_UCHAR_TYPE; dsize = 1; }\n"
	     );
  while ( my ( $stype, $aref ) = each %pl_typemaps ) {
    $fh->print(
	       "else if ( pdl->datatype == $stype )\n" .
	       "  { otype = SLANG_" . $$aref[0] . "_TYPE; dsize = $$aref[1]; }\n"
	       );
  }
  $fh->print( "else { croak(\"ERROR: Unable to convert a piddle of type %d to S-Lang\",\n" .
	      "pdl->datatype); }\n" );
  $fh->close;

} # if: $pdl

#============================================================================
# Finalize, and write the makefile
#============================================================================

my $defs = "-DI_SL_HAVE_PDL=$pdl";
$defs .= " -DI_SL_DEBUG" if $debug;

WriteMakefile(
    $defs ? (DEFINE => $defs) : (),
    defined $gdb ? (OPTIMIZE => debug_flag()) : (),
    INC			=> $incpath . $pdlinc,
    LIBS		=> $libpath,
    OBJECT 		=> 'SLang.o util.o',
    NAME		=> 'Inline::SLang',
    VERSION_FROM	=> 'SLang.pm', # finds $VERSION
    PREREQ_PM		=> {
                            # not really needed but am too lazy to code around
			    'Test::More' => 0,
			    'Inline' => 0.42,
			   },
    realclean		=> { FILES => '_Inline' },
    realclean		=> { FILES => '_Inline ' . $out . ' slconfig.dat stf.h topdl.h toslang.h' },
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
    --pdl     Turn on support for PDL if installed (default)
    --nopdl   Turn off support for PDL

  debugging:
    --gdb     Turn on compiler's debugging flag (use my guess).
    --gdb=x   Pass your own debugging flag, not mine.
    --debug   Turn on diagnostic print statements (a *lot* of
              screen output)

END

    exit 0;
}

# end of Makefile.PL
