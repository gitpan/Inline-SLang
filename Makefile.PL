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

use Data::Dumper;
use Config;
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
$pdl = 0; # hack for 0.11 since we don't yet support PDL

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

# now check for version in slang.h
#
# could also check for things like max num of array dims
# and whether support for floating-point numbers
#
# an alternative is to use a configure-style approach and compile
# a small C program...
#
my $fh = IO::File->new( "< ${slanginc}/slang.h" )
  or die "Error: unable to read from ${slanginc}/slang.h\n";
while ( <$fh> ) {
  chomp;
  s/^\s+//g;
  next unless /^#define\s+SLANG_VERSION\s/;
  my ( $dummy, $name, $version, @rest ) = split;
  die "Error: S-Lang version ($version) must be >= 10407 for Inline::SLang\n"
    unless $version >= 10407;
  print "Compiling against S-Lang version $version\n";
  last;
}
$fh->close;

my $incpath = "-I$slanginc";
my $libpath = "-L$slanglib -lslang";

#============================================================================
# PDL support
#============================================================================

# HACK for 0.11
if ( $pdl ) { print "\nNOTE: 0.11 does not support the --pdl option. Turning off\n\n"; $pdl = 0; }

if ( $pdl ) {
  # should I try and enforce a version of PDL?
  eval { use PDL; };

  # since --pdl is on by default I only make it a warning when
  # it cannot be found
  #
  if ( $@ ) {
    print "\nWARNING: PDL support has been disabled as not found on system\n\n";
    $pdl = 0;
  }
}

#============================================================================
# Finalize, and write the makefile
#============================================================================

my $defs = "-DI_SL_HAVE_PDL=$pdl";
$defs .= " -DI_SL_DEBUG" if $debug;

WriteMakefile(
    $defs ? (DEFINE => $defs) : (),
    defined $gdb ? (OPTIMIZE => debug_flag()) : (),
    INC			=> $incpath,
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
    --pdl     Turn on support for PDL, if present (default)
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
