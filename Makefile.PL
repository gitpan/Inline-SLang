#
# Setup Inline::SLang
# - code taken from Makefile.PL in Inline-Ruby-0.02
#   (Neil Watkiss), although all errors should be assumed to
#   be mine and not Neil's
#
# The module is named SLang rather than S-Lang since
# the hyphen confuses ExtUtils. The L remains capitalised
# to point out it's S-Lang and not 'slang'.
#

use strict;

print "\nWARNING: this is pre-alpha code. Absolutely no guaranties about" .
    " anything!\n\n";

eval { require 5.6.0 } or die <<EOD;
############
###
### The Inline::SLang module requires version v5.6.0 or later of perl
### Please upgrade your Perl before installing this module.
###
### (note that currently this requirement is just to save myself some
###  work and could probably be relaxed, although once PDL support
###  is included it will be a necessary requirement)
###
############
EOD

use Data::Dumper;
use Config;
use ExtUtils::MakeMaker;
use Getopt::Long;

## User options

my $gdb   = "";
my $debug = 0;
my $help  = 0;
my $slangdir = "";
my $slanglib = "";
my $slanginc = "";

## Search path for the S-Lang library

# what directories do we search in for S-Lang?
# (we force the CIAO installation to be first in the list to
#  make it less work if multiple matches are found)
#
my @guess_path = qw( /usr /usr/local );

if ( defined $ENV{"ASCDS_INSTALL"} ) {

    # the location of the S-Lang library/include files has changed
    # between CIAO 2.3 and the developmental CIAO 3.0 version
    #
    my @words = split / /, `cat $ENV{ASCDS_INSTALL}/VERSION`;
    die "Error: unable to read $ENV{ASCDS_INSTALL}/VERSION\n"
      if $#words == -1;

    if ( $words[1] eq "2.2.1" or $words[1] eq "2.3" ) {
	# note: only available if the OTS package was installed
	my $path = $ENV{"ASCDS_INSTALL"} . "/ots/slang.v1.4.4";
	if ( -d $path ) {
	    unshift @guess_path, $path;
	    print "Found CIAO $words[1] installation with OTS.\n\n";
	}
    } else {
	# assume v >= 3.0 (and that the location hasn't changed from 3.0)
##	unshift @guess_path, $ENV{"ASCDS_INSTALL"};
##	unshift @guess_path, $ENV{"ASCDS_INSTALL"} . "/ots/slang.v1.4.5"; # ugh - missing slang.h in include/
	unshift @guess_path, $ENV{"ASCDS_INSTALL"} . "/ots/slang"; # ugh - missing slang.h in include/
	print "Found CIAO $words[1] installation.\n\n";

## hack
#	$slanglib = $ENV{"ASCDS_INSTALL"} . "/lib";
#	$slanginc = $ENV{"ASCDS_INSTALL"} . "/ots/slang.v1.4.5/include";
    }
} # if: defined $ASCDS_INSTALL

#### TEST
#$slangdir = "/home/burke/local/";
#$debug = 1;

## Check the user-supplied options

GetOptions(
	   'gdb:s' => \$gdb,
	   'debug' => \$debug,
	   'help' => \$help,
	   'slangdir:s' => \$slangdir,
	   'slanglib:s' => \$slanglib,
	   'slanginc:s' => \$slanginc,
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
# - note we include $ASCDS_INSTALL if it's defined
#   and we're assuming a UNIX filesystem
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

#============================================================================
# Finalize, and write the makefile
#============================================================================

my $defs = "";
$defs .= "-DI_SL_DEBUG" if $debug;

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
			   }, # e.g., Module::Name => 1.1
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
  debugging:
    --gdb     Turn on compiler's debugging flag (use my guess).
    --gdb=x   Pass your own debugging flag, not mine.
    --debug   Turn on diagnostic print statements (a lot of
              screen output)

END
# ' stupid vim
    exit 0;
}
