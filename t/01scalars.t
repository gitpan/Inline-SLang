#
# test scalars
# . from Perl to S-Lang and vice-versa
# . this is mainly testing the stack-handling since the 
#   tests of the individual datatypes are done in
#   01scalars2perl.t and 01scalars2slang.t
#

use strict;

use Test::More tests => 11;

use Data::Dumper;

# check for approximately equal
# - for these operations an absolute tolerance is okay
#
use constant ABSTOL => 1.0e-10;
sub approx ($$$) {
    my ( $a, $b, $text ) = @_;
    my $delta = $a-$b;
    ok( abs($delta) < ABSTOL, "$text [delta=$delta]" );
}

## Tests

use Inline 'SLang';

my ( $ret1, $ret2, @ret );

## variable args

$ret1 = nvarargs();
is( $ret1, 0, "varargs: 0 supplied" );

$ret1 = nvarargs( "a", 2, 3.0 );
is( $ret1, 3, "varargs: 3 supplied" );

@ret = nvarargs( -5, -6 );
is( $#ret, 0, "varargs: 2 supplied" );
is( $ret[0], 2, "varargs: 2 supplied" );

$ret1 = sumup( 2, 3.4 );
approx( $ret1, 5.4, "varargs: sumup=5.4" );

$ret1 = sumup( 2, 3.4, 90 );
approx( $ret1, 95.4, "varargs: sumup=95.4" );

$ret1 = sumup( 2, 3.4, -100, 90 );
approx( $ret1, -4.6, "varargs: sumup=-4.6" );

$ret1 = concatall( "a", " ", "b" );
is( $ret1, "a b", "a + ' ' + b = 'a b'" );

$ret1 = concatall( " ", "2 " );
is( $ret1, " 2 ", "' ' + '2 ' = ' 2 '" );

## Null values

$ret1 = sendnull(undef);
is( $ret1, 1, 'undef (perl) converted to NULL (S-Lang)' );

$ret1 = sendnull('foo');
is( $ret1, 0, '"foo" != NULL' );

__END__

__SLang__

%% check the stack (variable args)

% if we don't clear the stack via _pop_n() we really mess up
define nvarargs () {
  variable n = _NARGS;
  () = printf( "varargs was sent %d arguments\n", n );
%%  _print_stack();
  _pop_n(n);
  return n;
}

define sumup () {
  variable sum = 0.0;
  foreach ( __pop_args(_NARGS) ) {
    variable arg = ();
    sum += arg.value;
  }
  return sum;
}

define concatall () {
  variable str = "";
  foreach ( __pop_args(_NARGS) ) {
    variable arg = ();
    str += arg.value;
  }
  return str;
}

% NULL value
define sendnull(x) { return x==NULL; }
