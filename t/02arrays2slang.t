#
# test conversion of Perl arrays to S-Lang
#
# many of these tests shouldn't be direct equality
# since it's floating point
#
# to do:
# - test > 1D arrays (not yet supported)
# - are we going to be able to test intrinsic functions like array_map() ?
#

use strict;

use Test::More tests => 6;

use Inline 'SLang';

use Data::Dumper;

# check for approximately equal
# - for these operations an absolute tolerance is okay
#
# really want to be able to test arrays easily
#
use constant ABSTOL => 1.0e-10;
sub approx ($$$) {
    my ( $a, $b, $text ) = @_;
    my $delta = $a-$b;
    ok( abs($delta) < ABSTOL, "$text [delta=$delta]" );
}

## Tests

my ( $ret1, $ret2, @ret );

TODO: {
	todo_skip "need to improve perl -> S-Lang array conversions", 6;

	$ret1 = add2( [ 3, 5 ] );
	is( ref($ret1), "ARRAY", 'perl 2 S-Lang: returned an array' );
	is( $#$ret1,    2,       '               of the right size' );
	is( $$ret1[0],  5,       '               can add 2 [0]' );
	is( $$ret1[1],  7,       '               can add 2 [1]' );

	$ret1 = sumup_nelems( [0], ["a", "b"], [3.4, 5.6, 9.4, 55] );
	is( $ret1, 7, "able to add up 1D elements" );

	$ret1 = sumup_nelems( [0], [ ["a", "b"], ["cc", "d"] ], [3.4, 5.6, 9.4, 55] );
	is( $ret1, 9, "able to add up 1D + 2D elements" );
}

__END__

__SLang__

%% Convert perl to S-Lang

define add2 (a) { return a+2; }

define sum_nelems () {
  variable sum = 0;
  foreach ( __pop_args(_NARGS) ) {
    variable arg = ();
    sum += length(arg.value);
  }
  return sum;
}

