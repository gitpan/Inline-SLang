# -*-perl-*-
#
# test in/out of associative arrays
#

use strict;

use Test::More tests => 26;

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

## S-Lang 2 perl

$ret1 = assocarray_uchar();
print "Assoc array:\n" . Dumper($ret1), "\n";
is( ref($ret1), "HASH", "Assoc_Array [Uchar_Type] converted to hash array ref" );
ok( eq_array( [sort keys %$ret1], [ "1", "a", "b b" ] ),
     "   keys for assoc array are okay" );
is( $$ret1{"a"},     1, "  key   a == 1" );
is( $$ret1{"b b"}, 120, "  key b b == 120" );
is( $$ret1{"1"},   255, "  key   1 == 255" );

$ret1 = assocarray_string();
print "Assoc array:\n" . Dumper($ret1), "\n";
is( ref($ret1), "HASH", "Assoc_Array [String_Type] converted to hash array ref" );
ok( eq_array( [sort keys %$ret1], [ "1", "a", "b b" ] ),
     "   keys for assoc array are okay" );
is( $$ret1{"a"},      "aa", '  key   a == "aa"' );
is( $$ret1{"b b"},   "1.2", '  key b b == "1.2"' );
is( $$ret1{"1"},   "[1:4]", '  key   1 == "[1:4]"' );

# check we don't mess up the stack
my $ret3;
( $ret1, $ret2, $ret3 ) = ret_multi();
is( $ret1, "a string",  'Returned: 1st elem = string' );
is( ref($ret2), "HASH", 'Returned: 2nd elem = assoc array' );
is( $$ret2{"a b q"}, 23, '  and "a b q" = 23' );
is( $$ret2{"1"},     -4, '  and "1"     = -4' );
is( $ret3, 22.4,        'Returned: 3rd elem = real' );

TODO: {
    todo_skip "need to handle SLANG_ARRAY_TYPE arrays", 5;

    $ret1 = assocarray_array();
    print "Assoc array:\n" . Dumper($ret1), "\n";
    is( ref($ret1), "HASH", "Assoc_Array [Array_Type] converted to hash array ref" );
    ok( eq_array( [sort keys %$ret1], [ "1", "a", "b b" ] ),
	"   keys for assoc array are okay" );

    ok( eq_array( $$ret1{"a"},   [0,1,2,3] ),
	'  key   a == [0,1,2,3]' );
    ok( eq_array( $$ret1{"b b"}, [1,2,3,4] ),
	'  key b b == [1,2,3,4]' );
    ok( eq_array( $$ret1{"1"},   [0.5,1.0,1.5,2.0] ),
	'  key   1 == [1,2,3,4]/2' );

}

TODO: {
  todo_skip "Unable to handle arrays of Any_Type", 5;

  $ret1 = assocarray_any1();
  print "Assoc array:\n" . Dumper($ret1), "\n";
  is( ref($ret1), "HASH", "Assoc_Array [] converted to hash array ref" );
  ok( eq_array( [sort keys %$ret1], [ "1", "a", "b b" ] ),
      "   keys for assoc array are okay" );

  is( $$ret1{"a"},   "aa", '  key   a == "aa"' );
  is( $$ret1{"b b"},  1.2, '  key b b == 1.2' );

  ok( eq_array( $$ret1{"1"}, [1,2,3,4] ),
      '  key   1 == [1,2,3,4]' );
}

## perl 2 S-Lang

TODO: {
    todo_skip "Unable to convert assoc arrays to perl", 1;

ok( input_assoc( { aa => 'a a', 23 => 2, "a string" => 2.3 } ),
    "Can convert an assoc array refrence to S-Lang" );

}

__END__

__SLang__

%% S-Lang 2 perl: associative arrays

define assocarray_uchar () {
  variable foo = Assoc_Type [UChar_Type];
  foo["a"]   = 1;
  foo["b b"] = 'x';
  foo["1"]   = 255;
  return foo;
}

define assocarray_string () {
  variable foo = Assoc_Type [String_Type];
  foo["a"]   = "aa";
  foo["b b"] = "1.2";
  foo["1"]   = "[1:4]";
  return foo;
}

define assocarray_array () {
  variable foo = Assoc_Type [Array_Type];
  foo["a"]   = [0:3];
  foo["b b"] = foo["a"] + 1; % want to try a 2D array
  foo["1"]   = foo["b b"] / 2.0;
  return foo;
}

define assocarray_any1 () {
  variable foo = Assoc_Type [];
  foo["a"]   = "aa";
  foo["b b"] = 1.2;
% do not handle array of Any_Type variables at the moment
%  foo["1"]   = [1:4];
  foo["1"] = NULL;
  return foo;
}

define assocarray_any2 () {
  variable foo = Assoc_Type [Any_Type];
  foo["a"]   = "aa";
  foo["b b"] = 1.2;
  foo["1"]   = [1:4];
  return foo;
}

define ret_multi() {
  variable foo = Assoc_Type [Integer_Type];
  foo["a b q"] = 23;
  foo["1"]     = -4;
  return "a string", foo, 22.4;
}

define input_assoc(x) { return typeof(x) == Assoc_Type; }

