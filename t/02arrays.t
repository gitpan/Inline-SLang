#
# test in/out of arrays
# - also see 02arrays2[perl|slang].t for tests of
#   the individual datatypes
#
# many of these tests shouldn't be direct equality
# since it's floating point
#
# to do:
# - test > 1D arrays (not yet supported)
# - are we going to be able to test intrinsic functions like array_map() ?
#

use strict;

use Test::More tests => 23;

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

## 

## play around with some types
#
# should perhaps really be in 02arrays2perl.t
#

$ret1 = array_ui(2,5);
ok( eq_array( $ret1, [2,5] ), 'UInteger_Type converted to perl arrays' );
$ret1 = array_chars();
print "UChar_Type [97..99] == ", Dumper($ret1), "\n";
ok( eq_array( $ret1, [97..99] ), 'UChar_Type converted to perl arrays' );
$ret1 = array_longs();
ok( eq_array( $ret1, [1000000000,21000000,-4] ), 'Long_Type converted to perl arrays' );

## Associative arrays

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

TODO: {
    todo_skip "need to handle SLANG_ARRAY_TYPE arrays???", 5;

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
    todo_skip "need to handle Any_Type variables", 5;

    $ret1 = assocarray_any();
    print "Assoc array:\n" . Dumper($ret1), "\n";
    is( ref($ret1), "HASH", "Assoc_Array [Any_Type] converted to hash array ref" );
    ok( eq_array( [sort keys %$ret1], [ "1", "a", "b b" ] ),
	"   keys for assoc array are okay" );

    is( $$ret1{"a"},   "aa", '  key   a == "aa"' );
    is( $$ret1{"b b"},  1.2, '  key b b == 1.2' );
    ok( eq_array( $$ret1{"1"}, [1,2,3,4] ),
	'  key   1 == [1,2,3,4]' );

}

#TODO: {
#	todo_skip "need to convert S-Lang assoc values to perl types", 3;
#	
#}

## Need to test the other types (not yet supported)

__END__

__SLang__

% force the data types into "uncommon" ones
define array_ui () {
  variable array = UInteger_Type [_NARGS];
  variable i;
  for ( i=_NARGS-1; i>=0; i-- ) { % note: reverse order
    variable var = ();
    array[i] = var;
  }
  return array;
}

define array_chars () { return ['a','b','c']; }
define array_longs () { return typecast([1e9,2.1e7,-4],Long_Type); }

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

define assocarray_any () {
  variable foo = Assoc_Type [];
  foo["a"]   = "aa";
  foo["b b"] = 1.2;
  foo["1"]   = [1:4];
  return foo;
}


