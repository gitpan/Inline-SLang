# -*-perl-*-
#
# test conversion of piddles to S-Lang
#
# many of these tests shouldn't be direct equality
# since it's floating point
#
# testing > 1D arrays is a pain at the moment since I haven't decided
# how to handle the fact that in S-Lang it's 2x3 but to PDL it's 3x2
#

use strict;

# we implicitly test support for !types here
use Inline 'SLang' => Config => EXPORT => [ qw( sl_array !types ) ];
use Inline 'SLang';

use constant NTESTS => 19;
use Test::More tests => NTESTS;

SKIP: {
    skip 'PDL support is not available', NTESTS
      unless Inline::SLang::sl_have_pdl();

    eval "use PDL;";

    ##use Data::Dumper;

    ## Tests

    my ( $pdl, $ret1, $ret2, $ret3, $ret4, @ret );

    # Can we send simple 1D arrays to S-Lang?
    my $val = [20];
    my $dim = [1];
    ok( isa_array(byte($val),$dim,UChar_Type()),    "Can convert byte to a 1D S-Lang array" );
    ok( isa_array(short($val),$dim,Short_Type()),   "Can convert short to a 1D S-Lang array" );
    ok( isa_array(ushort($val),$dim,UShort_Type()), "Can convert ushort to a 1D S-Lang array" );
    ok( isa_array(long($val),$dim,Int_Type()),      "Can convert long to a 1D S-Lang array" );
    ok( isa_array(float($val),$dim,Float_Type()),   "Can convert float to a 1D S-Lang array" );
    ok( isa_array(double($val),$dim,Double_Type()), "Can convert double to 1D a S-Lang array" );

    # Can we send simple 6D arrays to S-Lang?
    # - can't do 7D since there's a problem with 7D arrays in v1.4.9
    $val = [[[[[[20]]]]]];
    $dim = [1,1,1,1,1,1];
    ok( isa_array(byte($val),$dim,UChar_Type()),    "Can convert byte to a 6D S-Lang array" );
    ok( isa_array(short($val),$dim,Short_Type()),   "Can convert short to a 6D S-Lang array" );
    ok( isa_array(ushort($val),$dim,UShort_Type()), "Can convert ushort to a 6D S-Lang array" );
    ok( isa_array(long($val),$dim,Int_Type()),      "Can convert long to a 6D S-Lang array" );
    ok( isa_array(float($val),$dim,Float_Type()),   "Can convert float to a 6D S-Lang array" );
    ok( isa_array(double($val),$dim,Double_Type()), "Can convert double to a 6D S-Lang array" );

    # guess we should check the values get converted correctly
    $val = byte(1,2,3,128,255,256);
    ok( check_byte1d($val), "1D byte vals okay" );
    $val = short(-2,-1,0,1,2);
    ok( check_short1d($val), "1D short vals okay" );
    # too lazy to find out what the max value of a short is
    $val = ushort(2,1,0,1,2);
    ok( check_ushort1d($val), "1D ushort vals okay" );
    $val = long(-3,-16,0,1,2);
    ok( check_long1d($val), "1D long vals okay" );
    $val = (sequence(float(),10)-5)/2.0;
    ok( check_float1d($val), "1D float vals okay" );
    $val = (sequence(float(),10)-5)/2.0;
    ok( check_float1d($val), "1D float vals okay" );
    $val = (sequence(10)-5)/2.0;
    ok( check_dble1d($val), "1D double vals okay" );

=begin nDARRAYSAREPAINFUL

    $val = cat( byte(1,2,3), byte(128,255,256) );
    ok( check_byte2d($val), "2D byte vals okay" );
    $val = ones(short(),4,2)->xvals;
    ok( check_short2d($val), "2D short vals okay" );
    $val = sequence(2,4);
    ok( check_dble2d($val), "2D double vals okay" );

=end nDARRAYSAREPAINFUL

=cut

} # SKIP

=begin OLDCODE

# can we send this to S-Lang?
ok( isa_array($ret1), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret1,Integer_Type(),1,3), 1,
    "  and it seems to have the correct info" );
ok( check_array1d($ret1), "  and the right values" );

# note: $ret2 actually points to the storage area used by the obect,
#  so if you change it the object changes but without knowing about it
#  - so changing the array size is BAD
#
$ret2 = $ret1->toPerl();
is( ref($ret2), "ARRAY", "toPerl() returned an array reference" );
ok( eq_array( $ret2, [1,23,-10] ), "  with the correct values" );
$$ret2[1] = 4;
ok( $ret1->get(0) == 1 && $ret1->get(1) == 4 && $ret1->get(2) == -10,
    "  and can change the data stored in the object [scary]" ); 

# check persistence of $foo->toPerl return value after object is deleted
undef $ret1;
#print "Does array reference exist beyond object destruction: ", 
#   Dumper($ret2), "\n";
ok( ref($ret2) eq "ARRAY" && eq_array( $ret2, [1,4,-10] ),
    "  and it persists past the destruction of the object" );

## Now a 2D array

$ret1 = Array_Type->new( "Float_Type", [2,3] );
isa_ok( $ret1, "Array_Type" );

( $ret2, $ret3, $ret4 ) = $ret1->array_info();
is( $ret3, 2, "2D array is 2D" );
ok( eq_array( $ret2, [2,3] ), "  and is 2x3" );
is( "$ret4", "Float_Type", "  and the type is Float_Type" );

$ret1->set(0,0,0); $ret1->set(1,0,10);
$ret1->set(0,1,1); $ret1->set(1,1,11);
$ret1->set(0,2,2); $ret1->set(1,2,12);
ok( $ret1->get(0,2) == 2 && $ret1->get(1,2) == 12, "Some simple sets/gets work" );

# check get/set when sent invalid arguments
eval { $ret1->get(0,5); };
like( $@, qr/^Error: coord #1 of get\(\) call \(val=5\) lies outside valid range of -3:2/,
	"Can not access invalid element (>=nelem)" );
eval { $ret1->get(-4,1); };
like( $@, qr/^Error: coord #0 of get\(\) call \(val=-4\) lies outside valid range of -2:1/,
	"Can not access invalid element (<-nelem)" );
eval { $ret1->get(5); };
like( $@, qr/^Error: get\(\) called with 1 coordinates but array dimensionality is 2/,
	"Can not access invalid element (1-dimensional coordinate)" );

# can we send this to S-Lang?
ok( isa_array($ret1), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret1,Float_Type(),2,2,3), 1,
    "  [2D] and it seems to have the correct info" );
ok( check_array2d($ret1), "  [2D] and the right values" );

## Now a 0D array

$ret1 = Array_Type->new( "String_Type", [] );
isa_ok( $ret1, "Array_Type" );

( $ret2, $ret3, $ret4 ) = $ret1->array_info();
is( $ret3, 0, "0D array is 0D" );
is( $#$ret2, -1, "  and has no size" );
is( "$ret4", "String_Type", "  and the type is String_Type" );

# check get/set when sent invalid arguments
eval { $ret1->get(0); };
like( $@, qr/^Error: get\(\) called with 1 coordinates but array dimensionality is 0/,
	"Can not access any element (since there isn't one)" );

# can we send this to S-Lang?
ok( isa_array($ret1), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret1,String_Type(),0), 1,
    "  [OD] and it seems to have the correct info" );

## Now a 3D array

$ret1 = Array_Type->new( "Short_Type", [1,3,2] );
isa_ok( $ret1, "Array_Type" );
( $ret2, $ret3, $ret4 ) = $ret1->array_info();
is( $ret3, 3, "3D array is 3D" );
ok( eq_array( $ret2, [1,3,2] ), "  and is 1x3x2" );
is( "$ret4", "Short_Type", "  and the type is Short_Type" );

$ret1->set(0,0,0,1); $ret1->set(0,0,1,2);
$ret1->set(0,1,0,10); $ret1->set(0,1,1,11);
$ret1->set(0,2,0,20); $ret1->set(0,2,1,21);
ok( $ret1->get(0,1,0) == 10 && $ret1->get(0,2,1) == 21, "Some simple sets/gets work" );

# can we send this to S-Lang?
ok( isa_array($ret1), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret1,Short_Type(),3,1,3,2), 1,
    "  [3D] and it seems to have the correct info" );
ok( check_array3d($ret1), "  [3D] and the right values" );

## 4D array - 1 elem along each dimension

$ret1 = Array_Type->new( "Double_Type", [1,1,1,1] );
isa_ok( $ret1, "Array_Type" );
( $ret2, $ret3, $ret4 ) = $ret1->array_info();
is( $ret3, 4, "4D array is 4D" );
ok( eq_array( $ret2, [1,1,1,1] ), "  and is 1x1x1x1" );
is( "$ret4", "Double_Type", "  and the type is Double_Type" );

$ret1->set(0,0,0,0,-23.2);
ok( $ret1->get(0,0,0,0) == -23.2, "Some simple sets/gets work" );

# can we send this to S-Lang?
ok( isa_array($ret1), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret1,Short_Type(),4,1,1,1,1), 1,
    "  [4D] and it seems to have the correct info" );
ok( check_array4d($ret1), "  [4D] and the right values" );

## see if we trash the stack
$ret2 = Array_Type->new( "DataType_Type", [2] );
# note: could probably - with current internals - get away with
#  sending in strings, but not sure I want to rely on this behaviour
$ret2->set(0,Complex_Type());
$ret2->set(1,UShort_Type());
ok( isa_array($ret2), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret2,DataType_Type(),1,2), 1,
    "  [datatype array] and it seems to have the correct info" );
ok( check_array1d_dt($ret2), "  [datatype array] and the right values" );

$ret1 = Struct_Type();
$ret3 = Assoc_Type();
ok( check_multi($ret1,$ret2,$ret3), "Stack seems okay" );

# as an array reference -- ie not an Array_Type object
#
# 1) all integers
$ret1 = [ -9, 4, 23 ];
ok( isa_array($ret1), "Can convert an array ref to a S-Lang array: Int_Type 1D" );
is( check_array($ret1,Integer_Type(),1,3), 1,
    "  and it seems to have the correct info" );
ok( check_arrayref_int1d($ret1), "  and the right values" );

# 2) all floats
$ret1 = [ -9.0, 4.0, 23.0 ];
ok( isa_array($ret1), "Can convert an array ref to a S-Lang array: Double_Type 1D" );
is( check_array($ret1,Double_Type(),1,3), 1,
    "  and it seems to have the correct info" );
ok( check_arrayref_dbl1d($ret1), "  and the right values" );

# 3) all strings
$ret1 = [ "-9.0", "4.0", "23.0" ];
ok( isa_array($ret1), "Can convert an array ref to a S-Lang array: String_Type 1D" );
is( check_array($ret1,String_Type(),1,3), 1,
    "  and it seems to have the correct info" );
ok( check_arrayref_str1d($ret1), "  and the right values" );

# 3) all complex number
$ret1 = [ Math::Complex->make(4,-2.4), Math::Complex->make(-2.4,4) ];
ok( isa_array($ret1), "Can convert an array ref to a S-Lang array: Complex_Type 1D" );
is( check_array($ret1,Complex_Type(),1,2), 1,
    "  and it seems to have the correct info" );
ok( check_arrayref_cpl1d($ret1), "  and the right values" );

# 4) 2D integers (2x2)
$ret1 = [ [ 0, 1 ], [4, 3 ] ];
ok( isa_array($ret1), "Can convert an array ref to a S-Lang array: Int_Type 2D" );
is( check_array($ret1,Integer_Type(),2,2,2), 1,
    "  and it seems to have the correct info" );
ok( check_arrayref_int2d($ret1), "  and the right values" );

# 5) 2D floats (2x3)
$ret1 = [ [-4.0,73], [ 0, 1 ], [4, 3 ] ];
ok( isa_array($ret1), "Can convert an array ref to a S-Lang array: Double_Type 2D" );
is( check_array($ret1,Double_Type(),2,3,2), 1,
    "  and it seems to have the correct info" );
ok( check_arrayref_dbl2d($ret1), "  and the right values" );

# 6) 6D int's (1x1x1x1x1x1)
$ret1 = [ [ [ [ [ [ 24 ] ] ] ] ] ];
ok( isa_array($ret1), "Can convert an array ref to a S-Lang array: Int_Type 6D" );
is( check_array($ret1,Integer_Type(),6,1,1,1,1,1,1), 1,
    "  and it seems to have the correct info" );
ok( check_arrayref_int6d($ret1), "  and the right values" );

# stack checks when sent array references
# - note: use simple arrays only for now
$ret1 = Inline::SLang::sl_array( [1.1,2.2,-43.2], [3], "Double_Type" );
$ret2 = [ "a string", "another one", "fooble" ];
$ret3 = [ [ 0, 1 ], [4, 3 ] ];
ok( check_multiref($ret1,$ret2,$ret3), "Stack seems okay with array references" );

# check Array_Type constructor being sent actual data
#
my $aref = [ [-49.0], [23.2], [1.0e47] ];
$ret1 = Array_Type->new( "Double_Type", [3,1], $aref );
isa_ok( $ret1, "Array_Type" );
( $ret2, $ret3, $ret4 ) = $ret1->array_info();
is( $ret3, 2, "2D array is 2D" );
ok( eq_array( $ret2, [3,1] ), "  and is 3x1" );
is( "$ret4", "Double_Type", "  and the type is Double_Type" );

ok( $ret1->get(0,0) == -49.0 &&
    $ret1->get(1,0) == 23.2 &&
    $ret1->get(2,0) == 1.0e47,
    "Some simple gets work" );

# can we send this to S-Lang?
ok( isa_array($ret1), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret1,Double_Type(),2,3,1), 1,
    "  [2D] and it seems to have the correct info" );
ok( check_array2d_b($ret1), "  [2D] and the right values" );

### check the sl_array() constructor
# [it's just a wrapper around the Array_Type constructor]
#
# basically the same test but with different values sent to the
# function
#
print "--- Checking Inline::SLang::sl_array\n";
$ret1 = undef;
$ret1 = Inline::SLang::sl_array( $aref, [3,1], "Double_Type" );
isa_ok( $ret1, "Array_Type" );
( $ret2, $ret3, $ret4 ) = $ret1->array_info();
is( $ret3, 2, "2D array is 2D" );
ok( eq_array( $ret2, [3,1] ), "  and is 3x1" );
is( "$ret4", "Double_Type", "  and the type is Double_Type" );
ok( isa_array($ret1), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret1,Double_Type(),2,3,1), 1,
    "  [2D] and it seems to have the correct info" );
ok( check_array2d_b($ret1), "  [2D] and the right values" );

print "--- Checking sl_array (ie can we export it to main)\n";
$ret1 = undef;
$ret1 = sl_array( $aref, [3,1], "Double_Type" );
isa_ok( $ret1, "Array_Type" );
( $ret2, $ret3, $ret4 ) = $ret1->array_info();
is( $ret3, 2, "2D array is 2D" );
ok( eq_array( $ret2, [3,1] ), "  and is 3x1" );
is( "$ret4", "Double_Type", "  and the type is Double_Type" );
ok( isa_array($ret1), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret1,Double_Type(),2,3,1), 1,
    "  [2D] and it seems to have the correct info" );
ok( check_array2d_b($ret1), "  [2D] and the right values" );

print "--- Checking sl_array - can it guess array dims\n";
$ret1 = undef;
$ret1 = sl_array( $aref, "Double_Type" );
isa_ok( $ret1, "Array_Type" );
( $ret2, $ret3, $ret4 ) = $ret1->array_info();
is( $ret3, 2, "2D array is 2D" );
ok( eq_array( $ret2, [3,1] ), "  and is 3x1" );
is( "$ret4", "Double_Type", "  and the type is Double_Type" );
ok( isa_array($ret1), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret1,Double_Type(),2,3,1), 1,
    "  [2D] and it seems to have the correct info" );
ok( check_array2d_b($ret1), "  [2D] and the right values" );

print "--- Checking sl_array - can it guess array type\n";
$ret1 = undef;
$ret1 = sl_array( $aref, [3,1] );
isa_ok( $ret1, "Array_Type" );
( $ret2, $ret3, $ret4 ) = $ret1->array_info();
is( $ret3, 2, "2D array is 2D" );
ok( eq_array( $ret2, [3,1] ), "  and is 3x1" );
is( "$ret4", "Double_Type", "  and the type is Double_Type" );
ok( isa_array($ret1), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret1,Double_Type(),2,3,1), 1,
    "  [2D] and it seems to have the correct info" );
ok( check_array2d_b($ret1), "  [2D] and the right values" );

print "--- Checking sl_array - can it guess everything\n";
$ret1 = undef;
$ret1 = sl_array( $aref );
isa_ok( $ret1, "Array_Type" );
( $ret2, $ret3, $ret4 ) = $ret1->array_info();
is( $ret3, 2, "2D array is 2D" );
ok( eq_array( $ret2, [3,1] ), "  and is 3x1" );
is( "$ret4", "Double_Type", "  and the type is Double_Type" );
ok( isa_array($ret1), "Can convert Array_Type to a S-Lang array" );
is( check_array($ret1,Double_Type(),2,3,1), 1,
    "  [2D] and it seems to have the correct info" );
ok( check_array2d_b($ret1), "  [2D] and the right values" );


# and now some random checks
is( sumup_nelems( [0], ["a", "b"], [3.4, 5.6, 9.4, 55] ),
    7, "able to add up 1D elements" );
is( sumup_nelems( [0],
                  sl_array( [["a","b"],["cc","d"]], [2,2], "String_Type" ),
                  [3.4, 5.6, 9.4, 55] ),
    9, "able to add up 1D + 2D elements [sl_array full]" );
is( sumup_nelems( [0],
                  sl_array( [["a","b"],["cc","d"]], [2,2] ),
                  [3.4, 5.6, 9.4, 55] ),
    9, "able to add up 1D + 2D elements [sl_array no type]" );
is( sumup_nelems( [0],
                  sl_array( [["a","b"],["cc","d"]] ),
                  [3.4, 5.6, 9.4, 55] ),
    9, "able to add up 1D + 2D elements [sl_array no type/dims]" );
is( sumup_nelems( [0], [ ["a", "b"], ["cc", "d"] ], [3.4, 5.6, 9.4, 55] ),
    9, "able to add up 1D + 2D elements [array ref]" );

=end OLDCODE

=cut

__END__
__SLang__

#ifndef sum
define sum(x) { variable tot = 0; foreach ( x ) { tot += (); } return tot; }
#endif
define all(x) {
  variable dims, ndims, nelem;
  ( dims, ndims, ) = array_info(x);
  nelem = 1;
  foreach ( dims ) { variable y = (); nelem *= y; }
  return sum(x!=0) == nelem;
}
define any(x) { return sum(x!=0) != 0; }

define isa_array(a,dims,type) {
    if (
	andelse
	{ typeof(a) == Array_Type }
	{ _typeof(a) == type }
       ) {
	variable adims;
	( adims, , ) = array_info(a);
	return all( adims == dims );
    }
    return 0;
}

define check_byte1d(in) {
  if ( _typeof(in) != UChar_Type ) return 0;
  variable out = typecast( [1,2,3,128,255,0], UChar_Type );
  return all( in == out );
}

define check_short1d(in) {
  if ( _typeof(in) != Short_Type ) return 0;
  variable out = typecast( [-2,-1,0,1,2], Short_Type );
  return all( in == out );
}

define check_ushort1d(in) {
  if ( _typeof(in) != UShort_Type ) return 0;
  variable out = typecast( [2,1,0,1,2], UShort_Type );
  return all( in == out );
}

define check_long1d(in) {
  if ( _typeof(in) != Integer_Type ) return 0;
  variable out = typecast( [-3,-16,0,1,2], Integer_Type );
  return all( in == out );
}

define check_float1d(in) {
  if ( _typeof(in) != Float_Type ) return 0;
  variable out = typecast( [-5:4]/2.0, Float_Type );
  return all( in == out );
}

define check_dble1d(in) {
  if ( _typeof(in) != Double_Type ) return 0;
  variable out = typecast( [-5:4]/2.0, Double_Type );
  return all( in == out );
}

define check_byte2d(in) {
  if ( _typeof(in) != UChar_Type ) return 0;
  variable out = UChar_Type [3,2];
  out[*,0] = [1,2,3];
  out[*,1] = [128,255,0];
  return all( in == out );
}

define check_short2d(in) {
  if ( _typeof(in) != Short_Type ) return 0;
  variable out = Short_Type [4,2];
  out[*,0] = [0,1,2,3];
  out[*,1] = out[*,0];
  return all( in == out );
}

define check_dble2d(in) {
  if ( _typeof(in) != Double_Type ) return 0;
  variable out = typecast([0:7],Double_Type);
  reshape(out,[2,4]);
  return all( in == out );
}

% OLD

define check_array() {
  variable a, itype, ndims, size;
  _stk_reverse(_NARGS);
  ( ndims, itype, a ) = ();
  variable asize, andims, atype;
  ( asize, andims, atype ) = array_info(a);

  if ( andims != ndims ) return 0;
  if ( atype != itype ) return 0;

  _stk_reverse(_NARGS-3);
  size = __pop_args(_NARGS-3);
  variable i;
  _for( 0, ndims-1, 1 ) {
    i = ();
    if ( size[i].value != asize[i] ) return 0;
  }
  return 1;
}

define check_array1d(a) { return all(a == [1,23,-10]); }
define check_array1d_dt(a) { return all(a == [Complex_Type,UShort_Type]); }

define check_array2d(a) {
  variable out = Float_Type [2,3];
  out[0,0] = 0.0; out[1,0] = 10.0;
  out[0,1] = 1.0; out[1,1] = 11.0;
  out[0,2] = 2.0; out[1,2] = 12.0;
  return all( a == out );
}
define check_array2d_b(a) {
  variable out = Double_Type [3,1];
  out[0,0] = -49.0;
  out[1,0] = 23.2;
  out[2,0] = 1.0e47;
  return all( a == out );
}

define check_array3d(a) {
  variable out = Short_Type [1,3,2];
  out[0,0,*] = [1,2];
  out[0,1,*] = [10,11];
  out[0,2,*] = [20,21];
  return all( a == out );
}
define check_array4d(a) { return a[0,0,0,0] == -23.2; }

define check_arrayref_int1d(x) { return all( x == [-9,4,23] ); }
define check_arrayref_dbl1d(x) { return all( x == [-9.0,4.0,23.0] ); }
define check_arrayref_str1d(x) { return all( x == ["-9.0","4.0","23.0"] ); }
define check_arrayref_cpl1d(x) { return all( x == [4-2.4i,-2.4+4i] ); }

define check_arrayref_int2d(x) {
  variable match = Int_Type [2,2];
  match[0,*] = [0,1];
  match[1,*] = [4,3];
  return all( x == match );
}

define check_arrayref_dbl2d(x) {
  variable match = Double_Type [3,2];
  match[0,*] = [-4.0,73.0];
  match[1,*] = [0.0,1.0];
  match[2,*] = [4.0,3.0];
  return all( x == match );
}

define check_arrayref_int6d(x) {
  variable match = Int_Type [1,1,1,1,1,1];
  match[0,0,0,0,0,0] = 24;
  return all( x == match );
}

% to compare any_type things we have to dereference the values
%
define _comp_any (a,b) { return @a == @b; }
define check_arrayref_any1d(a) { 
  variable x = Any_Type [5];
  x[0] = -9;
  x[1] = "foo";
  x[2] = 23.0;
  x[3] = 4-3i;
  x[4] = Struct_Type;
  return all( array_map( Char_Type, &_comp_any, a, x ) );
}

define check_multi(x,y,z) {
  if ( orelse 
       { typeof(x) != DataType_Type }
       { x != Struct_Type }
     ) return 0;
  if ( orelse 
       { typeof(z) != DataType_Type }
       { z != Assoc_Type }
     ) return 0;
  % swapped logic
  if ( andelse 
       { isa_array(y) }
       { check_array1d_dt(y) }
     ) return 1;
  return 0;
}

define check_multiref(x,y,z) {
  if ( orelse 
       { typeof(x)  != Array_Type }
       { _typeof(x) != Double_Type }
     ) return 0;
  if ( orelse 
       { typeof(y)  != Array_Type }
       { _typeof(y) != String_Type }
     ) return 0;
  if ( orelse 
       { typeof(z)  != Array_Type }
       { _typeof(z) != Integer_Type }
     ) return 0;
  if ( sum( any( x != [1.1,2.2,-43.2] ) ) ) return 0;
  if ( sum( any( y != ["a string", "another one", "fooble"] ) ) ) return 0;

  variable dims, ndims;
  ( dims, ndims, ) = array_info( z );
  if ( orelse
       { ndims != 2 }
       { any( dims != [2,2] ) }
     ) return 0;
  variable zz = Integer_Type [2,2];
  zz[0,*] = [0,1];
  zz[1,*] = [4,3];
  if ( sum( any( z != zz ) ) ) return 0;

  return 1;
}

define add2 (a) { return a+2; }

define sumup_nelems () {
  variable sum = 0;
  foreach ( __pop_args(_NARGS) ) {
    variable arg = ();
    sum += length(arg.value);
  }
  return sum;
}

