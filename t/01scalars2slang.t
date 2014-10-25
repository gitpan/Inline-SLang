#
# test in/out of scalars
#
# many of these tests shouldn't be direct equality
# since it's floating point
#

use strict;

use Test::More tests => 19;

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

## perl 2 S-Lang

$ret1 = add2( 3 );
is( $ret1, 5, '2+3 = 5' );

$ret1 = add2( 3.9 );
approx( $ret1, 5.9, '2+3.9 = 5.9' );

$ret1 = concatfoo( "bar" );
is( $ret1, "barfoo", 'bar + foo = barfoo' );

# need to quote the 2 to make it a string
$ret1 = concatfoo( "2" );
is( $ret1, "2foo", '"2" + foo = 2foo' );

## complex numbers

$ret1 = Math::Complex->make(3,-4);
ok( is_complex($ret1), "perl complex translated to S-Lang complex" );
ok( check_complex($ret1), "  and the value is okay" );

## Null values

$ret1 = sendnull(undef);
is( $ret1, 1, 'undef (perl) converted to NULL (S-Lang)' );

$ret1 = sendnull('foo');
is( $ret1, 0, '"foo" != NULL' );

## datatypes

# now, Int_Type is a synonym, so let's see if it gets
# converted to Integer_Type?
$ret1 = Inline::SLang::DataType_Type->new( "Int_Type" );
isa_ok( $ret1, "Inline::SLang::DataType_Type" );
isa_ok( $ret1, "Inline::SLang::_Type" );
ok( !$ret1->is_struct_type, "and we are not a structure" );

is( "$ret1", "Integer_Type",
	"Able to 'stringify' the Inline::SLang::DataType_Type object" );

foreach my $type ( qw( DataType_Type UChar_Type Any_Type Assoc_Type ) ) {
    $ret1 = is_datatype(
	$type,
	Inline::SLang::DataType_Type->new( $type ) );
    ok( $ret1, "Recognises as a datatype: $type" );
}

# no type
$ret1 = Inline::SLang::DataType_Type->new();
isa_ok( $ret1, "Inline::SLang::DataType_Type" );
is( "$ret1", "DataType_Type", "empty constructor converts to DataType_Type" );

# incorrect type
#
# - perhaps we shouldn't include this test since it prints to STDERR ?
#
print "+++ NOTE: the error messages printed to the screen below are okay +++\n";
$ret1 = Inline::SLang::DataType_Type->new("FooFooFooFoo");
ok( !defined $ret1, "Can not create an unrecognised type" );

__END__
__SLang__

define add2 (a) { return a+2; }

define concatfoo () { variable str = (); return str + "foo"; }

define is_complex (x) { return typeof(x) == Complex_Type; }
define check_complex (x) { return x == 3 - 4i; }

% let S-Lang handle the conversion of a datatype to a string
define is_datatype (x,y) { return x == string(y); }

%% check the stack (variable args)

% if we don't clear the stack via _pop_n() we really mess up
define nvarargs () {
  variable n = _NARGS;
  () = printf( "varargs was sent %d arguments\n", n );
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

