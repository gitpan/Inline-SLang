#
# test in/out of structures
#
# we also test the Inline::SLang::Struct_Type object
# (since it's our object we'd best test it!)
#

use strict;

use Test::More tests => 39;

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

my ( $ret1, $ret2, $ret3, @ret );

## 

## S-Lang 2 perl

$ret1 = struct1();
##print Dumper( $ret1 ), "\n";
isa_ok( $ret1, "Inline::SLang::Struct_Type" );
isa_ok( $ret1, "Inline::SLang::_Type" );
ok( $ret1->is_struct_type, "and we are a structure" );

ok( eq_array( $ret1->get_field_names,
	      [ "f1", "f2", "f4", "f3" ] ),
    "  contains the correct fields (in the right order)" );
is( $ret1->get_field("f1"),    1, "    f1 == 1" );
is( $ret1->get_field("f2"), "f2", "    f2 == 'f2'" );
ok( eq_array( $ret1->get_field("f4"), [1,2,3]),
    "    f4 == [1,2,3]" );
is( $ret1->get_field("f3"), undef, "    f3 == undef" );

$ret1->set_field("f1",2);
is( $ret1->get_field("f1"), 2, "changed f1 to 2" );

$ret1->set_field("f2",-1,"f3",-2.1);
ok( eq_array( [$ret1->get_field("f2","f3")], [-1,-2.1] ),
    "changed f2 to -1 & f3 to -2.1" );

# test stringification on an easy structure
$ret1 = struct2();
is(
   "$ret1",
   "Structure Type: Struct_Type\n" .
   join( "", map { "\t$$_[0]\t= $$_[1]\n" } ( ["x1","a string"], ["y2","another string"] ) ),
   "The stringification works"
   );

# check we play nicely with the stack
( $ret1, $ret2, $ret3 ) = ret_multi();
ok( $ret1 == "more strings" && $ret3 == -234.5,
  "multi return: non-struct vals okay" );
isa_ok( $ret2, "Inline::SLang::Struct_Type" );
ok( !defined($ret2->get_field("gonzo")), "and structure field is NULL/undef" );

# test type-deffed structures
$ret1 = retbar();
isa_ok( $ret1, "Inline::SLang::_Type" );
isa_ok( $ret1, "Inline::SLang::Struct_Type" );
isa_ok( $ret1, "Inline::SLang::Bar_Type" );
is( $ret1->is_struct_type, 1, "typedef {}... returns a structure" );

ok( eq_array( $ret1->get_field_names, [ "foo", "bar" ] ),
    "  and contains the correct fields (in the right order)" );
is( $ret1->get_field("foo"),     2, "    foo == 2" );
is( $ret1->get_field("bar"), "baz", "    bar == 'baz'" );
is(
   "$ret1",
   "Structure Type: Bar_Type\n" .
   join( "", map { "\t$$_[0]\t= $$_[1]\n" } ( ["foo",2], ["bar","baz"] ) ),
   "  and the stringification works"
   );

## Perl 2 S-Lang

# first test the object constructor
#
$ret1 = Inline::SLang::Struct_Type->new( ['a','x','a_space'] );
##print Dumper( $ret1 ), "\n";
isa_ok( $ret1, "Inline::SLang::Struct_Type" );
isa_ok( $ret1, "Inline::SLang::_Type" );
ok( $ret1->is_struct_type, "and we are a structure" );

# have tested set_field() above, but repeat
# - note leave 'a_space' as undef
my $label = "  able to set fields in created structure";
$ret1->set_field( "x" => 'a string', 'a' => [1,2,4] );
is( $ret1->get_field("x"), "a string", $label );
is( $ret1->get_field("a_space"), undef, $label );
ok( eq_array( $ret1->get_field("a",), [1,2,4] ), $label );

# debug: currently can't convert an array reference form perl to S-Lang, so change field a
$ret1->set_field( 'a' => 1.2 );

# now, can we convert it to a S-Lang Struct_Type?
ok( is_a_struct($ret1),
    "Can convert Inline::SLang::Struct_Type to Struct_Type" );
ok( check_struct_fields($ret1,"a","x","a_space"),
	"  and the field names/order are correct" );

# check we don't mess up the stack
ok( send3("a string",$ret1,Inline::SLang::DataType_Type->new("Float_Type")),
   "Inline::SLang::Struct_Type 2 S-Lang plays okay w/ stack" );

# test type-deffed structures
$ret1 = Inline::SLang::Bar_Type->new();
isa_ok( $ret1, "Inline::SLang::_Type" );
isa_ok( $ret1, "Inline::SLang::Struct_Type" );
isa_ok( $ret1, "Inline::SLang::Bar_Type" );
is( $ret1->is_struct_type, 1, "typedef {}... returns a structure" );

$label = "  able to set fields in type-deffed structure";
$ret1->set_field( "bar" => 3, "foo" => "bar" );
is( $ret1->get_field("foo"), "bar", $label );
is( $ret1->get_field("bar"), 3, $label );
ok( check_bar($ret1), "  and can convert to S-Lang" );

# check we don't mess up the stack
ok( send3("a string",$ret1,Inline::SLang::DataType_Type->new("Float_Type")),
   "Inline::SLang::Bar_Type 2 S-Lang plays okay w/ stack" );

__END__
__SLang__

define struct1 () {
  variable a = struct { f1, f2, f4, f3 };
  a.f1 = 1.0;
  a.f2 = "f2";
  a.f4 = [1,2,3];
  a.f3 = NULL;
  return a;
}

define struct2 () {
  variable a = struct { x1, y2 };
  a.x1 = "a string";
  a.y2 = "another string";
  return a;
}

% also see how we handle NULL value types
define ret_multi () {
  return "more strings", struct { gonzo }, -234.5;
}

% test
typedef struct { foo, bar } Bar_Type;
define retbar() {
  variable bar = @Bar_Type;
  bar.foo = 2;
  bar.bar = "baz";
  return bar;
} % retbar()

%% Perl 2 S-Lang

define is_a_struct (x) { return is_struct_type(x); } 

define send3 (x,y,z) {
  if ( andelse
       { x == "a string" }
       { is_struct_type(y) }
       { z == Float_Type } )
    return 1;
  else
    return 0;
}

define check_struct_fields () {
  %%_print_stack();
  if ( _NARGS < 2 )
    verror( "Usage: Int_Type = %s(Struct_Type,String_Type,...);\n", _function_name );

  % grab the variables from the stack
  variable fields = __pop_args( _NARGS-1 );
  variable s = ();

  variable names = get_struct_field_names(s);
  if ( length(names)  != length(fields) ) return 0;

  % want names and fields to be equal
  _for ( 0, length(names)-1, 1 ) {
    variable i = ();
    if ( names[i] != fields[i].value ) return 0;
  }
  return 1;
} % check_struct_fields

define check_bar(in) {
  if (
    andelse
    { typeof(in) == Bar_Type }
    { in.foo == "bar" }
    { in.bar == 3 } )
    return 1;
  else
    return 0;
} % check_bar()
