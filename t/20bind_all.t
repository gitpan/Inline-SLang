#
# test binding functions: BIND_NS => "All"
#
# - this requires S-Lang v1.4.7 or later
#

use strict;

use Test::More tests => 2;

## Tests

my $use_ns;

BEGIN {
  eval '
use Inline \'SLang\' => Config => BIND_NS => "All";
use Inline \'SLang\' => <<\'EOS1\';

define fn_in_global(x) { "in global"; }

implements( "foo" );

define fn_in_foo(x) { "in foo"; }

EOS1
';
  $use_ns = $@ eq "";

}

SKIP: {
    skip
      "BIND_NS => 'All' requires >= v1.4.7 of S-Lang", 2
	unless $use_ns;

    is( fn_in_global(1), "in global",
	"Can call fn_in_global() as fn_in_global()" );

    is( foo::fn_in_foo("dummy"), "in foo",
	"Can call foo->fn_in_foo() as foo::fn_in_foo()" );
}

## End

