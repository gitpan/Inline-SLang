#
# test binding functions: BIND_NS => "All"
#
# - this requires S-Lann v1.4.7
#

use strict;

use Test::More tests => 2;

## Tests

use Inline 'SLang' => Config => BIND_NS => "All";
use Inline 'SLang' => <<'EOS1';

define fn_in_global(x) { "in global"; }

implements( "foo" );

define fn_in_foo(x) { "in foo"; }

EOS1

# can we test out namespaces?
my $use_ns = Inline::SLang::sl_eval( "_slang_version > 10406;" );

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

