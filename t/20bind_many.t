#
# test binding functions
# - earlier tests have implivitly tested the 
#     BIND_NS => [ "Global" ] 
#   option
#
# For S-Lang v1.4.1 the namespace tests do not work
#   ie we can not call foo->fn_in_foo()
# [at least I'm assuming it's an error in S-Lang and
#  not the perl interface since it works on 1.4.4 and
#  1.4.3 included some fixes to the namepace support]
#
# So, for now (v0.05), we skip some of these tests
# for S-Lang < 1.4.3 [although I have only tested for 1.4.4 &  1.4.8]
#

use strict;

use Test::More tests => 2;

## Tests

use Inline 'SLang' => Config => BIND_NS => [ "foo", "Global" ];
use Inline 'SLang' => <<'EOS1';

define fn_in_global(x) { "in global"; }

implements( "foo" );

define fn_in_foo(x) { "in foo"; }

EOS1

is( fn_in_global(1), "in global",
  "Can call fn_in_global() as fn_in_global()" );

# can we test out namespaces?
my $use_ns = Inline::SLang::sl_eval( "_slang_version > 10402;" );

SKIP: {
    skip
      "namespace handling appears broken in early v1.4.X S-Lang libs", 1
	unless $use_ns;

    is( foo::fn_in_foo("dummy"), "in foo",
	"Can call foo->fn_in_foo() as foo::fn_in_foo()" );
}

## End

