
use strict;

my $loaded = 0;
BEGIN { use Test::More tests => 2; }
END   { fail( "Able to 'use Inline SLang'" ) unless $loaded; }

## Tests

# could not work out how to use Test::More's use_ok()
# to test loading the module
#
use Inline 'SLang';
pass( "Able to 'use Inline SLang'" );
$loaded = 1;

# mainly here for users who 'perl -Mblib t/00init.t'
# this file
#
eval { print JAxH('Inline'); };
is( $@, "", "We're just another Inline hacker" );

__END__

__SLang__

define somefunc () {}

define JAxH(x) {
  () = printf( "Just Another %s Hacker\n", x );
}
