
use Inline SLang;

$a = ret1();
print "Class of \$a is " . ref($a) . "\n";
print "$a\n";

$b = ret2();
print "Class of \$b is " . ref($b) . "\n";
print "$b\n";

__END__
__SLang__

define ret1() {
  variable a = struct { a, b };
  a.a = 1;
  a.b = "foo";
  return a;
}

typedef struct { a, b } SimpleStruct_Type;
define ret2() {
  variable a = @SimpleStruct_Type;
  a.a = 2;
  a.b = "bar";
  return a;
}
