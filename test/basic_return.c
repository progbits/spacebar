int baz() {
  return 41;
}

int bar() {
  return baz() + 1;
}

int foo() {
  return bar() + 1;
}

int main() {
  int a = foo();
  foo();
  puti(a);
}
