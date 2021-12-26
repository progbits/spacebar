void foo(int a) {
  if (a < 10) {
    puti(42);
    putc(32);
  }

  if (a < 5) {
    puti(84);
  }
}

int main() {
  foo(2);
  putc(32);
  foo(5);
  putc(32);
  foo(10);
  putc(32);
  foo(11);
}

