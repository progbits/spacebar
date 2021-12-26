void foo(int *x) {
  puti(*x);
}

int main() {
  int x = 42;
  foo(&x);
}

