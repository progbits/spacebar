int fib(int i) {
  if (i < 2) {
    return i;
  }

  return fib(i - 1) + fib(i - 2);
}

int main() {
  puti(fib(13));
  putc(32);
}

