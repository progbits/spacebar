int main() {
  int i = 0;
  int a[8];
  for (; i < 8; i++) {
    a[i] = i * 10;
  }

  i = 0;
  for (; i < 8; i++) {
    puti(a[i]);
    putc(32);
  }
}

