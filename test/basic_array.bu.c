int main() {
  int a[8];
  int b = 0;

  for (int i = 0; i < 8; i++) {
    a[i] = i * 10;
  }

  puti(b);
  putc(32);

  for (int j = 0; j < 8; j++) {
    puti(a[j]);
    putc(32);
  }
}
