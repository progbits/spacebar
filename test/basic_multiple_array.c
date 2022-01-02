int main() {
  int a[8];
  for (int i = 0; i < 8; i++) {
    a[i] = i + 1;
  }

  int b[8];
  for (int i = 0; i < 8; i++) {
    b[i] = i * 10;
  }

  int c[8];
  for (int i = 0; i < 8; i++) {
    c[i] = a[i] + b[i];
  }

  for (int i = 0; i < 8; i++) {
    puti(c[i]);
    putc(32);
  }
}

