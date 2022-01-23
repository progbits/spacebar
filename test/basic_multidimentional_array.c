int main() {
  int (a[4])[3];

  for (int i = 0; i < 4; i++) {
    a[i][2] = (i + 1) * 2;
  }

  puti(a[0][2]);
  putc(32);

  puti(a[1][2]);
  putc(32);

  puti(a[2][2]);
  putc(32);

  puti(a[3][2]);
  putc(32);
}

