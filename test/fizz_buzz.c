int main() {
  for (int i = 0; i < 16; i++) {
    if (i % 3 == 0) {
      puti(i);
      putc(32);
    }
    if (i % 5 == 0) {
      puti(i);
      putc(32);
    }
    if (i % 3 == 0 && i % 5 == 0) {
      puti(i);
      putc(32);
    }
  }
}

