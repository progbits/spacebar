int main() {
  for (int i = 0; i < 10; i++) {
    if (i == 3 || i == 5) {
      puti(i);
      putc(32);
    }
    if (i == 4 || i == 2) {
      puti(i);
      putc(32);
    }
  }
}
