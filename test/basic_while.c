int main() {
  int j = 0;
  int i = 42;
  while (j < 10) {
    putc(i + j);
    j = j + 1;
  }
}
