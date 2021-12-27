int main() {
  int i = 0;
  while(1) {
    i = i + 1;
    puti(i);
    if (i > 10) {
      break;
    }
  }
  puti(42);
}
