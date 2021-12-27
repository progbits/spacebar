int main() {
  int i = 0;
  while(1) {
    i = i + 1;
    if (i > 10) {
      break;
    }

    if (i == 3) {
      continue;
    }

    if (i == 5) {
      continue;
    }
    puti(i);
  }
  puti(42);
}
