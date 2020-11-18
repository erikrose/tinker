#include <stdio.h>


extern double code();

int main() {
  double result = code();
  printf("%f\n", result);
  return 0;
}
