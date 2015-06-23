#include <stdio.h>
int A, B, C;
double E, F, G;
int main()
{
  A = 1;
  B = 2;
  C = 4;
  A = A * ((B << C) % B);
  printf("%d\n",A);
  G = 1;
  F = 2;
  E = F / G;
  printf("%2.2f\n",E);
  return 0;
}
