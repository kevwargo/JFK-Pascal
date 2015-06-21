#include <stdio.h>
int T1[3 - (-1) + 1][5 - (4) + 1];
char T2['d' - ('a') + 1]['m' - ('k') + 1];
int main()
{
  T1[2 - (-1)][4 - (4)] = 2;
  T2['c' - ('a')]['l' - ('k')] = 'f';
  return 0;
}
