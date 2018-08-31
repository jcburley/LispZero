#include <stdio.h>

int
inc(void)
{
  static int counter;
  ++counter;
  return counter;
}

int
main()
{
  printf("%d %d %d\n", inc(), inc(), inc());
}
