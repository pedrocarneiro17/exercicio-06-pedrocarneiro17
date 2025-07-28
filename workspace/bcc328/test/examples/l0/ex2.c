#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  static char buf[2048];

  {
    const int v_x = 1;
    printf("%d\n", v_x);
    {
      const int v_x = 2;
      printf("%d\n", v_x);
    }
    printf("%d\n", v_x);
  }

  return 0;

}
