#include <stdlib.h>
typedef struct
{
   int k;
   char buffer [];
} MY_STRUCT;
int main(void)
{
   MY_STRUCT *p = calloc (1, sizeof (MY_STRUCT) + 42);
   return 0;
}

