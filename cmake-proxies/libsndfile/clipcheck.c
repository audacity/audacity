#define  _ISOC9X_SOURCE 1
#define _ISOC99_SOURCE  1
#define  __USE_ISOC99   1
#define __USE_ISOC9X 1
#include <math.h>
#include <stdio.h>
int main (void)
{
   double fval ;
   int k, ival ;
   int pos = 0 ;
   int neg = 0 ;

   fval = 1.0 * 0x7FFFFFFF ;
   for (k = 0 ; k < 100 ; k++)
   {
      ival = (lrint (fval)) >> 24 ;
      if (ival != 127)
      {
         pos = 1 ;
         break ;
      }

      fval *= 1.2499999 ;
   }

   fval = -8.0 * 0x10000000 ;
   for (k = 0 ; k < 100 ; k++)
   {
      ival = (lrint (fval)) >> 24 ;
      if (ival != -128)
      {
         neg = 1 ;
         break ;
      }

      fval *= 1.2499999 ;
   }

   printf("%d;%d", pos, neg) ;

   return 0 ;
}

