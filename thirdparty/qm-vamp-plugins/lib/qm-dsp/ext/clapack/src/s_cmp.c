#include "f2c.h"
#ifdef __cplusplus
extern "C" {
#endif

/* compare two strings */

#ifdef KR_headers
integer s_cmp(a0, b0, la, lb) char *a0, *b0; ftnlen la, lb;
#else
integer s_cmp(char *a0, char *b0, ftnlen la, ftnlen lb)
#endif
{
register unsigned char *a, *aend, *b, *bend;
a = (unsigned char *)a0;
b = (unsigned char *)b0;
aend = a + la;
bend = b + lb;

if(la <= lb)
	{
	while(a < aend)
		if(*a != *b)
			return( *a - *b );
		else
			{ ++a; ++b; }

	while(b < bend)
		if(*b != ' ')
			return( ' ' - *b );
		else	++b;
	}

else
	{
	while(b < bend)
		if(*a == *b)
			{ ++a; ++b; }
		else
			return( *a - *b );
	while(a < aend)
		if(*a != ' ')
			return(*a - ' ');
		else	++a;
	}
return(0);
}
#ifdef __cplusplus
}
#endif
