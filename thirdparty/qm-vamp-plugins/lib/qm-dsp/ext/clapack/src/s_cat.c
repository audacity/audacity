/* Unless compiled with -DNO_OVERWRITE, this variant of s_cat allows the
 * target of a concatenation to appear on its right-hand side (contrary
 * to the Fortran 77 Standard, but in accordance with Fortran 90).
 */

#include "f2c.h"
#ifndef NO_OVERWRITE
#include "stdio.h"
#undef abs
#ifdef KR_headers
 extern char *F77_aloc();
 extern void free();
 extern void exit_();
#else
#undef min
#undef max
#include "stdlib.h"
extern
#ifdef __cplusplus
	"C"
#endif
	char *F77_aloc(ftnlen, const char*);
#endif
#include "string.h"
#endif /* NO_OVERWRITE */

#ifdef __cplusplus
extern "C" {
#endif

 VOID
#ifdef KR_headers
s_cat(lp, rpp, rnp, np, ll) char *lp, *rpp[]; ftnint rnp[], *np; ftnlen ll;
#else
s_cat(char *lp, char *rpp[], ftnint rnp[], ftnint *np, ftnlen ll)
#endif
{
	ftnlen i, nc;
	char *rp;
	ftnlen n = *np;
#ifndef NO_OVERWRITE
	ftnlen L, m;
	char *lp0, *lp1;

	lp0 = 0;
	lp1 = lp;
	L = ll;
	i = 0;
	while(i < n) {
		rp = rpp[i];
		m = rnp[i++];
		if (rp >= lp1 || rp + m <= lp) {
			if ((L -= m) <= 0) {
				n = i;
				break;
				}
			lp1 += m;
			continue;
			}
		lp0 = lp;
		lp = lp1 = F77_aloc(L = ll, "s_cat");
		break;
		}
	lp1 = lp;
#endif /* NO_OVERWRITE */
	for(i = 0 ; i < n ; ++i) {
		nc = ll;
		if(rnp[i] < nc)
			nc = rnp[i];
		ll -= nc;
		rp = rpp[i];
		while(--nc >= 0)
			*lp++ = *rp++;
		}
	while(--ll >= 0)
		*lp++ = ' ';
#ifndef NO_OVERWRITE
	if (lp0) {
		memcpy(lp0, lp1, L);
		free(lp1);
		}
#endif
	}
#ifdef __cplusplus
}
#endif
