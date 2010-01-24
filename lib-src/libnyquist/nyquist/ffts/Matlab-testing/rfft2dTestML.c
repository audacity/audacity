/*  A program to test real 2d forward and inverse fast fourier transform routines	*/

#include <stdio.h>
#include <stdlib.h>
#include <fp.h>
#include <math.h>
#include "fftlib.h"
#include "fftext.h"
#include "fft2d.h"

#if macintosh
#include <timer.h>
#endif


#define	BIPRAND(a) (2.0/(RAND_MAX+1.0)*a-1.0)

void main(){
long 	N2 = 64;	/* the number of rows in 2d ffts, must be power of 2 */
long 	N = 256;	/* the number of cols in 2d ffts, must be power of 2 */
float	*a;
float	maxerrfft;
long 	i1;
long 	TheErr;
long		M;
long		M2;

FILE *fdataout;				/* output file */

unsigned int	randseed = 777;
int		rannum;
#if macintosh
	UnsignedWide 		TheTime1;
	Microseconds(&TheTime1);
	randseed = TheTime1.lo;
#endif

printf(" %6d  Byte Floats \n", sizeof(a[0]));
printf(" randseed = %10u\n", randseed);

srand(randseed);
M = roundtol(LOG2(N));
N = POW2(M);
M2 = roundtol(LOG2(N2));
N2 = POW2(M2);

printf("fft size = %6d X%6d,  ", N2, N);

TheErr = 0;

if(!TheErr){
	TheErr = fft2dInit(M2, M);
}

a = (float *) malloc(N2*N*sizeof(float) );
if (a == 0) TheErr = 2;
if(!TheErr){
	fdataout = fopen("fftdat.dr2", "wb");
	if (fdataout == NULL) TheErr = -50;
}
if(!TheErr){

		/*  write sizes to fdataout */
	fwrite(&N, sizeof(N), 1, fdataout);
	fwrite(&N2, sizeof(N2), 1, fdataout);
		/*  set up a simple test case and write to fdataout */
	for (i1=0; i1<N2*N; i1++){
		rannum = rand();
		a[i1] = BIPRAND(rannum);
	}
	fwrite(a, N2*N*sizeof(float), 1, fdataout);

	/* real, 2d fast fourier transform */
	rfft2d(a, M2, M);

	/* write out answer */
	fwrite(a, N2*N*sizeof(float), 1, fdataout);
	fclose(fdataout);

	/* compute and check inverse transform */
	rifft2d(a, M2, M);

	maxerrfft = 0;
	srand(randseed);
	for (i1=0; i1<N2*N; i1++){
		rannum = rand();
		maxerrfft = fmax(maxerrfft, fabs(BIPRAND(rannum)-a[i1]));
	}

	printf("maxerr rfft = %6.4e\n", maxerrfft);

	free(a);
	fft2dFree();
}
else{
	if(TheErr==2)	printf(" out of memory \n");
	else	printf(" error \n");
	fft2dFree();
}
printf(" Done. \n");
return;
}
