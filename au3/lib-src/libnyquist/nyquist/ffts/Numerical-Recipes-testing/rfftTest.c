/*  A program to test real forward and inverse fast fourier transform routines	*/

#include <NR.H>	/* uses realft from numerical recipes in C to verify riffts */
			/*change fmin in numerical recipes to fminnr to avoid conflict with fp.h */

#include <stdio.h>
#include <stdlib.h>
#include <fp.h>
#include <math.h>
#include "fftlib.h"
#include "fftext.h"

#if macintosh
#include <timer.h>
#endif

#define NSIZES 	24		/* the number of different fft sizes to test */

#define	BIPRAND(a) (2.0/(RAND_MAX+1.0)*a-1.0)

void main(){
long 	fftSize[NSIZES] = 	/* size of FFTs, must be powers of 2 */
		{2,		4, 		8,		16, 		32, 		64, 		128, 	256,
		512, 	1024, 	2048, 	4096, 	8192, 	16384, 	32768, 	65536,
		131072, 	262144, 	524288, 	1048576, 	2097152, 	4194304, 	8388608, 	16777216};
float	*a;
const long N2 = 2;		/* the number ffts to test at each size */
long 	isize;
long 	i1;
long 	i2;
long 	TheErr;
long		N;
long		M;
float 	maxerrifft;
float 	maxerrfft;

unsigned int	randseed = 777;
int		rannum;
#if macintosh
	UnsignedWide 		TheTime1;
	Microseconds(&TheTime1);
	randseed = TheTime1.lo;
#endif

printf(" %6d  Byte Floats \n", sizeof(a[0]));
printf(" randseed = %10u\n", randseed);
for (isize = 0; isize < NSIZES; isize++){

	srand(randseed);
	N = fftSize[isize];
	printf("rffts size = %8d,  ", N);
	M = roundtol(LOG2(N));

	TheErr = 0;
	TheErr = fftInit(M);

	if(!TheErr){
		a = (float *) malloc(N2*N*sizeof(float) );
		if (a == 0) TheErr = 2;
	}

	if(!TheErr){

			/*  set up a simple test case */
		for (i1=0; i1<N2*N; i1++){
			rannum = rand();
			a[i1] = BIPRAND(rannum);
		}

			/*  first use realft from numerical recipes in C to verify riffts */
			/*  unfortunately  numerical recipes in C uses backwards time */
			/*  forward fft, so our answer comes out time reversed */
		for (i2=0; i2<N2; i2++){
			realft((a+i2*N)-1, N, 1);
		}
		riffts(a, M, N2);

		srand(randseed);
		for (i2=0; i2<N2; i2++){
			rannum = rand();
			maxerrifft = fabs(BIPRAND(rannum)-a[i2*N]);
			for (i1=1; i1<N; i1++){
				rannum = rand();
				maxerrifft = fmax(maxerrifft, fabs(BIPRAND(rannum)-a[i2*N+N-i1]));
			}
		}

		printf("maxerrifft = %6.4e,  ", maxerrifft);

			/*  now use iffts to verify ffts */
		srand(randseed);
		for (i1=0; i1<N2*N; i1++){
			rannum = rand();
			a[i1] = BIPRAND(rannum);
		}

		riffts(a, M, N2);
		rffts(a, M, N2);

		maxerrfft = 0;
		srand(randseed);
		for (i1=0; i1<N2*N; i1++){
			rannum = rand();
			maxerrfft = fmax(maxerrfft, fabs(BIPRAND(rannum)-a[i1]));
		}

		printf("maxerrfft = %6.4e\n", maxerrfft);

		fftFree();
		free(a);
		a = 0;
	}
	else{
		if(TheErr==2)	printf(" out of memory \n");
		else	printf(" error \n");
		fftFree();
	}
}
printf(" Done. \n");
return;
}
