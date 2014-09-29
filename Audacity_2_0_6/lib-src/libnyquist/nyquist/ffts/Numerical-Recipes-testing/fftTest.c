/*  A program to test complex forward and inverse fast fourier transform routines	*/

#include <NR.H>	/* uses four1 from numerical recipes in C to verify iffts */
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

typedef  struct{
	float Re;
	float Im;
	} Complex;

void main(){
long 	fftSize[NSIZES] = 	/* size of FFTs, must be powers of 2 */
		{2, 		4, 		8, 		16, 		32, 		64, 		128, 	256,
		512, 	1024, 	2048, 	4096, 	8192, 	16384, 	32768, 	65536,
		131072, 	262144, 	524288, 	1048576, 	2097152, 	4194304, 	8388608, 	16777216};
Complex	*a1;
const long N2 = 2;		/* the number ffts to test at each size */
long 	isize;
long 	i1;
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

printf(" %6d  Byte Floats \n", sizeof(a1[0].Re));
printf(" randseed = %10u\n", randseed);
for (isize = 0; isize < NSIZES; isize++){

	srand(randseed);
	N = fftSize[isize];
	printf("ffts size = %8d,  ", N);
	M = roundtol(LOG2(N));

	TheErr = fftInit(M);

	if(!TheErr){
		a1 = (Complex *) malloc( N2*N*sizeof(Complex) );
		if (a1 == 0) TheErr = 2;
	}

	if(!TheErr){

			/*  set up a1 simple test case */
		for (i1=0; i1<N2*N; i1++){
			rannum = rand();
			a1[i1].Re = BIPRAND(rannum);
			rannum = rand();
			a1[i1].Im = BIPRAND(rannum);
		}

			/*  first use four1 from numerical recipes in C to verify iffts */
			/*  Note their inverse fft is really the conventional forward fft */
		for (i1=0; i1<N2; i1++){
			four1((float *)(a1+i1*N)-1, N, -1);
		}
		iffts((float *)a1, M, N2);

		maxerrifft = 0;
		srand(randseed);
		for (i1=0; i1<N2*N; i1++){
			rannum = rand();
			maxerrifft = fmax(maxerrifft, fabs(BIPRAND(rannum)-a1[i1].Re));
			a1[i1].Re = BIPRAND(rannum);
			rannum = rand();
			maxerrifft = fmax(maxerrifft, fabs(BIPRAND(rannum)-a1[i1].Im));
			a1[i1].Im = BIPRAND(rannum);
		}

		printf("maxerrifft = %6.4e,  ", maxerrifft);

			/*  now use iffts to verify ffts */
		iffts((float *)a1, M, N2);
		ffts((float *)a1, M, N2);

		maxerrfft = 0;
		srand(randseed);
		for (i1=0; i1<N2*N; i1++){
			rannum = rand();
			maxerrfft = fmax(maxerrfft, fabs(BIPRAND(rannum)-a1[i1].Re));
			rannum = rand();
			maxerrfft = fmax(maxerrfft, fabs(BIPRAND(rannum)-a1[i1].Im));
		}

		printf("maxerrfft = %6.4e\n", maxerrfft);

		free(a1);
		fftFree();
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
