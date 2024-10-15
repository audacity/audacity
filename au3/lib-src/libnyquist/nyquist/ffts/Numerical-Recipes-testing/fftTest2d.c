/*  A program to test 2d complex forward and inverse fast fourier transform routines	*/

#include <NR.H>	/* uses fourn from numerical recipes in C to verify ifft2d */
			/*change fmin in numerical recipes to fminnr to avoid conflict with fp.h */

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

#define NSIZES 	24		/* the number of different ffts col sizes to test */

#define	BIPRAND(a) (2.0/(RAND_MAX+1.0)*a-1.0)
//#define	BIPRAND(a) round(100*(2.0/(RAND_MAX+1.0)*a-1.0))
typedef  struct{
	float Re;
	float Im;
	} Complex;

void main(){
long 	fftSize[NSIZES] = 	/* size of FFTs cols, must be powers of 2 */
		{2, 		4, 		8, 		16, 		32, 		64, 		128, 	256,
		512, 	1024, 	2048, 	4096, 	8192, 	16384, 	32768, 	65536,
		131072, 	262144, 	524288, 	1048576, 	2097152, 	4194304, 	8388608, 	16777216};
Complex	*a1;
long	N2 = 64;	/* the number of rows in the 2d fft */
long 	isize;
long 	i1;
long 	TheErr;
long	N;
long	M;
long	M2;
float 	maxerrifft;
float 	maxerrfft;
unsigned long 	nn[2];

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
	M = roundtol(LOG2(N));
	N = POW2(M);
	M2 = roundtol(LOG2(N2));
	N2 = POW2(M2);

	printf("ffts size = %6d X%6d,  ", N2, N);

	nn[0] = N2;
	nn[1] = N;
	
	TheErr = fft2dInit(M2, M);

	if(!TheErr){
		a1 = (Complex *) malloc(N2*N*sizeof(Complex) );
		if (a1 == 0) TheErr = 2;
	}

	if(!TheErr){

			/*  set up a simple test case */
		for (i1=0; i1<N2*N; i1++){
			rannum = rand();
			a1[i1].Re = BIPRAND(rannum);
			rannum = rand();
			a1[i1].Im = BIPRAND(rannum);
		}

			/*  first use fourn from numerical recipes in C to verify ifft2d */
			/*  Note their inverse fft is really the conventional forward fft */
		fourn((float *)a1-1, nn-1, 2, -1);

		ifft2d((float *)a1, M2, M);

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
		ifft2d((float *)a1, M2, M);
		fft2d((float *)a1, M2, M);

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
		fft2dFree();
	}
	else{
		if(TheErr==2)	printf(" out of memory \n");
		else	printf(" error \n");
		fft2dFree();
	}
}
printf(" Done. \n");
return;
}
