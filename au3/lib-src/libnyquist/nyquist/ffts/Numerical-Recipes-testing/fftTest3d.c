/*  A program to test 3d complex forward and inverse fast fourier transform routines	*/

#include <NR.H>	/* uses fourn from numerical recipes in C to verify ifft3d */
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
long 	isize;
long 	i1;
long 	TheErr;
long		N;
long		M;
long		N2 = 16;	/* the number of rows in the 3d fft */
long		M2;
long		N3 = 32;	/* the number of pages in the 3d fft */
long		M3;
float 	maxerrifft;
float 	maxerrfft;
unsigned long 	nn[3];

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
	M3 = roundtol(LOG2(N3));
	N3 = POW2(M3);

	printf("ffts size = %5d X%5d X%6d,  ", N3, N2, N);

	nn[0] = N3;
	nn[1] = N2;
	nn[2] = N;
	
	TheErr = fft3dInit(M3, M2, M);

	if(!TheErr){
		a1 = (Complex *) malloc(N3*N2*N*sizeof(Complex) );
		if (a1 == 0) TheErr = 2;
	}

	if(!TheErr){

			/*  set up a simple test case */
		for (i1=0; i1<N3*N2*N; i1++){
			rannum = rand();
			a1[i1].Re = BIPRAND(rannum);
			rannum = rand();
			a1[i1].Im = BIPRAND(rannum);
		}

			/*  first use fourn from numerical recipes in C to verify ifft3d */
			/*  Note their inverse fft is really the conventional forward fft */
		fourn((float *)a1-1, nn-1, 3, -1);

		ifft3d((float *)a1, M3, M2, M);

		maxerrifft = 0;
		srand(randseed);
		for (i1=0; i1<N3*N2*N; i1++){
			rannum = rand();
			maxerrifft = fmax(maxerrifft, fabs(BIPRAND(rannum)-a1[i1].Re));
			a1[i1].Re = BIPRAND(rannum);
			rannum = rand();
			maxerrifft = fmax(maxerrifft, fabs(BIPRAND(rannum)-a1[i1].Im));
			a1[i1].Im = BIPRAND(rannum);
		}

		printf("errifft = %4.3e,  ", maxerrifft);

			/*  now use iffts to verify ffts */
		ifft3d((float *)a1, M3, M2, M);
		fft3d((float *)a1, M3, M2, M);

		maxerrfft = 0;
		srand(randseed);
		for (i1=0; i1<N3*N2*N; i1++){
			rannum = rand();
			maxerrfft = fmax(maxerrfft, fabs(BIPRAND(rannum)-a1[i1].Re));
			rannum = rand();
			maxerrfft = fmax(maxerrfft, fabs(BIPRAND(rannum)-a1[i1].Im));
		}

		printf("errfft = %4.3e\n", maxerrfft);

		free(a1);
		fft3dFree();
	}
	else{
		if(TheErr==2)	printf(" out of memory \n");
		else	printf(" error \n");
		fft3dFree();
	}
}
printf(" Done. \n");
return;
}
