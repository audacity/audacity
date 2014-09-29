/*  A program to test real 2d forward and inverse fast fourier transform routines	*/

#include <NR.H>	/* uses rlft3 from numerical recipes in C to verify rifft2d */
			/*change fmin in numerical recipes to fminnr to avoid conflict with fp.h */

#include <stdio.h>
#include <stdlib.h>
#include <fp.h>
#include <math.h>
#include "fftlib.h"
#include "fftext.h"
#include "fft2d.h"
#include <NRUTIL.H>	// uses ugly tensors from numerical recipes; so can call rlft3

#if macintosh
#include <timer.h>
#endif

#define NSIZES 	24		/* the number of different ffts sizes to test */

#define	BIPRAND(a) (2.0/(RAND_MAX+1.0)*a-1.0)

void main(){
long 	fftSize[NSIZES] = 	/* size of FFTs, must be powers of 2 */
		{2,		4, 		8,		16, 		32, 		64, 		128, 	256,
		512, 	1024, 	2048, 	4096, 	8192, 	16384, 	32768, 	65536,
		131072, 	262144, 	524288, 	1048576, 	2097152, 	4194304, 	8388608, 	16777216};
float	*a;
long 	N2 = 64;	/* the number of rows in 2d ffts, must be power of 2 */
long 	isize;
long 	i1;
long 	i2;
long 	TheErr;
long		N;
long		M;
long		M2;
float 	maxerrifft;
float 	maxerrfft;

float	***NRtensdata;	/* needed for rlft3 */
float	**NRmatdata;	/* needed for rlft3 */
float	*specdata;	/* needed for rlft3 */
float t1,t2;

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
	M = roundtol(LOG2(N));
	N = POW2(M);
	M2 = roundtol(LOG2(N2));
	N2 = POW2(M2);

	printf("rffts size = %6d X%6d,  ", N2, N);

	TheErr = 0;
	TheErr = fft2dInit(M2, M);

	if(!TheErr){
		NRmatdata=matrix(1,1,1,2*N2);
		specdata = &NRmatdata[1][1];
		NRtensdata=f3tensor(1,1,1,N2,1,N);	// uses ugly tensors from NRUTIL; so can call rlft3
		a = &NRtensdata[1][1][1];
		if ((a == 0)||(specdata == 0)) TheErr = 2;
	}

	if(!TheErr){

			/*  set up a simple test case */
		for (i1=0; i1<N2*N; i1++){
			rannum = rand();
			a[i1] = BIPRAND(rannum);
		}

			/*  first use rlft3 from numerical recipes in C to verify rifft2d */
			/*  unfortunately  numerical recipes in C uses backwards time and space */
			/*  forward fft, so our answer comes out time and space reversed */

		rlft3(NRtensdata, NRmatdata, 1, N2, N, 1);
		
		/* move data to my in-place order */
		a[1] = a[N2/2*N];			// pack in nyquest wavenumber point for DC freq transform
		for (i2=0; i2<N2/2; i2++){ // move transform of nyquest frequency
			a[(N2/2+i2)*N] = specdata[i2*2];
			a[(N2/2+i2)*N+1] = specdata[i2*2+1];
		}
		a[N2/2*N+1] = specdata[N2];	// pack in nyquest wavenumber point for nyquest freq transform


		rifft2d(a, M2, M);

		srand(randseed);
		rannum = rand();
		maxerrifft = fabs(BIPRAND(rannum)-a[0]);
		for (i1=1; i1<N; i1++){
			rannum = rand();
		t1=BIPRAND(rannum);
		t2=a[N-i1];
			maxerrifft = fmax(maxerrifft, fabs(BIPRAND(rannum)-a[N-i1]));
		}
		for (i2=1; i2<N2; i2++){
			rannum = rand();
			t1=BIPRAND(rannum);
			t2=a[(N2-i2)*N];
			maxerrifft = fmax(maxerrifft, fabs(BIPRAND(rannum)-a[(N2-i2)*N]));
			for (i1=1; i1<N; i1++){
				rannum = rand();
			t1=BIPRAND(rannum);
			t2=a[(N2-i2)*N+N-i1];
				maxerrifft = fmax(maxerrifft, fabs(BIPRAND(rannum)-a[(N2-i2)*N+N-i1]));
			}
		}

		printf("maxerrifft = %6.4e,  ", maxerrifft);

			/*  now use rifft2d to verify rfft2d */
		srand(randseed);
		for (i1=0; i1<N2*N; i1++){
			rannum = rand();
			a[i1] = BIPRAND(rannum);
		}

		rifft2d(a, M2, M);
		rfft2d(a, M2, M);

		maxerrfft = 0;
		srand(randseed);
		for (i1=0; i1<N2*N; i1++){
			rannum = rand();
			maxerrfft = fmax(maxerrfft, fabs(BIPRAND(rannum)-a[i1]));
		}

		printf("maxerrfft = %6.4e\n", maxerrfft);

		fft2dFree();
		free_f3tensor(NRtensdata,1,1,1,N2,1,N);
		free_matrix(NRmatdata,1,1,1,2*N2);
		a = 0;
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
