/*  A program to time complex forward and inverse fast fourier transform routines	*/
void four1(float data[], unsigned long nn, int isign);

#include <stdio.h>
#include <stdlib.h>
#include <fp.h>
#include <math.h>
#include "fftlib.h"
#include "fftext.h"

#if macintosh
#include <timer.h>
#endif

#define NSIZES 3		/* the number of different ffts sizes to time */

typedef  struct{
	float Re;
	float Im;
	} Complex;

void main(){
long 	fftSize[NSIZES] = {1024, 16384, 262144};	/* size of FFTs, must be powers of 2 */
long 	fftRepeats[NSIZES] = {2000, 50, 1};		/* number of timing loops */
Complex	*a;
long 	isize;
long 	i1;
long 	TheErr;
long	N;
long	M;

#if macintosh
UnsignedWide 		TheTime1;
UnsignedWide 		TheTime2;
double		TheTime;
#endif

printf(" %6d  Byte Floats \n", sizeof(a[0].Re));
for (isize = 0; isize < NSIZES; isize++){

	N = fftSize[isize];
	printf("ffts size = %7d,  ", N);
	M = roundtol(LOG2(N));

	TheErr = fftInit(M);

	if(!TheErr){
		a = (Complex *) malloc(N*sizeof(Complex) );
		if (a == 0) TheErr = 2;
	}

	if(!TheErr){

			/*  set up a simple test case */
		for (i1=0; i1<N; i1++){
			a[i1].Re = sqrt(i1+.77777);
			a[i1].Im = sqrt(i1+.22222);	
		}

	#if macintosh
							/* make sure routines are in physical (not virtual) memory */
		Microseconds(&TheTime1);
		ffts((float *)a, M, 1);
		iffts((float *)a, M, 1);

		Microseconds(&TheTime1);

		for (i1=0;i1<fftRepeats[isize];i1++){		/* do many times for timing */
			ffts((float *)a, M, 1);
			iffts((float *)a, M, 1);
		}	

		Microseconds(&TheTime2);

		TheTime = (double)(TheTime2.hi - TheTime1.hi) * 65536.0 * 65536.0;
		TheTime = (TheTime + (double)(TheTime2.lo - TheTime1.lo));
		printf("ffts time = %12.1f  µs,  a[0].Re=%6e\n", TheTime/fftRepeats[isize]/2, a[0].Re);

	#else
		printf("start timing %12d real ffts's\n", fftRepeats[isize]*2);
		for (i1=0;i1<fftRepeats[isize];i1++){		/* do many times for timing */
			ffts((float *)a, M, 1);
			iffts((float *)a, M, 1);
		}
		printf("end timing \n");
	#endif
		free(a);
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
