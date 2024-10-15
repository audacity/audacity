/*  A program to time real input fast fourier transform routine	*/

#include <stdio.h>
#include <stdlib.h>
#include <fp.h>
#include <math.h>
#include "fftlib.h"
#include "fftext.h"

#if macintosh
#include <timer.h>
#endif

#define NSIZES 3		/* the number of different fft sizes to time */

void main(){
float	*a;
long 	fftSize[NSIZES] = {2048, 32768, 524288};	/* size of FFTs, must be powers of 2 */
long 	fftRepeats[NSIZES] = {2000, 50, 1};		/* number of timing loops */
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

printf(" %6d  Byte Floats \n", sizeof(a[0]));
for (isize = 0; isize < NSIZES; isize++){

	N = fftSize[isize];
	printf("rffts size = %9d  ", N);
	M = roundtol(LOG2(N));
	TheErr = fftInit(M);

	if(!TheErr){
		a = (float *) malloc(N*sizeof(float));
		if (a == 0) TheErr = 2;
	}

	if(!TheErr){
				/*  set up a simple test case */
		for (i1=0; i1<N; i1++){
			a[i1] = sqrt(i1+.77777);	
		}

	#if macintosh
							/* make sure routines are in physical (not virtual) memory */
		Microseconds(&TheTime1);
		rffts(a, M, 1);
		riffts(a, M, 1);

		Microseconds(&TheTime1);

		for (i1 = 0; i1 < fftRepeats[isize]; i1++){		/* do many times for timing */
			rffts(a, M, 1);
			riffts(a, M, 1);
		}

		Microseconds(&TheTime2);


		TheTime = (double)(TheTime2.hi - TheTime1.hi) * 65536.0 * 65536.0;
		TheTime = (TheTime + (double)(TheTime2.lo - TheTime1.lo));
		printf("Ave of rffts & riffts Times = %14.1f  µs.\n", TheTime/fftRepeats[isize]/2);

	#else
		printf("start timing %12d real fft's\n", fftRepeats[isize]*2);
		for (i1=0;i1<fftRepeats[isize];i1++){		/* do many times for timing */
			rffts(a, M, 1);
			riffts(a, M, 1);
		}
		printf("end timing \n");
	#endif
		free (a);
		fftFree();
	}
	else{
		if(TheErr==2)	printf(" out of memory ");
		else	printf(" error ");
		fftFree();
	}
}
printf(" Done. \n");
return;
}
