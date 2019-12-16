/* lpc.c -- implement LPC analysis */

#include <math.h>

#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"


void abs_max(double *x, long desde, long hasta, double *x_maxptr, long *indptr)
{
              /*  use:
	                  abs_max(s,0,10,&mimax,&miind);
	          */

	double x_max = x[desde];
	long ind = desde;
	long i;
	for(i = desde+1; i<hasta; i++)
		if (fabs(x[i]) > x_max)
		{
			x_max = fabs(x[i]);
            ind = i;
		}
	*x_maxptr = x_max;
	*indptr = ind;

}

void xcorr(double *s, double *rxx, long N)
{
	/* use:
	xcorr(s,rxx,N);
	*/
	long i,j;
	for(i=0; i < N; i++)
	{
		rxx[i] = 0.0;
		for(j=0; j < N-i; j++)
			rxx[i] += s[j]*s[j+i]; 
    }
}




// SOUND PARAMETERS
// w         Lisp vector containinig signal values
// P         number of poles
// N         length of sound

// AUTOCORRELEATION PARAMETERS
// rxx       array containing autocorrelation coefs
// max_rxx   temporal maximun value of rxx
// i_max     index of max_rxx

// LPC PARAMETERS
// alpha     array of filter coefs
// k         reflection coef
// E         residual energy
// rms1      energy of input signal (not RMS)
// rms2      residual energy = E    
// unv       voiced/unvoiced parameter = ERR = rms2/rms1

// PITCH DETECTION ALGORITHM: Implemented separately

char *lpanal_expected_flonum_vector = "expected flonum vector";
char *lpanal_insufficient_space = "insufficient space";

LVAL snd_lpanal(LVAL w, long P)
{
    
	double *s, *rxx;
	long N;
	double *alpha;
	// double *k, *E; THIS ONLY FOR VECTORIZED k AND E
	double k, E;
	double rms1; // rms2=E;
	double unv;
	double suma, alphatemp; // help variables
	
	long i,j;
	LVAL result;

	xlsave1(result);
	//// end vars /////////////

	//// allocate memory ///////
    if (!vectorp(w)) xlfail(lpanal_expected_flonum_vector);
    N = getsize(w);
	s = calloc(sizeof(double), N); //signal
    if (!s) xlfail(lpanal_insufficient_space);
    rxx = calloc(sizeof(double), N); //autocorrelation
    if (!rxx) xlfail(lpanal_insufficient_space);
	alpha = calloc(sizeof(double), P); // filter coefs
    if (!alpha) xlfail(lpanal_insufficient_space);
	//k = calloc(sizeof(double), P); // reflection coefs
	//E = calloc(sizeof(double), P); // residual energy

	//////   copy Lisp array sound data to array of double ///////
	for (i = 0; i < N; i++) {
        LVAL elem = getelement(w, i);
        if (!floatp(elem))
            xlfail(lpanal_expected_flonum_vector);
        s[i] = getflonum(elem);
    }
    /////   autocorrelation  ////////////////

	xcorr(s, rxx, N); // this may be optimized as only P autocorr factors are needed (not N)


	////////     LPC   analysis    //////////////////////////////////
    
	/// Durbin algorithm

	/// inicialization
    //for(i=0; i<P;i++)
	//	alpha[i]=k[i]=E[i]=0.0; // don't need this. Done by default.
	
	//E[0] = rxx[0] - pow(rxx[1],2)/rxx[0];	
	//k[0] = rxx[1]/rxx[0];
	//alpha[0] = k[0];
	E = rxx[0] - pow(rxx[1],2)/rxx[0]; // NO VECTORS k OR E	
	k = rxx[1]/rxx[0];                 //
	alpha[0] = k;                      //

	/// recursive solve
	for(i=1;i<P;i++)
	{
		suma=0.0;
		for(j=0;j<i;j++)
			suma += alpha[j] * rxx[i-j];
		//k[i] = (rxx[i+1]-suma)/E[i-1];
		//alpha[i]=k[i];
		k = (rxx[i+1]-suma)/E;
        alpha[i]=k;
		for(j=0; j <= ((i-1) >> 1); j++)
		{
			//alphatemp = alpha[j] - k[i] * alpha[i-j-1];
			//alpha[i-j-1] -= k[i] * alpha[j];
			//alpha[j] = alphatemp;
			alphatemp = alpha[j] - k * alpha[i-j-1];
			alpha[i-j-1] -= k * alpha[j];
			alpha[j] = alphatemp;

		}
		//E[i] = E[i-1] * (1 - pow(k[i],2));
		E *= (1 - pow(k,2));

	}

	// input signal energy = rxx[0];
	rms1 = rxx[0];

    // voiced/unvoiced
	unv= sqrt(E/rms1); 

    ///// HERE: CHECK STABILITY AND MODIFY COEFS  /////////////
    /////       not implemented

	

	// prepare output result
     result = newvector(P);
     for (i = 0; i < P; i++) setelement(result, i, cvflonum(alpha[P-i-1])); // alpoles format
     
	xlpop();	
	
	// free memory
	free(s); free(rxx); free(alpha);

	return (cons  (cvflonum(rms1),                    // input signal energy
		          cons(cvflonum(E),                   // residual energy
		          cons(cvflonum(unv),                 // ERR, voiced/unvoiced
				  cons(result,  NULL)))));           // coefs

}
