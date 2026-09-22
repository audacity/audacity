/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file copyright 2005 Nicolas Chetry, copyright 2008 QMUL.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#include <cmath>
#include <cstdlib>
#include <cstring>

#include "MFCC.h"
#include "dsp/transforms/FFT.h"
#include "base/Window.h"

MFCC::MFCC(MFCCConfig config)
{
    int i,j;

    /* Calculate at startup */
    double *freqs, *lower, *center, *upper, *triangleHeight, *fftFreqs;
  
    lowestFrequency   = 66.6666666;
    linearFilters     = 13;
    linearSpacing     = 66.66666666;
    logFilters        = 27;
    logSpacing        = 1.0711703;
  
    /* FFT and analysis window sizes */
    fftSize           = config.fftsize;
    fft               = new FFTReal(fftSize);

    totalFilters      = linearFilters + logFilters;
    logPower          = config.logpower;
  
    samplingRate      = config.FS;
  
    /* The number of cepstral componenents */
    nceps             = config.nceps;

    /* Set if user want C0 */
    WANT_C0           = (config.want_c0 ? 1 : 0);
  
    /* Allocate space for feature vector */
    if (WANT_C0 == 1) {
        ceps              = (double*)calloc(nceps+1, sizeof(double));
    } else {
        ceps              = (double*)calloc(nceps, sizeof(double));
    }
 
    /* Allocate space for local vectors */
    mfccDCTMatrix     = (double**)calloc(nceps+1, sizeof(double*));
    for (i = 0; i < nceps+1; i++) {
        mfccDCTMatrix[i]= (double*)calloc(totalFilters, sizeof(double)); 
    }

    mfccFilterWeights = (double**)calloc(totalFilters, sizeof(double*));
    for (i = 0; i < totalFilters; i++) {
        mfccFilterWeights[i] = (double*)calloc(fftSize, sizeof(double)); 
    }
    
    freqs  = (double*)calloc(totalFilters+2,sizeof(double));
    
    lower  = (double*)calloc(totalFilters,sizeof(double));
    center = (double*)calloc(totalFilters,sizeof(double));
    upper  = (double*)calloc(totalFilters,sizeof(double));
    
    triangleHeight = (double*)calloc(totalFilters,sizeof(double));
    fftFreqs       = (double*)calloc(fftSize,sizeof(double));
  
    for (i = 0; i < linearFilters; i++) {
        freqs[i] = lowestFrequency + ((double)i) * linearSpacing;
    }
  
    for (i = linearFilters; i < totalFilters+2; i++) {
        freqs[i] = freqs[linearFilters-1] * 
            pow(logSpacing, (double)(i-linearFilters+1));
    }
  
    /* Define lower, center and upper */
    memcpy(lower,  freqs,totalFilters*sizeof(double));
    memcpy(center, &freqs[1],totalFilters*sizeof(double));
    memcpy(upper,  &freqs[2],totalFilters*sizeof(double));
    
    for (i=0;i<totalFilters;i++){
        triangleHeight[i] = 2./(upper[i]-lower[i]);
    }
  
    for (i=0;i<fftSize;i++){
        fftFreqs[i] = ((double) i / ((double) fftSize ) * 
                       (double) samplingRate);
    }

    /* Build now the mccFilterWeight matrix */
    for (i=0;i<totalFilters;i++){

        for (j=0;j<fftSize;j++) {
      
            if ((fftFreqs[j] > lower[i]) && (fftFreqs[j] <= center[i])) {
          
                mfccFilterWeights[i][j] = triangleHeight[i] * 
                    (fftFreqs[j]-lower[i]) / (center[i]-lower[i]); 
          
            }
            else
            {
                mfccFilterWeights[i][j] = 0.0;
            }

            if ((fftFreqs[j]>center[i]) && (fftFreqs[j]<upper[i])) {

                mfccFilterWeights[i][j] = mfccFilterWeights[i][j]
                    + triangleHeight[i] * (upper[i]-fftFreqs[j]) 
                    / (upper[i]-center[i]);
            }
            else
            {
                mfccFilterWeights[i][j] = mfccFilterWeights[i][j] + 0.0;
            }
        }

    }

    /*
     * We calculate now mfccDCT matrix 
     * NB: +1 because of the DC component
     */

    const double pi = 3.14159265358979323846264338327950288;
  
    for (i = 0; i < nceps+1; i++) {
        for (j = 0; j < totalFilters; j++) {
            mfccDCTMatrix[i][j] = (1./sqrt((double) totalFilters / 2.))  
                * cos((double) i * ((double) j + 0.5) / (double) totalFilters * pi);
        }
    }

    for (j = 0; j < totalFilters; j++){
        mfccDCTMatrix[0][j] = (sqrt(2.)/2.) * mfccDCTMatrix[0][j];
    }
   
    /* The analysis window */
    window      = new Window<double>(config.window, fftSize);

    /* Allocate memory for the FFT */
    realOut     = (double*)calloc(fftSize, sizeof(double));
    imagOut     = (double*)calloc(fftSize, sizeof(double));

    earMag      = (double*)calloc(totalFilters, sizeof(double));
    fftMag      = (double*)calloc(fftSize/2, sizeof(double));
  
    free(freqs);
    free(lower);
    free(center);
    free(upper);
    free(triangleHeight);
    free(fftFreqs);
}

MFCC::~MFCC()
{
    int i;
  
    /* Free the structure */
    for (i = 0; i < nceps+1; i++) {
        free(mfccDCTMatrix[i]);
    }
    free(mfccDCTMatrix);
    
    for (i = 0; i < totalFilters; i++) {
        free(mfccFilterWeights[i]);
    }
    free(mfccFilterWeights);
    
    /* Free the feature vector */
    free(ceps);
    
    /* The analysis window */
    delete window;

    free(earMag);
    free(fftMag);
    
    /* Free the FFT */
    free(realOut);
    free(imagOut);

    delete fft;
}


/*
 * 
 * Extract the MFCC on the input frame 
 * 
 */ 
int MFCC::process(const double *inframe, double *outceps)
{
    double *inputData = (double *)malloc(fftSize * sizeof(double));
    for (int i = 0; i < fftSize; ++i) inputData[i] = inframe[i];

    window->cut(inputData);
  
    /* Calculate the fft on the input frame */
    fft->forward(inputData, realOut, imagOut);

    free(inputData);

    return process(realOut, imagOut, outceps);
}

int MFCC::process(const double *real, const double *imag, double *outceps)
{
    int i, j;

    for (i = 0; i < fftSize/2; ++i) {
        fftMag[i] = sqrt(real[i] * real[i] + imag[i] * imag[i]);
    }

    for (i = 0; i < totalFilters; ++i) {
        earMag[i] = 0.0;
    }

    /* Multiply by mfccFilterWeights */
    for (i = 0; i < totalFilters; i++) {
        double tmp = 0.0;
        for (j = 0; j < fftSize/2; j++) {
            tmp = tmp + (mfccFilterWeights[i][j] * fftMag[j]);
        }
        if (tmp > 0) earMag[i] = log10(tmp);
        else earMag[i] = 0.0;

        if (logPower != 1.0) {
            earMag[i] = pow(earMag[i], logPower);
        }
    }

    /*
     * 
     * Calculate now the cepstral coefficients 
     * with or without the DC component
     *
     */
  
    if (WANT_C0 == 1) {
     
        for (i = 0; i < nceps+1; i++) {
            double tmp = 0.;
            for (j = 0; j < totalFilters; j++){
                tmp = tmp + mfccDCTMatrix[i][j] * earMag[j];
            }
            outceps[i] = tmp;
        }
    } else {  
        for (i = 1; i < nceps+1; i++) {
            double tmp = 0.;
            for (j = 0; j < totalFilters; j++){
                tmp = tmp + mfccDCTMatrix[i][j] * earMag[j];
            }
            outceps[i-1] = tmp;
        }
    }
    
    return nceps;
}

