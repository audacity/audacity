/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file 2005-2006 Christian Landone.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_TEMPOTRACK_H
#define QM_DSP_TEMPOTRACK_H


#include <stdio.h>
#include <vector>

#include "dsp/signalconditioning/DFProcess.h"
#include "maths/Correlation.h"
#include "dsp/signalconditioning/Framer.h"

struct WinThresh
{
    int pre;
    int post;
};

struct TTParams
{
    int winLength; //Analysis window length
    int lagLength; //Lag & Stride size
    int alpha; //alpha-norm parameter
    int LPOrd; // low-pass Filter order
    double* LPACoeffs; //low pass Filter den coefficients
    double* LPBCoeffs; //low pass Filter num coefficients
    WinThresh WinT;//window size in frames for adaptive thresholding [pre post]:
};


class TempoTrack  
{
public:
    TempoTrack( TTParams Params );
    virtual ~TempoTrack();

    std::vector<int> process( std::vector <double> DF,
                              std::vector <double> *tempoReturn = 0);

        
private:
    void initialise( TTParams Params );
    void deInitialise();

    int beatPredict( int FSP, double alignment, double period, int step);
    int phaseMM( double* DF, double* weighting, int winLength, double period );
    void createPhaseExtractor( double* Filter, int winLength,  double period,  int fsp, int lastBeat );
    int findMeter( double* ACF,  int len, double period );
    void constDetect( double* periodP, int currentIdx, int* flag );
    void stepDetect( double* periodP, double* periodG, int currentIdx, int* flag );
    void createCombFilter( double* Filter, int winLength, int TSig, double beatLag );
    double tempoMM( double* ACF, double* weight, int sig );
        
    int m_dataLength;
    int m_winLength;
    int m_lagLength;

    double m_rayparam;
    double m_sigma;
    double m_DFWVNnorm;

    std::vector<int>  m_beats; // Vector of detected beats

    double m_lockedTempo;

    double* m_tempoScratch;
    double* m_smoothRCF; // Smoothed Output of Comb Filterbank (m_tempoScratch)
        
    // Processing Buffers 
    double* m_rawDFFrame; // Original Detection Function Analysis Frame
    double* m_smoothDFFrame; // Smoothed Detection Function Analysis Frame
    double* m_frameACF; // AutoCorrelation of Smoothed Detection Function 

    //Low Pass Coefficients for DF Smoothing
    double* m_ACoeffs;
    double* m_BCoeffs;
        
    // Objetcs/operators declaration
    Framer m_DFFramer;
    DFProcess* m_DFConditioning;
    Correlation m_correlator;
    // Config structure for DFProcess
    DFProcConfig m_DFPParams;

    // also want to smooth m_tempoScratch 
    DFProcess* m_RCFConditioning;
    // Config structure for RCFProcess
    DFProcConfig m_RCFPParams;
};

#endif
