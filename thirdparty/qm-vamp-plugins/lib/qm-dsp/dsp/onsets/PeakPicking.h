/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file 2005-2006 Christian Landone.

    Modifications:

    - delta threshold
    Description: add delta threshold used as offset in the smoothed
    detection function
    Author: Mathieu Barthet
    Date: June 2010

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_PEAKPICKING_H
#define QM_DSP_PEAKPICKING_H

#include "maths/MathUtilities.h"
#include "maths/MathAliases.h"
#include "dsp/signalconditioning/DFProcess.h"


struct PPWinThresh
{
    int pre;
    int post;

    PPWinThresh(int x, int y) :
        pre(x),
        post(y)
    {
    }
};

struct QFitThresh
{
    double a;
    double b;
    double c;

    QFitThresh(double x, double y, double z) :
        a(x),
        b(y),
        c(z)
    {
    }
};

struct PPickParams
{
    int length; // detection function length
    double tau; // time resolution of the detection function
    int alpha; // alpha-norm parameter
    double cutoff;// low-pass filter cutoff freq
    int LPOrd; // low-pass filter order
    double* LPACoeffs; // low-pass filter denominator coefficients
    double* LPBCoeffs; // low-pass filter numerator coefficients
    PPWinThresh WinT;// window size in frames for adaptive thresholding [pre post]:
    QFitThresh QuadThresh;
    float delta; // delta threshold used as an offset when computing the smoothed detection function

    PPickParams() :
        length(0),
        tau(0),
        alpha(0),
        cutoff(0),
        LPOrd(0),
        LPACoeffs(NULL),
        LPBCoeffs(NULL),
        WinT(0,0),
        QuadThresh(0,0,0),
        delta(0)
    {
    }
};

class PeakPicking  
{
public:
    PeakPicking( PPickParams Config );
    virtual ~PeakPicking();
        
    void process( double* src, int len, std::vector<int> &onsets  );

private:
    void initialise( PPickParams Config  );
    void deInitialise();
    int  quadEval( std::vector<double> &src, std::vector<int> &idx );
        
    DFProcConfig m_DFProcessingParams;

    int m_DFLength ;
    double Qfilta ;
    double Qfiltb;
    double Qfiltc;

    double* m_workBuffer;
        
    DFProcess*  m_DFSmoothing;
};

#endif
