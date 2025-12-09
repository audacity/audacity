/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 or later - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file TimeWarper.cpp
\brief Contains definitions for IdentityTimeWarper, ShiftTimeWarper,
LinearTimeWarper, LogarithmicTimeWarper, QuadraticTimeWarper,
Geometric TimeWarper classes

*//*******************************************************************/

#include "TimeWarper.h"

#include <algorithm>
#include <math.h>
#include <wx/debug.h>

TimeWarper::~TimeWarper() = default;

double IdentityTimeWarper::Warp(double originalTime) const
{
    return originalTime;
}

double ShiftTimeWarper::Warp(double originalTime) const
{
    return mWarper->Warp(originalTime + mShift);
}

double LinearTimeWarper::Warp(double originalTime) const
{
    return originalTime * mScale + mShift;
}

double LinearInputRateTimeWarper::Warp(double originalTime) const
{
    double rate = mRateWarper.Warp(originalTime);
    return mTStart + mScale * log(rate / mRStart);
}

LinearInputRateTimeWarper::LinearInputRateTimeWarper(double tStart, double tEnd,
                                                     double rStart, double rEnd)
    : mRateWarper(tStart, rStart, tEnd, rEnd), mRStart(rStart),
    mTStart(tStart), mScale((tEnd - tStart) / (rEnd - rStart))
{
    wxASSERT(mRStart != 0.0);
    wxASSERT(tStart < tEnd);
}

double LinearOutputRateTimeWarper::Warp(double originalTime) const
{
    double scaledTime = mTimeWarper.Warp(originalTime);
    return mTStart + mScale * (sqrt(mC1 + scaledTime * mC2) - mRStart);
}

LinearOutputRateTimeWarper::LinearOutputRateTimeWarper(double tStart, double tEnd,
                                                       double rStart, double rEnd)
    : mTimeWarper(tStart, 0.0, tEnd, 1.0),
    mRStart(rStart), mTStart(tStart),
    mScale(2.0 * (tEnd - tStart) / (rEnd * rEnd - rStart * rStart)),
    mC1(rStart * rStart), mC2(rEnd * rEnd - rStart * rStart)
{
    wxASSERT(rStart != rEnd);
    wxASSERT(rStart > 0.0);
    wxASSERT(rEnd > 0.0);
    wxASSERT(tStart < tEnd);
}

double LinearInputStretchTimeWarper::Warp(double originalTime) const
{
    double scaledTime = mTimeWarper.Warp(originalTime);
    return mTStart + mC1 * scaledTime * (1.0 + mC2 * scaledTime);
}

LinearInputStretchTimeWarper::LinearInputStretchTimeWarper(double tStart, double tEnd,
                                                           double rStart, double rEnd)
    : mTimeWarper(tStart, 0.0, tEnd, 1.0), mTStart(tStart),
    mC1((tEnd - tStart) / rStart), mC2(0.5 * (rStart / rEnd - 1.0))
{
    wxASSERT(rStart > 0.0);
    wxASSERT(rEnd > 0.0);
    wxASSERT(tStart < tEnd);
}

double LinearOutputStretchTimeWarper::Warp(double originalTime) const
{
    double scaledTime = mTimeWarper.Warp(originalTime);
    return mTStart + mC1 * (pow(mC2, scaledTime) - 1.0);
}

LinearOutputStretchTimeWarper::LinearOutputStretchTimeWarper(double tStart, double tEnd,
                                                             double rStart, double rEnd)
    : mTimeWarper(tStart, 0.0, tEnd, 1.0), mTStart(tStart),
    mC1((tEnd - tStart) / (rStart * log(rStart / rEnd))), mC2(rStart / rEnd)
{
    wxASSERT(rStart != rEnd);
    wxASSERT(rStart > 0.0);
    wxASSERT(rEnd > 0.0);
    wxASSERT(tStart < tEnd);
}

double GeometricInputTimeWarper::Warp(double originalTime) const
{
    double scaledTime = mTimeWarper.Warp(originalTime);
    return mTStart + mScale * (pow(mRatio, scaledTime) - 1.0);
}

GeometricInputTimeWarper::GeometricInputTimeWarper(double tStart, double tEnd,
                                                   double rStart, double rEnd)
    : mTimeWarper(tStart, 0.0, tEnd, 1.0), mTStart(tStart),
    mScale((tEnd - tStart) / (log(rStart / rEnd) * rStart)), mRatio(rStart / rEnd)
{
    wxASSERT(rStart != rEnd);
    wxASSERT(rStart > 0.0);
    wxASSERT(rEnd > 0.0);
    wxASSERT(tStart < tEnd);
}

double GeometricOutputTimeWarper::Warp(double originalTime) const
{
    double scaledTime = mTimeWarper.Warp(originalTime);
    return mTStart + mScale * log1p(mC0 * scaledTime);
}

GeometricOutputTimeWarper::GeometricOutputTimeWarper(double tStart, double tEnd,
                                                     double rStart, double rEnd)
    : mTimeWarper(tStart, 0.0, tEnd, 1.0), mTStart(tStart),
    mScale((tEnd - tStart) / (rEnd - rStart)), mC0((rEnd - rStart) / rStart)
{
    wxASSERT(rStart > 0.0);
    wxASSERT(rEnd > 0.0);
    wxASSERT(tStart < tEnd);
}

PasteTimeWarper::PasteTimeWarper(double oldT1, double newT1)
    : mOldT1{oldT1}, mNewT1{newT1}
{ }

double PasteTimeWarper::Warp(double originalTime) const
{
    if (originalTime < mOldT1) {
        return std::min(originalTime, mNewT1);
    } else {
        return originalTime + mNewT1 - mOldT1;
    }
}
