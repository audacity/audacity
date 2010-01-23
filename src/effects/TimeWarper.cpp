/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file TimeWarper.cpp
\brief Contains definitions for IdentityTimeWarper, ShiftTimeWarper,
LinearTimeWarper, LogarithmicTimeWarper classes

*//*******************************************************************/

#include <wx/string.h>
#include "TimeWarper.h"
#include <math.h>

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
   return originalTime*mScale + mShift;
}

double LogarithmicTimeWarper::Warp(double originalTime) const
{
   double rate = mRateWarper.Warp(originalTime);
   return mTStart + mScale*log(rate/mRStart);
}

LogarithmicTimeWarper::LogarithmicTimeWarper(double tStart, double tEnd,
                                             double rStart, double rEnd)
: mRateWarper(tStart, rStart, tEnd, rEnd), mRStart(rStart),
  mTStart(tStart), mScale((tEnd-tStart)/(rEnd-rStart))
{
   wxASSERT(mRStart != 0.0);
   wxASSERT(tStart < tEnd);
}

StepTimeWarper::StepTimeWarper(double tStep, double offset)
: mTStep(tStep), mOffset(offset)
{ }

double StepTimeWarper::Warp(double originalTime) const
{
   return originalTime + ((originalTime > mTStep) ? mOffset : 0.0);
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: TBD
