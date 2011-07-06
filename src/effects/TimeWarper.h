/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file TimeWarper.h
\brief Contains declarations for TimeWarper, IdentityTimeWarper,
ShiftTimeWarper, LinearTimeWarper, LogarithmicTimeWarper classes

\class TimeWarper
\brief Transforms one point in time to another point. For example, a time
stretching effect might use one to keep track of what happens to labels and
split points in the input.

\class IdentityTimeWarper
\brief No change to time at all

\class ShiftTimeWarper
\brief Behaves like another, given TimeWarper, except shifted by a fixed amount

\class LinearTimeWarper
\brief Linear scaling, initialised by giving two points on the line

\class LogarithmicTimeWarper
\brief TimeScale - rate varies linearly, so time changes logarithmically.

\class StepTimeWarper
\brief Like identity but with a jump

\class RegionTimeWarper
\brief No change before the specified region; during the region, warp according
to the given warper; after the region, constant shift so as to match at the end
of the warped region.

*//*******************************************************************/

#ifndef __TIMEWARPER__
#define __TIMEWARPER__

class TimeWarper
{
public:
   virtual ~TimeWarper() { }
   virtual double Warp(double originalTime) const = 0;
};

class IdentityTimeWarper : public TimeWarper
{
public:
   virtual double Warp(double originalTime) const;
};

class ShiftTimeWarper : public TimeWarper
{
private:
   TimeWarper *mWarper;
   double mShift;
public:
   ShiftTimeWarper(TimeWarper *warper, double shiftAmount)
      : mWarper(warper), mShift(shiftAmount) { }
   virtual ~ShiftTimeWarper()
   { delete mWarper; }
   virtual double Warp(double originalTime) const;
};

class LinearTimeWarper : public TimeWarper
{
private:
   double mScale;
   double mShift;
public:
   LinearTimeWarper(double tBefore0, double tAfter0,
                    double tBefore1, double tAfter1)
      : mScale((tAfter1 - tAfter0)/(tBefore1 - tBefore0)),
        mShift(tAfter0 - mScale*tBefore0)
   { }
   virtual double Warp(double originalTime) const;
};

class LogarithmicTimeWarper : public TimeWarper
{
private:
   LinearTimeWarper mRateWarper;
   double mRStart;
   double mTStart;
   double mScale;
public:
   LogarithmicTimeWarper(double tStart, double tEnd,
                         double rStart, double rEnd);
   virtual double Warp(double originalTime) const;
};

class StepTimeWarper : public TimeWarper
{
private:
   double mTStep;
   double mOffset;
public:
   StepTimeWarper(double tStep, double offset);
   virtual double Warp(double originalTime) const;
};


// Note: this assumes that tStart is a fixed point of warper->warp()
class RegionTimeWarper : public TimeWarper
{
private:
   TimeWarper *mWarper;
   double mTStart;
   double mTEnd;
   double mOffset;
public:
   RegionTimeWarper(double tStart, double tEnd, TimeWarper *warper)
      : mWarper(warper), mTStart(tStart), mTEnd(tEnd),
         mOffset(mWarper->Warp(mTEnd)-mTEnd)
   { }
   virtual ~RegionTimeWarper()
   { delete mWarper; }
   virtual double Warp(double originalTime) const
   {
      if (originalTime < mTStart)
      {
         return originalTime;
      } else if (originalTime < mTEnd)
      {
         return mWarper->Warp(originalTime);
      } else
      {
         return mOffset + originalTime;
      }
   }
};

#endif /* End of include guard: __TIMEWARPER__ */

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
