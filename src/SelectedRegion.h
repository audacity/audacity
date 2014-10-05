/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectedRegion.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SELECTEDREGION__
#define __AUDACITY_SELECTEDREGION__

#include "Audacity.h"

class AUDACITY_DLL_API SelectedRegion {

   // Maintains the invariant:  t1() >= t0()

public:
   SelectedRegion()
      : mT0(0.0)
      , mT1(0.0)
   {}

   SelectedRegion(double t0, double t1)
      : mT0(t0)
      , mT1(t1)
   { ensureOrdering(); }

   explicit
   SelectedRegion(const SelectedRegion &x)
      : mT0(x.mT0)
      , mT1(x.mT1)
   {}

   SelectedRegion& operator=(const SelectedRegion& x)
   {
      if (this != &x) {
         mT0 = x.mT0;
         mT1 = x.mT1;
      }
      return *this;
   }

   double t0() const { return mT0; }
   double t1() const { return mT1; }
   double duration() const { return mT1 - mT0; }
   bool isPoint() const { return mT1 <= mT0; }

   // PRL: to do: more integrity checks

   // Returns true iff the bounds got swapped
   bool setT0(double t, bool maySwap = true) {
      mT0 = t;
      if (maySwap)
         return ensureOrdering();
      else {
         if (mT1 < mT0)
            mT1 = mT0;
         return false;
      }
   }

   // Returns true iff the bounds got swapped
   bool setT1(double t, bool maySwap = true) {
      mT1 = t;
      if (maySwap)
         return ensureOrdering();
      else {
         if (mT1 < mT0)
            mT0 = mT1;
         return false;
      }
   }

   // Returns true iff the bounds got swapped
   bool setTimes(double t0, double t1) {
      mT0 = t0;
      mT1 = t1;
      return ensureOrdering();
   }

   // Returns true iff the bounds got swapped
   bool moveT0(double delta, bool maySwap = true) {
      return setT0(mT0 + delta, maySwap);
   }

   // Returns true iff the bounds got swapped
   bool moveT1(double delta, bool maySwap = true) {
      return setT1(mT1 + delta, maySwap);
   }

   void move(double delta) {
      mT0 += delta;
      mT1 += delta;
   }

   void collapseToT0() { mT1 = mT0; }

   void collapseToT1() { mT0 = mT1; }

private:
   bool ensureOrdering() 
   {
      if (mT1 < mT0) {
         const double t = mT1;
         mT1 = mT0;
         mT0 = t;
         return true;
      }
      else
         return false;
   }

   double mT0;
   double mT1;

};

#endif
