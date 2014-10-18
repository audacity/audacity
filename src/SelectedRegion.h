/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectedRegion.h

  Dominic Mazzoni

*******************************************************************//**

\class SelectedRegion
\brief Defines a selected portion of a project

  This includes starting and ending times, and other optional information
  such as a frequency range, but not the set of selected tracks.

  Maintains the invariants that ending time is not less than starting time
  and that starting and ending frequencies, when both defined, are also
  correctly ordered.

*//****************************************************************//**

**********************************************************************/

#ifndef __AUDACITY_SELECTEDREGION__
#define __AUDACITY_SELECTEDREGION__

#include "Audacity.h"
#include "Experimental.h"

class AUDACITY_DLL_API SelectedRegion {

   // Maintains the invariant:  t1() >= t0()

public:

   static const int UndefinedFrequency = -1;

   SelectedRegion()
      : mT0(0.0)
      , mT1(0.0)
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      , mF0(UndefinedFrequency)
      , mF1(UndefinedFrequency)
#endif
   {}

   SelectedRegion(double t0, double t1)
      : mT0(t0)
      , mT1(t1)
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      , mF0(UndefinedFrequency)
      , mF1(UndefinedFrequency)
#endif
   { ensureOrdering(); }


   // LLL: 2014/10/6
   // Removed "explicit" until we drop OSX PPC support and upgrade to a newer
   // compiler.
   //
   // explicit
   SelectedRegion(const SelectedRegion &x)
      : mT0(x.mT0)
      , mT1(x.mT1)
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      , mF0(x.mF0)
      , mF1(x.mF1)
#endif
   {}

   SelectedRegion& operator=(const SelectedRegion& x)
   {
      if (this != &x) {
         mT0 = x.mT0;
         mT1 = x.mT1;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
         mF0 = x.mF0;
         mF1 = x.mF1;
#endif
      }
      return *this;
   }

   // Accessors

   double t0() const { return mT0; }
   double t1() const { return mT1; }
   double duration() const { return mT1 - mT0; }
   bool isPoint() const { return mT1 <= mT0; }

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   double f0() const { return mF0; }
   double f1() const { return mF1; }
   double fc() const {
      if (mF0 == UndefinedFrequency ||
          mF1 == UndefinedFrequency)
          return UndefinedFrequency;
      else
         return sqrt(mF0 * mF1);
   };
#endif

   // Mutators
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

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   // Returns true iff the bounds got swapped
   bool setF0(double f) {
      mF0 = f;
      return ensureFrequencyOrdering();
   }

   // Returns true iff the bounds got swapped
   bool setF1(double f) {
      mF1 = f;
      return ensureFrequencyOrdering();
   }

   // Returns true iff the bounds got swapped
   bool setFrequencies(double f0, double f1)
   {
      mF0 = f0;
      mF1 = f1;
      return ensureFrequencyOrdering();
   }
#endif

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

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   bool ensureFrequencyOrdering()
   {
      if (mF1 < 0)
         mF1 = UndefinedFrequency;
      if (mF0 < 0)
         mF0 = UndefinedFrequency;

      if (mF0 != UndefinedFrequency &&
          mF1 != UndefinedFrequency &&
          mF1 < mF0) {
         const double t = mF1;
         mF1 = mF0;
         mF0 = t;
         return true;
      }
      else
         return false;
   }
#endif

   double mT0;
   double mT1;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   double mF0; // low frequency
   double mF1; // high frequency
#endif

};

#endif
