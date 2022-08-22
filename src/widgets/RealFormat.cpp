/**********************************************************************

  Audacity: A Digital Audio Editor

  RealFormat.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#include "RealFormat.h"

const RealFormat &RealFormat::LinearInstance()
{
   static RealFormat instance{ false };
   return instance;
}

const RealFormat &RealFormat::LogInstance()
{
   static RealFormat instance{ true };
   return instance;
}

void RealFormat::SetTickSizes(
   double units, double& mMajor, double& mMinor, int& mDigits
) const
{
   double d = 0.000001;
   mDigits = 6;
   for (;;) {
      if (units < d) {
         mMinor = d;
         mMajor = d * 5.0;
         return;
      }
      d *= 5.0;
      if (units < d) {
         mMinor = d;
         mMajor = d * 2.0;
         return;
      }
      d *= 2.0;
      mDigits--;
      // More than 10 digit numbers?  Something is badly wrong.
      // Probably units is coming in with too high a value.
      wxASSERT(mDigits >= -10);
      if (mDigits < -10)
         break;
   }
   if (mLog) {
      mDigits++;
   }
   mMinor = d;
   mMajor = d * 2.0;
}

void RealFormat::SetLabelString(
   wxString& s, double d, double mMinor, int mDigits, bool useMajor
) const
{
   // Replace -0 with 0
   if (d < 0.0 && (d + mMinor > 0.0) && !mLog)
      d = 0.0;
   if (mMinor >= 1.0)
      s.Printf(wxT("%d"), (int)floor(d + 0.5));
   else {
      s.Printf(wxString::Format(wxT("%%.%df"), mDigits), d);
   }
}

RealFormat::~RealFormat() = default;
