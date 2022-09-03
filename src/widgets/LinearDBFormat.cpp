/**********************************************************************

  Audacity: A Digital Audio Editor

  LinearDBFormat.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#include "LinearDBFormat.h"

const LinearDBFormat &::LinearDBFormat::Instance()
{
   static LinearDBFormat instance;
   return instance;
}

void LinearDBFormat::SetTickSizes(
   double units, double& mMajor, double& mMinor, double& mMinorMinor,
   int& mDigits
) const
{
   if (units < 0.001) {
      mMinor = 0.001;
      mMajor = 0.005;
      return;
   }
   if (units < 0.01) {
      mMinor = 0.01;
      mMajor = 0.05;
      return;
   }
   if (units < 0.1) {
      mMinor = 0.1;
      mMajor = 0.5;
      return;
   }
   if (units < 1.0) {
      mMinor = 1.0;
      mMajor = 6.0;
      return;
   }
   if (units < 3.0) {
      mMinor = 3.0;
      mMajor = 12.0;
      return;
   }
   if (units < 6.0) {
      mMinor = 6.0;
      mMajor = 24.0;
      return;
   }
   if (units < 12.0) {
      mMinor = 12.0;
      mMajor = 48.0;
      return;
   }
   if (units < 24.0) {
      mMinor = 24.0;
      mMajor = 96.0;
      return;
   }
   double d = 20.0;
   for (;;) {
      if (units < d) {
         mMinor = d;
         mMajor = d * 5.0;
         return;
      }
      d *= 5.0;
      if (units < d) {
         mMinor = d;
         mMajor = d * 5.0;
         return;
      }
      d *= 2.0;
   }
}

void LinearDBFormat::SetLabelString(
   wxString& s, double d, double mMinor, int mDigits, bool useMajor
) const
{
   // Replace -0 with 0
   if (d < 0.0 && (d + mMinor > 0.0))
      d = 0.0;
   if (mMinor >= 1.0)
      s.Printf(wxT("%d"), (int)floor(d + 0.5));
   else {
      int precision = -log10(mMinor);
      s.Printf(wxT("%.*f"), precision, d);
   }
}

LinearDBFormat::~LinearDBFormat() = default;
