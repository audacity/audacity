/**********************************************************************

  Audacity: A Digital Audio Editor

  IntFormat.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#include "IntFormat.h"

void IntFormat::SetTickSizes(
   double units, double& mMajor, double& mMinor, int& mDigits,
   const std::any& data
) const
{
   double d = 1.0;
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
   }
}

void IntFormat::SetLabelString(
   wxString& s, double d, double mMinor, int mDigits, bool useMajor,
   const std::any& data
) const
{
   s.Printf(wxT("%d"), (int)floor(d + 0.5));
}

IntFormat::~IntFormat() = default;
