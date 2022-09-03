/**********************************************************************

  Audacity: A Digital Audio Editor

  RealFormat.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#include "RealFormat.h"

void RealFormat::SetTickSizes(
   double units, double& mMajor, double& mMinor, double& mMinorMinor,
   int& mDigits, const std::any& data
) const
{
   const RealFormatData* realData = std::any_cast<RealFormatData>(&data);

   const bool log = realData ? realData->log : false;

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
   if (log) {
      mDigits++;
   }
   mMinor = d;
   mMajor = d * 2.0;
}

void RealFormat::SetLabelString(
   wxString& s, double d, double mMinor, int mDigits, bool useMajor,
   const std::any& data
) const
{
   if (mMinor >= 1.0)
      s.Printf(wxT("%d"), (int)floor(d + 0.5));
   else {
      s.Printf(wxString::Format(wxT("%%.%df"), mDigits), d);
   }
}

RealFormat::~RealFormat() = default;
