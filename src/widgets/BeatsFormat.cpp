/**********************************************************************

  Audacity: A Digital Audio Editor

  BeatsFormat.cpp

  Michael Papadopoulos

**********************************************************************/

#include "BeatsFormat.h"

void BeatsFormat::SetTickSizes(
   double units, double& major, double& minor, double &minorMinor,
   int& mDigits
) const
{
   // Check that all data is positive
   if (!(mBpm > 0 && mTimeSigUpper > 0 && mTimeSigLower > 0)) return;
   // Also check that the lower time signature is valid (power of 2)
   if(mTimeSigLower & (mTimeSigLower - 1)) return;

   int factor = std::ceil(units);
   major = (60 * mTimeSigUpper * factor) / (mBpm * ((double)mTimeSigLower / 4));

   if (units < 3 * (60/mBpm))
      minor = 60 / (mBpm * ((double)mTimeSigLower / 4));
   if (units < 1.5 * (60 / mBpm) && mTimeSigLower < 16)
      minorMinor = 60 / (mBpm * 4);
   mDigits = 0;
}

void BeatsFormat::SetLabelString(
   wxString& s, double d, double minor, int mDigits, TickType tickType
) const
{
   if (d < 0) {
      return;
   }

   double val = (mBpm * ((double)mTimeSigLower / 4) * d) / (60 * mTimeSigUpper);
   int beat = round((val - floor(val)) * mTimeSigUpper);

   if (tickType == RulerFormat::t_major) {
      s.Printf(wxT("%d"), (int)round(val + 1));
   }
   else if (tickType == RulerFormat::t_minor) {
      s.Printf(wxT("%d.%d"), (int)floor(val + 1), (int)beat);
   }
}

BeatsFormat::~BeatsFormat() = default;
