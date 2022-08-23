/**********************************************************************

  Audacity: A Digital Audio Editor

  BeatsFormat.cpp

  Michael Papadopoulos

**********************************************************************/

#include "BeatsFormat.h"

void BeatsFormat::SetTickSizes(
   double units, double& mMajor, double& mMinor, double &mMinorMinor,
   int& mDigits
) const
{
   // Check that all data is positive
   wxASSERT(mBpm > 0 && mTimeSigUpper > 0 && mTimeSigLower > 0);
   // Also check that the lower time signature is valid (power of 2)
   wxASSERT(!(mTimeSigLower & (mTimeSigLower - 1)));

   mMajor = (60 * mTimeSigUpper) / (mBpm * mTimeSigLower);
   mMinor = 60 / (mBpm * mTimeSigLower);
   if (mTimeSigLower < 16)
      mMinorMinor = 60 / (mBpm * 16);
   mDigits = 0;
}

void BeatsFormat::SetLabelString(
   wxString& s, double d, double mMinor, int mDigits, TickType tickType
) const
{
   if (tickType == RulerFormat::t_major) {
      double val = (mBpm * d * mTimeSigLower) / (60 * mTimeSigUpper);

      s.Printf(wxT("%d"), (int)round(val + 1));
   }
}

BeatsFormat::~BeatsFormat() = default;
