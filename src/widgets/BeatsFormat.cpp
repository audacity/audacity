/**********************************************************************

  Audacity: A Digital Audio Editor

  BeatsFormat.cpp

  Michael Papadopoulos

**********************************************************************/

#include "BeatsFormat.h"

void BeatsFormat::SetTickSizes(
   double units, double& mMajor, double& mMinor, double &mMinorMinor,
   int& mDigits, const std::any& data
) const
{
   const BeatsFormatData* beatsData = std::any_cast<BeatsFormatData>(&data);

   const double bpm = beatsData ? beatsData->bpm : 60;
   const int timeSigUpper = beatsData ? beatsData->timeSigUpper : 4;
   const int timeSigLower = beatsData ? beatsData->timeSigLower : 4;

   // Check that all data is positive
   wxASSERT(bpm > 0 && timeSigUpper > 0 && timeSigLower > 0);
   // Also check that the lower time signature is valid (power of 2)
   wxASSERT(!(timeSigLower & (timeSigLower - 1)));

   mMajor = (60 * timeSigUpper) / (bpm * timeSigLower);
   mMinor = 60 / (bpm * timeSigLower);
   if (timeSigLower < 16)
      mMinorMinor = 60 / (bpm * 16);
   mDigits = 0;
}

void BeatsFormat::SetLabelString(
   wxString& s, double d, double mMinor, int mDigits, TickType tickType,
   const std::any& data
) const
{
   if (tickType == RulerFormat::t_major) {
      const BeatsFormatData* beatsData = std::any_cast<BeatsFormatData>(&data);

      const double bpm = beatsData ? beatsData->bpm : 0;
      const int timeSigUpper = beatsData ? beatsData->timeSigUpper : 0;
      const int timeSigLower = beatsData ? beatsData->timeSigLower : 0;

      double val = (bpm * d * timeSigLower) / (60 * timeSigUpper);

      s.Printf(wxT("%d"), (int)round(val + 1));
   }
}

BeatsFormat::~BeatsFormat() = default;
