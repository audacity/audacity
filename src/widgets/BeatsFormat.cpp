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
   if (!(bpm > 0 && timeSigUpper > 0 && timeSigLower > 0)) return;
   // Also check that the lower time signature is valid (power of 2)
   if(timeSigLower & (timeSigLower - 1)) return;

   int factor = std::ceil(units);
   mMajor = (60 * timeSigUpper * factor) / (bpm * ((double)timeSigLower / 4));

   if (units < 3 * (60/bpm))
      mMinor = 60 / (bpm * ((double)timeSigLower / 4));
   if (units < 1.5 * (60 / bpm) && timeSigLower < 16)
      mMinorMinor = 60 / (bpm * 4);
   mDigits = 0;
}

void BeatsFormat::SetLabelString(
   wxString& s, double d, double mMinor, int mDigits, TickType tickType,
   const std::any& data
) const
{
   if (d < 0) {
      return;
   }
   const BeatsFormatData* beatsData = std::any_cast<BeatsFormatData>(&data);

   const double bpm = beatsData ? beatsData->bpm : 0;
   const int timeSigUpper = beatsData ? beatsData->timeSigUpper : 0;
   const int timeSigLower = beatsData ? beatsData->timeSigLower : 0;

   double val = (bpm * ((double)timeSigLower / 4) * d) / (60 * timeSigUpper);
   int beat = round((val - floor(val)) * timeSigUpper);

   if (tickType == RulerFormat::t_major) {
      s.Printf(wxT("%d"), (int)round(val + 1));
   }
   else if (tickType == RulerFormat::t_minor) {
      s.Printf(wxT("%d.%d"), (int)floor(val + 1), (int)beat);
   }
}

BeatsFormat::~BeatsFormat() = default;
