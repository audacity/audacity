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

   if (units < .05 * (60 / mBpm))
   {
      // measures
      major = (60 * mTimeSigUpper) / (mBpm * ((double)mTimeSigLower / 4));
      // sixteenth notes (label every quarter note)
      minor = 60 / (mBpm * ((double)mTimeSigLower));
      // sixtyfourth notes
      minorMinor = 60 / (mBpm * ((double)mTimeSigLower * 4));
   }
   else if (units < .1 * (60 / mBpm))
   {
      // measures
      major = (60 * mTimeSigUpper) / (mBpm * ((double)mTimeSigLower / 4));
      // eigth notes (label every quarter note)
      minor = 60 / (mBpm * ((double)mTimeSigLower / 2));
      // thirtysecondth notes
      minorMinor = 60 / (mBpm * ((double)mTimeSigLower * 2));
   }
   else if (units < .4 * (60 / mBpm))
   {
      // measures
      major = (60 * mTimeSigUpper) / (mBpm * ((double)mTimeSigLower / 4));
      // eigth notes (label every quarter note)
      minor = 60 / (mBpm * ((double)mTimeSigLower / 2));
      // sixteenth notes
      minorMinor = 60 / (mBpm * ((double)mTimeSigLower));
   }
   else if (units < .8 * (60 / mBpm))
   {
      // measures
      major = (60 * mTimeSigUpper) / (mBpm * ((double)mTimeSigLower / 4));
      // quarter notes
      minor = 60 / (mBpm * ((double)mTimeSigLower / 4));
      // sixteenth notes
      minorMinor = 60 / (mBpm * ((double)mTimeSigLower));
   }
   else if (units < 4 * (60 / mBpm))
   {
      // measures
      major = (60 * mTimeSigUpper) / (mBpm * ((double)mTimeSigLower / 4));
      // quarter notes
      minorMinor = 60 / (mBpm * ((double)mTimeSigLower / 4));
   }
   else {
      // Introduce a scaling factor so space is maintained between
      // measures as the units increase
      int factor = std::ceil(units / 4);
      major = (60 * mTimeSigUpper * factor) / (mBpm * ((double)mTimeSigLower / 8));
      minor = (60 * factor) / (mBpm * ((double)mTimeSigLower / 16));
      minorMinor = (60 * factor) / (mBpm * ((double)mTimeSigLower / 4));
   }

   mDigits = 0;
}

void BeatsFormat::SetLabelString(
   wxString& s, double d, double units, double minor, int mDigits, TickType tickType
) const
{
   if (d < 0) {
      return;
   }

   double val = (mBpm * ((double)mTimeSigLower / 4) * d) / (60 * mTimeSigUpper);
   double beatApprox = (val - floor(val)) * mTimeSigUpper + 1;
   int beat = round(beatApprox);

   // Don't add decimal if it's a major tick or is on the beat
   // Segment by distance with units
   if (units < .4 * (60 / mBpm))
   {
      if (tickType == RulerFormat::t_major) {
         s.Printf(wxT("%d"), (int)round(val + 1));
      }
      else if (tickType == RulerFormat::t_minor && abs(beat - beatApprox) < 1.0e-5f) {
         s.Printf(wxT("%d.%d"), (int)floor(val + 1), (int)beat);
      }
   }
   else if (units < .8 * (60 / mBpm))
   {
      if (tickType == RulerFormat::t_major || tickType == RulerFormat::t_minor && beat == 1) {
         s.Printf(wxT("%d"), (int)round(val + 1));
      }
      else if (tickType == RulerFormat::t_minor) {
         s.Printf(wxT("%d.%d"), (int)floor(val + 1), (int)beat);
      }
   }
   else {
      if (tickType == RulerFormat::t_major) {
         s.Printf(wxT("%d"), (int)round(val + 1));
      }
   }
}

BeatsFormat::~BeatsFormat() = default;
