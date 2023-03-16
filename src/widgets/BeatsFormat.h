/**********************************************************************

  Audacity: A Digital Audio Editor

  BeatsFormat.h

  Michael Papadopoulos

**********************************************************************/

#ifndef __AUDACITY_BEATS_FORMAT__
#define __AUDACITY_BEATS_FORMAT__

#include "RulerFormat.h"

class BeatsFormat final : public RulerFormat {
public:
   using RulerFormat::RulerFormat;
   ~BeatsFormat() override;

   void SetTickSizes(
      double units, double& major, double& minor, double &minorMinor,
      int& mDigits
   ) const override;

   void SetLabelString(
      wxString& s, double d, double units, double minor, int mDigits, TickType tickType
   ) const override;

   void SetData(double bpm, int timeSigUpper, int timeSigLower)
   {
      mBpm = bpm;
      mTimeSigUpper = timeSigUpper;
      mTimeSigLower = timeSigLower;
   }

private:
   double mBpm{ 60.0 };
   int mTimeSigUpper{ 4 };
   int mTimeSigLower{ 4 };
};

#endif
