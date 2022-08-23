/**********************************************************************

  Audacity: A Digital Audio Editor

  BeatsFormat.h

  Michael Papadopoulos

**********************************************************************/

#ifndef __AUDACITY_BEATS_FORMAT__
#define __AUDACITY_BEATS_FORMAT__

#include "RulerFormat.h"

struct BeatsFormatData { const double bpm; const int timeSigUpper; const int timeSigLower; };

class BeatsFormat final : public RulerFormat {
public:
   using RulerFormat::RulerFormat;
   ~BeatsFormat() override;

   void SetTickSizes(
      double units, double& mMajor, double& mMinor, double &mMinorMinor,
      int& mDigits, const std::any& data
   ) const override;

   void SetLabelString(
      wxString& s, double d, double mMinor, int mDigits, TickType tickType,
      const std::any& data
   ) const override;

   std::string Identify() const override { return "BeatsFormat"; };
};

#endif
