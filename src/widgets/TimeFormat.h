/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeFormat.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_TIME_FORMAT__
#define __AUDACITY_TIME_FORMAT__

#include "RulerFormat.h"

class TimeFormat final : public RulerFormat {
public:
   using RulerFormat::RulerFormat;
   ~TimeFormat() override;

   void SetTickSizes(
      double units, double& mMajor, double& mMinor, int& mDigits,
      const std::any& data
   ) const override;

   void SetLabelString(
      wxString& s, double d, double mMinor, int mDigits, bool useMajor,
      const std::any& data
   ) const override;

   std::string Identify() const override { return "TimeFormat"; };
};

#endif
