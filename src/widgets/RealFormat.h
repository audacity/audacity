/**********************************************************************

  Audacity: A Digital Audio Editor

  RealFormat.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_REAL_FORMAT__
#define __AUDACITY_REAL_FORMAT__

#include "RulerFormat.h"

struct RealFormatData { const bool log; };

class RealFormat final : public RulerFormat {
public:
   using RulerFormat::RulerFormat;
   ~RealFormat() override;

   void SetTickSizes(
      double units, double& mMajor, double& mMinor, double& mMinorMinor,
      int& mDigits, const std::any& data
   ) const override;

   void SetLabelString(
      wxString& s, double d, double mMinor, int mDigits, TickType tickType,
      const std::any& data
   ) const override;

   std::string Identify() const override { return "RealFormat"; };
};

#endif
