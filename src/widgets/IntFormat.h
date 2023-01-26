/**********************************************************************

  Audacity: A Digital Audio Editor

  IntFormat.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_INT_FORMAT__
#define __AUDACITY_INT_FORMAT__

#include "RulerFormat.h"

class IntFormat final : public RulerFormat {
public:
   using RulerFormat::RulerFormat;
   ~IntFormat() override;

   void SetTickSizes(
      double units, double& mMajor, double& mMinor, double& mMinorMinor,
      int& mDigits, const std::any& data
   ) const override;

   void SetLabelString(
      wxString& s, double d, double mMinor, int mDigits, TickType tickType,
      const std::any& data
   ) const override;

   std::string Identify() const override {return "IntFormat"; };
};

#endif
