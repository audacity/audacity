/**********************************************************************

  Audacity: A Digital Audio Editor

  IntFormat.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_INT_FORMAT__
#define __AUDACITY_INT_FORMAT__

#include "RulerFormat.h"

class IntFormat final : public RulerFormat
{
public:
    static const IntFormat& Instance();

    ~IntFormat() override;

    void SetTickSizes(
        double units, double& major, double& minor, double& minorMinor, int& mDigits) const override;

    void SetLabelString(
        wxString& s, double d, double units, double minor, int mDigits, TickType tickType) const override;

private:
    using RulerFormat::RulerFormat;
};

#endif
