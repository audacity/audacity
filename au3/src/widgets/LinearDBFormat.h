/**********************************************************************

  Audacity: A Digital Audio Editor

  LinearDBFormat.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_LINEAR_DB_FORMAT__
#define __AUDACITY_LINEAR_DB_FORMAT__

#include "RulerFormat.h"

class LinearDBFormat final : public RulerFormat
{
public:
    static const LinearDBFormat& Instance();
    ~LinearDBFormat() override;

    void SetTickSizes(
        double units, double& major, double& minor, double& minorMinor, int& mDigits) const override;

    void SetLabelString(
        wxString& s, double d, double units, double minor, int mDigits, TickType tickType) const override;

private:
    using RulerFormat::RulerFormat;
};

#endif
