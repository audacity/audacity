/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeFormat.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_TIME_FORMAT__
#define __AUDACITY_TIME_FORMAT__

#include "RulerFormat.h"

class TimeFormat final : public RulerFormat
{
public:
    static const TimeFormat& Instance();
    ~TimeFormat() override;

    void SetTickSizes(
        double units, double& major, double& minor, double& minorMinor, int& mDigits) const override;

    void SetLabelString(
        wxString& s, double d, double units, double minor, int mDigits, TickType tickType) const override;

private:
    using RulerFormat::RulerFormat;
};

#endif
