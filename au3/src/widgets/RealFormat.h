/**********************************************************************

  Audacity: A Digital Audio Editor

  RealFormat.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_REAL_FORMAT__
#define __AUDACITY_REAL_FORMAT__

#include "RulerFormat.h"

// Unified what was formerly RealFormat and RealLogFormat
class RealFormat final : public RulerFormat
{
public:
    static const RealFormat& LinearInstance();
    static const RealFormat& LogInstance();
    ~RealFormat() override;

    void SetTickSizes(
        double units, double& major, double& minor, double& minorMinor, int& mDigits) const override;

    void SetLabelString(
        wxString& s, double d, double units, double minor, int mDigits, TickType tickType) const override;

private:
    RealFormat(bool log)
        : mLog{log} {}
    const bool mLog;
};

#endif
