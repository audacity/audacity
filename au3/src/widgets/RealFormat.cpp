/**********************************************************************

  Audacity: A Digital Audio Editor

  RealFormat.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#include "RealFormat.h"

const RealFormat& RealFormat::LinearInstance()
{
    static RealFormat instance{ false };
    return instance;
}

const RealFormat& RealFormat::LogInstance()
{
    static RealFormat instance{ true };
    return instance;
}

void RealFormat::SetTickSizes(
    double units, double& major, double& minor, double& minorMinor,
    int& mDigits) const
{
    double d = 0.000001;
    mDigits = 6;
    for (;;) {
        if (units < d) {
            minor = d;
            major = d * 5.0;
            return;
        }
        d *= 5.0;
        if (units < d) {
            minor = d;
            major = d * 2.0;
            return;
        }
        d *= 2.0;
        mDigits--;
        // More than 10 digit numbers?  Something is badly wrong.
        // Probably units is coming in with too high a value.
        wxASSERT(mDigits >= -10);
        if (mDigits < -10) {
            break;
        }
    }
    if (mLog) {
        mDigits++;
    }
    minor = d;
    major = d * 2.0;
}

void RealFormat::SetLabelString(
    wxString& s, double d, double, double minor, int mDigits, TickType tickType) const
{
    // Replace -0 with 0
    if (d < 0.0 && (d + minor > 0.0) && !mLog) {
        d = 0.0;
    }
    if (minor >= 1.0) {
        s.Printf(wxT("%d"), (int)floor(d + 0.5));
    } else {
        s.Printf(wxString::Format(wxT("%%.%df"), mDigits), d);
    }
}

RealFormat::~RealFormat() = default;
