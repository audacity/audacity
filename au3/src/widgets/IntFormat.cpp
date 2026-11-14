/**********************************************************************

  Audacity: A Digital Audio Editor

  IntFormat.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#include "IntFormat.h"

const IntFormat& ::IntFormat::Instance()
{
    static IntFormat instance;
    return instance;
}

void IntFormat::SetTickSizes(
    double units, double& major, double& minor, double& minorMinor,
    int& mDigits) const
{
    double d;
    d = 1.0;
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
    }
}

void IntFormat::SetLabelString(
    wxString& s, double d, double, double minor, int mDigits, TickType tickType) const
{
    // Replace -0 with 0
    if (d < 0.0 && (d + minor > 0.0)) {
        d = 0.0;
    }
    s.Printf(wxT("%d"), (int)floor(d + 0.5));
}

IntFormat::~IntFormat() = default;
