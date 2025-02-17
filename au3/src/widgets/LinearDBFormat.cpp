/**********************************************************************

  Audacity: A Digital Audio Editor

  LinearDBFormat.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#include "LinearDBFormat.h"

const LinearDBFormat& ::LinearDBFormat::Instance()
{
    static LinearDBFormat instance;
    return instance;
}

void LinearDBFormat::SetTickSizes(
    double units, double& major, double& minor, double& minorMinor,
    int& mDigits) const
{
    if (units < 0.001) {
        minor = 0.001;
        major = 0.005;
        return;
    }
    if (units < 0.01) {
        minor = 0.01;
        major = 0.05;
        return;
    }
    if (units < 0.1) {
        minor = 0.1;
        major = 0.5;
        return;
    }
    if (units < 1.0) {
        minor = 1.0;
        major = 6.0;
        return;
    }
    if (units < 3.0) {
        minor = 3.0;
        major = 12.0;
        return;
    }
    if (units < 6.0) {
        minor = 6.0;
        major = 24.0;
        return;
    }
    if (units < 12.0) {
        minor = 12.0;
        major = 48.0;
        return;
    }
    if (units < 24.0) {
        minor = 24.0;
        major = 96.0;
        return;
    }
    double d = 20.0;
    for (;;) {
        if (units < d) {
            minor = d;
            major = d * 5.0;
            return;
        }
        d *= 5.0;
        if (units < d) {
            minor = d;
            major = d * 5.0;
            return;
        }
        d *= 2.0;
    }
}

void LinearDBFormat::SetLabelString(
    wxString& s, double d, double, double mMinor, int mDigits, TickType tickType) const
{
    // Replace -0 with 0
    if (d < 0.0 && (d + mMinor > 0.0)) {
        d = 0.0;
    }
    if (mMinor >= 1.0) {
        s.Printf(wxT("%d"), (int)floor(d + 0.5));
    } else {
        int precision = -log10(mMinor);
        s.Printf(wxT("%.*f"), precision, d);
    }
}

LinearDBFormat::~LinearDBFormat() = default;
