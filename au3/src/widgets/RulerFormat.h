/**********************************************************************

  Audacity: A Digital Audio Editor

  RulerFormat.h

  Michael Papadopoulos

**********************************************************************/

#ifndef __AUDACITY_RULER_FORMAT__
#define __AUDACITY_RULER_FORMAT__

#include <wx/string.h>
#include <cmath>

class RulerFormat
{
public:
    enum TickType {
        t_major, t_minor, t_minorMinor
    };
    explicit RulerFormat() {}
    virtual ~RulerFormat();

    virtual void SetTickSizes(
        double units, double& major, double& minor, double& minorMinor, int& mDigits) const = 0;

    virtual void SetLabelString(
        wxString& s, double d, double units, double minor, int mDigits, TickType tickType) const = 0;
};

#endif
