/**********************************************************************

  Audacity: A Digital Audio Editor

  FreqGauge.h

  Dominic Mazzoni

**********************************************************************/

#pragma once

#include <vector>
#include <wx/statusbr.h>

class AUDACITY_DLL_API FreqGauge final : public wxStatusBar
{
public:
    FreqGauge(wxWindow* parent, wxWindowID winid);

    void SetRange(int range, int bar = 12, int gap = 3);
    void SetValue(int value);
    void Reset();

private:
    wxRect mRect;
    int mRange;
    int mCur;
    int mLast;
    int mInterval;
    int mBar;
    int mGap;
    int mMargin;
};
