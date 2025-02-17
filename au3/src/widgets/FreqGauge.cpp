/**********************************************************************

  Audacity: A Digital Audio Editor

  FreqGauge.cpp

  Dominic Mazzoni

**********************************************************************/

/*
  Salvo Ventura - November 2006
  Extended range check for additional FFT windows
*/

#include "FreqGauge.h"
#include "FFT.h"

#include <wx/dcclient.h>

FreqGauge::FreqGauge(wxWindow* parent, wxWindowID winid)
    :  wxStatusBar(parent, winid, wxST_SIZEGRIP)
{
    mRange = 0;
}

void FreqGauge::SetRange(int range, int bar, int gap)
{
    mRange = range;
    mBar = bar;
    mGap = gap;

    GetFieldRect(0, mRect);
    mRect.Inflate(-1);

    mInterval = mRange / (mRect.width / (mBar + mGap));
    mRect.width = mBar;
    mMargin = mRect.x;
    mLast = -1;

    Update();
}

void FreqGauge::SetValue(int value)
{
    mCur = value / mInterval;

    if (mCur != mLast) {
        wxClientDC dc(this);
        dc.SetPen(*wxTRANSPARENT_PEN);
        dc.SetBrush(wxColour(100, 100, 220));

        while (mLast < mCur)
        {
            mLast++;
            mRect.x = mMargin + mLast * (mBar + mGap);
            dc.DrawRectangle(mRect);
        }
        Update();
    }
}

void FreqGauge::Reset()
{
    mRange = 0;
    Refresh(true);
}
