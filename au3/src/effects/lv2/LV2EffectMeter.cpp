/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2EffectMeter.cpp

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "LV2EffectMeter.h"
#include <wx/dcbuffer.h>

BEGIN_EVENT_TABLE(LV2EffectMeter, wxWindow)
EVT_IDLE(LV2EffectMeter::OnIdle)
EVT_ERASE_BACKGROUND(LV2EffectMeter::OnErase)
EVT_PAINT(LV2EffectMeter::OnPaint)
EVT_SIZE(LV2EffectMeter::OnSize)
END_EVENT_TABLE()

LV2EffectMeter::LV2EffectMeter(
    wxWindow* parent, const LV2ControlPortPtr port, const float& value)
    : wxWindow{parent, wxID_ANY, wxDefaultPosition, wxDefaultSize,
               wxSIMPLE_BORDER}
    , mControlPort(port)
    , mValue{value}
{
    mLastValue = -value;
    SetBackgroundColour(*wxWHITE);
    SetMinSize({ 20, 20 });
}

LV2EffectMeter::~LV2EffectMeter()
{
}

void LV2EffectMeter::OnIdle(wxIdleEvent& evt)
{
    evt.Skip();
    if (!mConnected) {
        return;
    }
    if (mLastValue != mValue) {
        Refresh(false);
    }
}

void LV2EffectMeter::OnErase(wxEraseEvent& WXUNUSED(evt))
{
    // Just ignore it to prevent flashing
}

void LV2EffectMeter::OnPaint(wxPaintEvent& WXUNUSED(evt))
{
    if (!mConnected) {
        return;
    }

    std::unique_ptr<wxDC> dc { wxAutoBufferedPaintDCFactory(this) };

    // Cache some metrics
    wxRect r = GetClientRect();
    wxCoord x = r.GetLeft();
    wxCoord y = r.GetTop();
    wxCoord w = r.GetWidth();
    wxCoord h = r.GetHeight();

    // These use unscaled value, min, and max
    float val = std::clamp(mValue, mControlPort->mMin, mControlPort->mMax)
                - mControlPort->mMin;

    // Setup for erasing the background
    dc->SetPen(*wxTRANSPARENT_PEN);
    dc->SetBrush(wxColour(100, 100, 220));

    dc->Clear();
    dc->DrawRectangle(x, y, (w * (val / fabs(mControlPort->mMax - mControlPort->mMin))), h);

    mLastValue = mValue;
}

void LV2EffectMeter::OnSize(wxSizeEvent& WXUNUSED(evt))
{
    Refresh(false);
}

#endif
