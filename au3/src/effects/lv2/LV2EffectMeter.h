/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2EffectMeter.h
  @brief widget to paint a varying bar in plain LV2 UI

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#ifndef __AUDACITY_LV2_EFFECT_METER__
#define __AUDACITY_LV2_EFFECT_METER__

#include <wx/window.h>
#include "LV2Ports.h"

//! UI widget that watches a floating point location and then updates a bar
class LV2EffectMeter final : public wxWindow
{
public:
    LV2EffectMeter(wxWindow* parent, const LV2ControlPortPtr ctrl, const float& value);
    virtual ~LV2EffectMeter();
    void Disconnect()
    {
        // Stop using mValue, it may be dangling
        mConnected = false;
    }

private:
    void OnErase(wxEraseEvent& evt);
    void OnPaint(wxPaintEvent& evt);
    void OnIdle(wxIdleEvent& evt);
    void OnSize(wxSizeEvent& evt);

private:
    bool mConnected{ true };
    const LV2ControlPortPtr mControlPort;
    const float& mValue;
    float mLastValue;

    DECLARE_EVENT_TABLE()
};

#endif
