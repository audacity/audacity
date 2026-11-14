/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipIndicatorPanel.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "ClipIndicatorPanel.h"
#include "AudioIO.h"
#include "DynamicRangeProcessorPanelCommon.h"
#include <wx/dcclient.h>

BEGIN_EVENT_TABLE(ClipIndicatorPanel, wxPanelWrapper)
EVT_PAINT(ClipIndicatorPanel::OnPaint)
END_EVENT_TABLE()

ClipIndicatorPanel::ClipIndicatorPanel(wxWindow* parent, int id)
    : wxPanelWrapper{parent, id}
    , mPlaybackStartStopSubscription{AudioIO::Get()->Subscribe(
                                         [this](const AudioIOEvent& evt) {
        if (evt.type == AudioIOEvent::PLAYBACK && evt.on)
            Reset();
    })}
{
}

void ClipIndicatorPanel::SetClipped()
{
    mClipping = true;
    Refresh();
}

void ClipIndicatorPanel::Reset()
{
    mClipping = false;
    Refresh();
}

void ClipIndicatorPanel::OnPaint(wxPaintEvent& evt)
{
    using namespace DynamicRangeProcessorPanel;

    wxPaintDC dc { this };
    const auto color = mClipping ? *wxRED : *wxLIGHT_GREY;
    dc.SetBrush(color);
    dc.SetPen(lineColor);
    auto rect = GetPanelRect(*this);
    dc.DrawRoundedRectangle(rect, 2);

    // This is supposed to be a LED. Add a little shine.
    dc.SetBrush(GetColorMix(*wxWHITE, color, 0.5));
    dc.SetPen(*wxTRANSPARENT_PEN);
    dc.DrawRoundedRectangle(rect.Deflate(2), 2);
}
