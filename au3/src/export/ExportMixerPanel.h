/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMixerPanel.h

  Dominic Mazzoni

**********************************************************************/

#pragma once

#include <memory>

#include "wxPanelWrapper.h"

#include "MemoryX.h"

class wxMemoryDC;

namespace MixerOptions {
class Downmix;
}

///\class ExportMixerPanel
///\brief Panel that displays mixing for advanced mixing option.
class ExportMixerPanel final : public wxPanelWrapper
{
public:
    ExportMixerPanel(wxWindow* parent, wxWindowID id, MixerOptions::Downmix* mixerSpec, wxArrayString trackNames,
                     const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize);
    virtual ~ExportMixerPanel();

    void OnMouseEvent(wxMouseEvent& event);
    void OnPaint(wxPaintEvent& event);

private:
    std::unique_ptr<wxBitmap> mBitmap;
    wxRect mEnvRect;
    int mWidth;
    int mHeight;
    MixerOptions::Downmix* mMixerSpec;
    ArrayOf<wxRect> mChannelRects;
    ArrayOf<wxRect> mTrackRects;
    int mSelectedTrack, mSelectedChannel;
    wxArrayString mTrackNames;
    int mBoxWidth, mChannelHeight, mTrackHeight;

    void SetFont(wxMemoryDC& memDC, const wxString& text, int width, int height);
    double Distance(wxPoint& a, wxPoint& b);
    bool IsOnLine(wxPoint p, wxPoint la, wxPoint lb);

    DECLARE_EVENT_TABLE()
};
