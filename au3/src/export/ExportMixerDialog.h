/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMixerDialog.h

  Dominic Mazzoni

**********************************************************************/

#include <memory>

#include <wx/arrstr.h>

#include "wxPanelWrapper.h"

#include "Track.h"

class wxStaticText;

class WaveTrack;

class TranslatableString;

namespace MixerOptions {
class Downmix;
}

///\class ExportMixerDialog
///\brief Dialog for advanced mixing.
class ExportMixerDialog final : public wxDialogWrapper
{
public:
    // constructors and destructors
    ExportMixerDialog(TrackIterRange<const WaveTrack> tracks, MixerOptions::Downmix* mixerSpec, wxWindow* parent, wxWindowID id,
                      const TranslatableString& title, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
                      long style = wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
    virtual ~ExportMixerDialog();

private:
    wxStaticText* mChannelsText;
    MixerOptions::Downmix* mMixerSpec;
    wxArrayString mTrackNames;

private:
    void OnOk(wxCommandEvent& event);
    void OnCancel(wxCommandEvent& event);
    void OnMixerPanelHelp(wxCommandEvent& event);
    void OnSlider(wxCommandEvent& event);
    void OnSize(wxSizeEvent& event);

private:
    DECLARE_EVENT_TABLE()
};
