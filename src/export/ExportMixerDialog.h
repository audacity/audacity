/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMixerDialog.h

  Dominic Mazzoni

**********************************************************************/

#include <memory>

#include <wx/arrstr.h>

#include "wxPanelWrapper.h"

class wxStaticText;

class TrackList;

class TranslatableString;

namespace MixerOptions { class Downmix; }

///\class ExportMixerDialog
///\brief Dialog for advanced mixing.
class ExportMixerDialog final : public wxDialogWrapper
{
public:
   // constructors and destructors
   ExportMixerDialog( const TrackList * tracks, bool selectedOnly, unsigned maxNumChannels,
         wxWindow *parent, wxWindowID id, const TranslatableString &title,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize,
         long style = wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER );
   virtual ~ExportMixerDialog();

   MixerOptions::Downmix* GetMixerSpec() { return mMixerSpec.get(); }

private:
   wxStaticText *mChannelsText;
   std::unique_ptr<MixerOptions::Downmix> mMixerSpec;
   wxArrayString mTrackNames;

private:
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   void OnMixerPanelHelp( wxCommandEvent &event );
   void OnSlider( wxCommandEvent &event );
   void OnSize( wxSizeEvent &event );

private:
   DECLARE_EVENT_TABLE()
};
