/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerToolBar.cpp

  Dominic Mazzoni

*******************************************************************//*!

\class MixerToolBar
\brief A ToolBar that provides the record and playback volume settings.

*//*******************************************************************/


#include "../Audacity.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/choice.h>
#include <wx/event.h>
#include <wx/intl.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/tooltip.h>
#endif

#include "MixerToolBar.h"

#include "../AudacityApp.h"
#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../widgets/ASlider.h"

IMPLEMENT_CLASS(MixerToolBar, ToolBar);

////////////////////////////////////////////////////////////
/// Methods for MixerToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(MixerToolBar, ToolBar)
   EVT_PAINT(MixerToolBar::OnPaint)
   EVT_SLIDER(wxID_ANY, MixerToolBar::SetMixer)
   EVT_CHOICE(wxID_ANY, MixerToolBar::SetMixer)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, MixerToolBar::OnCaptureKey)
END_EVENT_TABLE()

//Standard contructor
MixerToolBar::MixerToolBar()
: ToolBar(MixerBarID, _("Mixer"), wxT("Mixer"))
{
   mPlayBitmap = NULL;
   mRecordBitmap = NULL;
   mInputSliderVolume = 0.0;
   mOutputSliderVolume = 0.0;
}

MixerToolBar::~MixerToolBar()
{
}

void MixerToolBar::Create(wxWindow *parent)
{
   ToolBar::Create(parent);
}

void MixerToolBar::Populate()
{
   if( mRecordBitmap == NULL )
      mRecordBitmap = std::make_unique<wxBitmap>(theTheme.Bitmap(bmpMic));

   Add(safenew wxStaticBitmap(this,
                          wxID_ANY,
                          *mRecordBitmap), 0, wxALIGN_CENTER);

   mInputSlider = safenew ASlider(this, wxID_ANY, _("Recording Volume"),
                              wxDefaultPosition, wxSize(130, 25));
   mInputSlider->SetScroll(0.1f, 2.0f);
   mInputSlider->SetName(_("Slider Recording"));
   Add(mInputSlider, 0, wxALIGN_CENTER);

   if( mPlayBitmap == NULL )
      mPlayBitmap = std::make_unique<wxBitmap>(theTheme.Bitmap(bmpSpeaker));

   Add(safenew wxStaticBitmap(this,
                          wxID_ANY,
                          *mPlayBitmap), 0, wxALIGN_CENTER);

   mOutputSlider = safenew ASlider(this, wxID_ANY, _("Playback Volume"),
                               wxDefaultPosition, wxSize(130, 25));
   mOutputSlider->SetScroll(0.1f, 2.0f);
   mOutputSlider->SetName(_("Slider Playback"));
   Add(mOutputSlider, 0, wxALIGN_CENTER);

   // this bit taken from SelectionBar::Populate()
   mInputSlider->Connect(wxEVT_SET_FOCUS,
                 wxFocusEventHandler(MixerToolBar::OnFocus),
                 NULL,
                 this);
   mInputSlider->Connect(wxEVT_KILL_FOCUS,
                 wxFocusEventHandler(MixerToolBar::OnFocus),
                 NULL,
                 this);
   mOutputSlider->Connect(wxEVT_SET_FOCUS,
                 wxFocusEventHandler(MixerToolBar::OnFocus),
                 NULL,
                 this);
   mOutputSlider->Connect(wxEVT_KILL_FOCUS,
                 wxFocusEventHandler(MixerToolBar::OnFocus),
                 NULL,
                 this);
   // Show or hide the input slider based on whether it works
   mInputSlider->Enable(gAudioIO->InputMixerWorks());

   UpdateControls();

   // Add a little space
   Add(2, -1);
}

//Also from SelectionBar;
void MixerToolBar::OnFocus(wxFocusEvent &event)
{
   if (event.GetEventType() == wxEVT_KILL_FOCUS) {
      AudacityProject::ReleaseKeyboard(this);
   }
   else {
      AudacityProject::CaptureKeyboard(this);
   }

   Refresh(false);

   event.Skip();
}

void MixerToolBar::OnCaptureKey(wxCommandEvent &event)
{
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();
   int keyCode = kevent->GetKeyCode();

   // Pass LEFT/RIGHT/UP/DOWN/PAGEUP/PAGEDOWN through for input/output sliders
   if (FindFocus() == mInputSlider && (keyCode == WXK_LEFT || keyCode == WXK_RIGHT
                                    || keyCode == WXK_UP || keyCode == WXK_DOWN
                                    || keyCode == WXK_PAGEUP || keyCode == WXK_PAGEDOWN)) {
      return;
   }
   if (FindFocus() == mOutputSlider && (keyCode == WXK_LEFT || keyCode == WXK_RIGHT
                                    || keyCode == WXK_UP || keyCode == WXK_DOWN
                                    || keyCode == WXK_PAGEUP || keyCode == WXK_PAGEDOWN)) {
      return;
   }

   event.Skip();

   return;
}

void MixerToolBar::UpdatePrefs()
{
#if USE_PORTMIXER
   float inputVolume;
   float playbackVolume;
   int inputSource;

   // Reset the selected source
   gAudioIO->GetMixer(&inputSource, &inputVolume, &playbackVolume);

   // Show or hide the input slider based on whether it works
   mInputSlider->Enable(gAudioIO->InputMixerWorks());

   // Layout the toolbar
   Layout();

   // Resize the toolbar to fit the contents
   Fit();

   // And make that size the minimum
   SetMinSize( wxWindow::GetSizer()->GetMinSize() );
   SetSize( GetMinSize() );

   // Notify someone that we've changed our size
   Updated();
#endif

   // Set label to pull in language change
   SetLabel(_("Mixer"));

   RegenerateTooltips();

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void MixerToolBar::UpdateControls()
{
#if USE_PORTMIXER
   float inputVolume;
   float playbackVolume;
   int inputSource;

   // Show or hide the input slider based on whether it works
   mInputSlider->Enable(gAudioIO->InputMixerWorks());

   gAudioIO->GetMixer(&inputSource, &inputVolume, &playbackVolume);

   if (mOutputSlider->Get() != playbackVolume) {
      mOutputSlider->Set(playbackVolume);
      mOutputSliderVolume = playbackVolume;
      SetToolTips();
   }

   if (mInputSlider->Get() != inputVolume) {
      mInputSlider->Set(inputVolume);
      mInputSliderVolume = inputVolume;
      SetToolTips();
   }
#endif // USE_PORTMIXER
}

void MixerToolBar::SetMixer(wxCommandEvent & WXUNUSED(event))
{
#if USE_PORTMIXER
   float inputVolume = mInputSlider->Get();
   float outputVolume = mOutputSlider->Get();
   float oldIn, oldOut;
   int inputSource;

   gAudioIO->GetMixer(&inputSource, &oldIn, &oldOut);
   gAudioIO->SetMixer(inputSource, inputVolume, outputVolume);
   mOutputSliderVolume = outputVolume;
   mInputSliderVolume = inputVolume;
   SetToolTips();
#endif // USE_PORTMIXER
}

void MixerToolBar::ShowOutputGainDialog()
{
   mOutputSlider->ShowDialog();
   wxCommandEvent e;
   SetMixer(e);
   UpdateControls();
}

void MixerToolBar::ShowInputGainDialog()
{
   mInputSlider->ShowDialog();
   wxCommandEvent e;
   SetMixer(e);
   UpdateControls();
}

void MixerToolBar::AdjustOutputGain(int adj)
{
   if (adj < 0) {
      mOutputSlider->Decrease(-adj);
   }
   else {
      mOutputSlider->Increase(adj);
   }
   wxCommandEvent e;
   SetMixer(e);
   UpdateControls();
}

void MixerToolBar::AdjustInputGain(int adj)
{
   if (adj < 0) {
      mInputSlider->Decrease(-adj);
   }
   else {
      mInputSlider->Increase(adj);
   }
   wxCommandEvent e;
   SetMixer(e);
   UpdateControls();
}

void MixerToolBar::SetToolTips()
{
   if (mInputSlider->IsEnabled()) {
      mInputSlider->SetToolTipTemplate(_("Recording Volume: %.2f"));
   }
   else {
      mInputSlider->SetToolTipTemplate(_("Recording Volume (Unavailable; use system mixer.)"));
   }

   if (mOutputSlider->IsEnabled()) {
      mOutputSlider->SetToolTipTemplate(wxString::Format(
            _("Playback Volume: %%.2f%s"), gAudioIO->OutputMixerEmulated() ? _(" (emulated)") : wxT("")));
   }
   else {
      mOutputSlider->SetToolTipTemplate(_("Playback Volume (Unavailable; use system mixer.)"));
   }
}
