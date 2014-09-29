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
   mInputSliderVolume = 0.0;
   mOutputSliderVolume = 0.0;
}

MixerToolBar::~MixerToolBar()
{
   delete mPlayBitmap;
   delete mRecordBitmap;
}

void MixerToolBar::Create(wxWindow *parent)
{
   ToolBar::Create(parent);
}

void MixerToolBar::RecreateTipWindows()
{
   // Hack to make sure they appear on top of other windows
   mInputSlider->RecreateTipWin();
   mOutputSlider->RecreateTipWin();
}

void MixerToolBar::Populate()
{
   mPlayBitmap = new wxBitmap(theTheme.Bitmap(bmpSpeaker));

   Add(new wxStaticBitmap(this,
                          wxID_ANY,
                          *mPlayBitmap), 0, wxALIGN_CENTER);

   mOutputSlider = new ASlider(this, wxID_ANY, _("Playback Volume"),
                               wxDefaultPosition, wxSize(130, 25));
   mOutputSlider->SetScroll(0.1f, 2.0f);
   mOutputSlider->SetName(_("Slider Playback"));
   Add(mOutputSlider, 0, wxALIGN_CENTER);

   mRecordBitmap = new wxBitmap(theTheme.Bitmap(bmpMic));

   Add(new wxStaticBitmap(this,
                          wxID_ANY,
                          *mRecordBitmap), 0, wxALIGN_CENTER);

   mInputSlider = new ASlider(this, wxID_ANY, _("Recording Volume"),
                              wxDefaultPosition, wxSize(130, 25));
   mInputSlider->SetScroll(0.1f, 2.0f);
   mInputSlider->SetName(_("Slider Recording"));
   Add(mInputSlider, 0, wxALIGN_CENTER);

   // this bit taken from SelectionBar::Populate()
   mOutputSlider->Connect(wxEVT_SET_FOCUS,
                 wxFocusEventHandler(MixerToolBar::OnFocus),
                 NULL,
                 this);
   mOutputSlider->Connect(wxEVT_KILL_FOCUS,
                 wxFocusEventHandler(MixerToolBar::OnFocus),
                 NULL,
                 this);
   mInputSlider->Connect(wxEVT_SET_FOCUS,
                 wxFocusEventHandler(MixerToolBar::OnFocus),
                 NULL,
                 this);
   mInputSlider->Connect(wxEVT_KILL_FOCUS,
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
   wxCommandEvent e(EVT_CAPTURE_KEYBOARD);

   if (event.GetEventType() == wxEVT_KILL_FOCUS) {
      e.SetEventType(EVT_RELEASE_KEYBOARD);
   }
   e.SetEventObject(this);
   GetParent()->GetEventHandler()->ProcessEvent(e);

   Refresh(false);

   event.Skip();
}

void MixerToolBar::OnCaptureKey(wxCommandEvent &event)
{
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();
   int keyCode = kevent->GetKeyCode();

   // Pass LEFT/RIGHT/UP/DOWN/PAGEUP/PAGEDOWN through for input/output sliders
   if (FindFocus() == mOutputSlider && (keyCode == WXK_LEFT || keyCode == WXK_RIGHT
                                    || keyCode == WXK_UP || keyCode == WXK_DOWN
                                    || keyCode == WXK_PAGEUP || keyCode == WXK_PAGEDOWN)) {
      return;
   }
   if (FindFocus() == mInputSlider && (keyCode == WXK_LEFT || keyCode == WXK_RIGHT
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
#if wxUSE_TOOLTIPS
   if (mInputSlider->IsEnabled()) {
      mInputSlider->SetToolTip(wxString::Format(
            _("Recording Volume: %.2f"), mInputSliderVolume));
   }
   else {
      mInputSlider->SetToolTip(
            _("Recording Volume (Unavailable; use system mixer.)"));
   }

   if (mOutputSlider->IsEnabled()) {
      mOutputSlider->SetToolTip(wxString::Format(
            _("Playback Volume: %.2f%s"), mOutputSliderVolume, gAudioIO->OutputMixerEmulated() ? _(" (emulated)") : wxT("")));
   }
   else {
      mOutputSlider->SetToolTip(
            _("Playback Volume (Unavailable; use system mixer.)"));
   }
#endif
}

