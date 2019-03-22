/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerToolBar.cpp

  Dominic Mazzoni

*******************************************************************//*!

\class MixerToolBar
\brief A ToolBar that provides the record and playback volume settings.

*//*******************************************************************/


#include "../Audacity.h" // for USE_* macros
#include "MixerToolBar.h"

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

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../widgets/ASlider.h"
#include "../widgets/Grabber.h"

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
: ToolBar(MixerBarID, _("Mixer"), wxT("Mixer"), true)
{
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
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
   // Recording icon and slider
   Add(safenew AStaticBitmap(this,
                          wxID_ANY,
                          theTheme.Bitmap(bmpMic)), 0, wxALIGN_CENTER);
   mInputSlider = safenew ASlider(this, wxID_ANY, _("Recording Volume"),
                              wxDefaultPosition, wxSize(130, 25),
                              ASlider::Options{}.Line( 0.1f ).Page( 2.0f ));
   Add(mInputSlider, 1, wxALIGN_CENTER);
   mInputSlider->SetSizeHints(wxSize(75, 25), wxSize(1000, 25));

   // Playback icon and slider
   Add(safenew AStaticBitmap(this,
                          wxID_ANY,
                          theTheme.Bitmap(bmpSpeaker)), 0, wxALIGN_CENTER);
   mOutputSlider = safenew ASlider(this, wxID_ANY, _("Playback Volume"),
                               wxDefaultPosition, wxSize(130, 25),
                               ASlider::Options{}.Line( 0.1f ).Page( 2.0f ));
   Add(mOutputSlider, 1, wxALIGN_CENTER);
   mOutputSlider->SetSizeHints(wxSize(75, 25), wxSize(1000, 25));

   // this bit taken from SelectionBar::Populate()
   mInputSlider->Bind(wxEVT_SET_FOCUS,
                 &MixerToolBar::OnFocus,
                 this);
   mInputSlider->Bind(wxEVT_KILL_FOCUS,
                 &MixerToolBar::OnFocus,
                 this);
   mOutputSlider->Bind(wxEVT_SET_FOCUS,
                 &MixerToolBar::OnFocus,
                 this);
   mOutputSlider->Bind(wxEVT_KILL_FOCUS,
                 &MixerToolBar::OnFocus,
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
   Layout();

// This code is from before the mixer toolbar was resizable.
// Now that it is resizable we trust the user to resize the mixer toolbar themselves.
#if 0
   wxSize oldSize( GetSize() );
   // Layout the toolbar
   Layout();
   // Resize the toolbar to fit the contents
   //Fit();
   // And make that size the minimum
   wxSize newMinSize( wxWindow::GetSizer()->GetMinSize() );
   SetMinSize( newMinSize  );
   // IF size must increase, do so.
   if( newMinSize.x > oldSize.x ){
      SetSize( newMinSize );
      // Notify someone that we've changed our size
      Updated();
   }
   // ELSE preserve original size.
   else
      SetSize( oldSize );
#endif
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
      wxString format;
      if (gAudioIO->OutputMixerEmulated())
         format = _("Playback Volume: %s (emulated)");
      else
         format = _("Playback Volume: %s");

      mOutputSlider->SetToolTipTemplate(
         wxString::Format( format, "%.2f" ) );
   }
   else {
      mOutputSlider->SetToolTipTemplate(_("Playback Volume (Unavailable; use system mixer.)"));
   }
}
