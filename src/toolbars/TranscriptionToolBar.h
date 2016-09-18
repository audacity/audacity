/**********************************************************************

  Audacity: A Digital Audio Editor


  TranscriptionToolBar.h

  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TRANSCRIPTION_TOOLBAR__
#define __AUDACITY_TRANSCRIPTION_TOOLBAR__

#include "ToolBar.h"
#include "../Experimental.h"

#include "../MemoryX.h"
#include <wx/brush.h>
#include <wx/pen.h>

#include "audacity/Types.h"
#include "../Theme.h"

class wxBitmap;
class wxBrush;
class wxChoice;
class wxCommandEvent;
class wxImage;
class wxKeyEvent;
class wxPen;

class AButton;
class ASlider;
class TimeTrack;
class WaveTrack;

#ifdef EXPERIMENTAL_VOICE_DETECTION
class VoiceKey;
//TTB 0-8 are button-ids, which also correspond to their
//position in mButtons.  9 & 10 are ids for sliders, which aren't
//in the button array.
#endif

enum
{
   TTB_PlaySpeed,
   TTB_PlaySpeedSlider,

#ifdef EXPERIMENTAL_VOICE_DETECTION
   TTB_StartOn,
   TTB_EndOn,
   TTB_StartOff,
   TTB_EndOff,
   TTB_SelectSound,
   TTB_SelectSilence,
   TTB_AutomateSelection,
   TTB_MakeLabel,
   TTB_Calibrate,
   TTB_SensitivitySlider,
   TTB_KeyType,
#endif

   TTBNumButtons
};

class TranscriptionToolBar final : public ToolBar {

 public:

   TranscriptionToolBar();
   virtual ~TranscriptionToolBar();

   void Create(wxWindow *parent);

   void OnKeyEvent(wxKeyEvent & event);
   void OnPlaySpeed(wxCommandEvent & event);
   void OnSpeedSlider(wxCommandEvent & event);

   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override;
   void UpdatePrefs() override;

   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

#ifdef EXPERIMENTAL_VOICE_DETECTION
   void OnStartOn(wxCommandEvent & event);
   void OnStartOff(wxCommandEvent & event);
   void OnEndOn(wxCommandEvent & event);
   void OnEndOff(wxCommandEvent & event);
   void OnSelectSound(wxCommandEvent & event);
   void OnSelectSilence(wxCommandEvent & event);
   void OnCalibrate(wxCommandEvent & event);
   void OnMakeLabel(wxCommandEvent & event);
   void OnAutomateSelection(wxCommandEvent & event);
   void OnSensitivitySlider(wxCommandEvent & event);

   //void Populate() override;
   //void Repaint(wxDC * WXUNUSED(dc)) override {}
   //void EnableDisableButtons() override;
   //void UpdatePrefs() override;

   //void OnFocus(wxFocusEvent &event);
   //void OnCaptureKey(wxCommandEvent &event);

   double GetSensitivity();
   void SetKeyType(wxCommandEvent & event);
#endif

   void PlayAtSpeed(bool looped, bool cutPreview);
   void ShowPlaySpeedDialog();
   void AdjustPlaySpeed(float adj);

   void SetEnabled(bool enabled);
   void SetPlaying(bool down, bool looped, bool cutPreview);

   double GetPlaySpeed() const { return mPlaySpeed / 100.0; }

 private:

   void InitializeTranscriptionToolBar();
   AButton *AddButton(
      teBmps eFore, teBmps eDisabled,
      int id,
      const wxChar *label);
   void MakeAlternateImages(
      teBmps eFore, teBmps eDisabled,
      int id, unsigned altIdx);
   void GetSamples(WaveTrack *t, sampleCount *s0, sampleCount *slen);
   void SetButton(bool newstate, AButton *button);
   void RegenerateTooltips() override;

   AButton *mButtons[TTBNumButtons];
   wxImage *upImage;
   wxImage *downImage;
   wxImage *hiliteImage;

   ASlider *mPlaySpeedSlider;
   double mPlaySpeed;
   ASlider *mSensitivitySlider;

#ifdef EXPERIMENTAL_VOICE_DETECTION
   double mSensitivity;
   std::unique_ptr<VoiceKey> mVk;
   wxChoice *mKeyTypeChoice;
#endif

   wxBrush mBackgroundBrush;
   wxPen mBackgroundPen;
   int mBackgroundWidth;
   int mBackgroundHeight;

   std::unique_ptr<TimeTrack> mTimeTrack;

 public:

   DECLARE_CLASS(TranscriptionToolBar)
   DECLARE_EVENT_TABLE()
};

#endif

