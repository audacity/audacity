/**********************************************************************

  Audacity: A Digital Audio Editor


  TranscriptionToolBar.h

  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TRANSCRIPTION_TOOLBAR__
#define __AUDACITY_TRANSCRIPTION_TOOLBAR__

#include "ToolBar.h"

#include <wx/brush.h>
#include <wx/pen.h>

#include "../Sequence.h"
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
class VoiceKey;
class WaveTrack;

//TTB 0-8 are button-ids, which also correspond to their
//position in mButtons.  9 & 10 are ids for sliders, which aren't
//in the button array.
enum
   {
      TTB_PlaySpeed,
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
      TTB_PlaySpeedSlider,
      TTB_KeyType
   };

#define TTBNumButtons 10

class TranscriptionToolBar:public ToolBar {

 public:

   TranscriptionToolBar();
   virtual ~TranscriptionToolBar();

   void Create(wxWindow *parent);

   virtual void OnKeyEvent(wxKeyEvent & event);
   virtual void OnPlaySpeed(wxCommandEvent & event);
   virtual void OnSpeedSlider(wxCommandEvent & event);
   virtual void OnStartOn(wxCommandEvent & event);
   virtual void OnStartOff(wxCommandEvent & event);
   virtual void OnEndOn(wxCommandEvent & event);
   virtual void OnEndOff(wxCommandEvent & event);
   virtual void OnSelectSound(wxCommandEvent & event);
   virtual void OnSelectSilence(wxCommandEvent & event);
   virtual void OnCalibrate(wxCommandEvent & event);
   virtual void OnMakeLabel(wxCommandEvent & event);
   virtual void OnAutomateSelection(wxCommandEvent & event);
   virtual void OnSensitivitySlider(wxCommandEvent & event);

   virtual void Populate();
   virtual void Repaint(wxDC * WXUNUSED(dc)) {};
   virtual void EnableDisableButtons();
   virtual void UpdatePrefs();

   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   virtual double GetSensitivity();
   virtual void SetKeyType(wxCommandEvent & event);

   void PlayAtSpeed();
   void ShowPlaySpeedDialog();
   void AdjustPlaySpeed(float adj);

 private:

   void InitializeTranscriptionToolBar();
   AButton *AddButton(
      teBmps eFore, teBmps eDisabled,
      int id,
      const wxChar *label);
   void GetSamples(WaveTrack *t, sampleCount *s0, sampleCount *slen);
   void SetButton(bool newstate, AButton *button);
   void RegenerateTooltips();

   AButton *mButtons[TTBNumButtons];
   wxImage *upImage;
   wxImage *downImage;
   wxImage *hiliteImage;

   ASlider *mPlaySpeedSlider;
   double mPlaySpeed;
   ASlider *mSensitivitySlider;
   double mSensitivity;
   VoiceKey *mVk;

   wxBrush mBackgroundBrush;
   wxPen mBackgroundPen;
   int mBackgroundWidth;
   int mBackgroundHeight;

   TimeTrack *mTimeTrack;
   wxChoice *mKeyTypeChoice;

 public:

   DECLARE_CLASS(TranscriptionToolBar);
   DECLARE_EVENT_TABLE();
};


#define COMMAND_LINE_LOG_TRACE    TRUE
#endif

