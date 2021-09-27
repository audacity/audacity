/**********************************************************************

  Audacity: A Digital Audio Editor


  ControlToolbar.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_CONTROL_TOOLBAR__
#define __AUDACITY_CONTROL_TOOLBAR__

#include "ToolBar.h"

class wxBoxSizer;
class wxCommandEvent;
class wxDC;
class wxKeyEvent;
class wxTimer;
class wxTimerEvent;
class wxWindow;
class wxStatusBar;

class AButton;
class AudacityProject;

// In the GUI, ControlToolBar appears as the "Transport Toolbar". "Control Toolbar" is historic.
class AUDACITY_DLL_API ControlToolBar final : public ToolBar {

 public:

   ControlToolBar( AudacityProject &project );
   virtual ~ControlToolBar();

   static ControlToolBar *Find( AudacityProject &project );
   static ControlToolBar &Get( AudacityProject &project );
   static const ControlToolBar &Get( const AudacityProject &project );

   void Create(wxWindow *parent) override;

   void UpdatePrefs() override;
   void OnKeyEvent(wxKeyEvent & event);

   // msmeyer: These are public, but it's far better to
   // call the "real" interface functions below.
   void OnRewind(wxCommandEvent & evt);
   void OnPlay(wxCommandEvent & evt);
   void OnStop(wxCommandEvent & evt);
   void OnRecord(wxCommandEvent & evt);
   void OnFF(wxCommandEvent & evt);
   void OnPause(wxCommandEvent & evt);
   void OnLoop(wxCommandEvent & evt);
   void OnIdle(wxIdleEvent & event);

   // Choice among the appearances of the play button:
   enum class PlayAppearance {
      Straight, Looped, CutPreview, Scrub, Seek
   };

   //These allow buttons to be controlled externally:
   void SetPlay(bool down, PlayAppearance appearance = PlayAppearance::Straight);
   void SetStop();

   // Play the region [t0,t1]
   // Return the Audio IO token or -1 for failure
   void PlayDefault();

   void Populate() override;
   void Repaint(wxDC *dc) override;
   void EnableDisableButtons() override;

   void ReCreateButtons() override;
   void RegenerateTooltips() override;

   // Starting and stopping of scrolling display
   void StartScrollingIfPreferred();
   void StartScrolling();
   void StopScrolling();

 private:
   void UpdateStatusBar();

   static AButton *MakeButton(
      ControlToolBar *pBar,
      teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
      int id,
      bool processdownevents,
      const TranslatableString &label);

   static
   void MakeAlternateImages(AButton &button, int idx,
                            teBmps eEnabledUp,
                            teBmps eEnabledDown,
                            teBmps eDisabled);

   void ArrangeButtons();
   TranslatableString StateForStatusBar();

   enum
   {
      ID_PAUSE_BUTTON = 11000,
      ID_PLAY_BUTTON,
      ID_STOP_BUTTON,
      ID_FF_BUTTON,
      ID_REW_BUTTON,
      ID_RECORD_BUTTON,
      ID_LOOP_BUTTON,
      BUTTON_COUNT,
   };

   AButton *mRewind;
   AButton *mPlay;
   AButton *mRecord;
   AButton *mPause;
   AButton *mStop;
   AButton *mFF;
   AButton *mLoop;

   wxString mStrLocale; // standard locale abbreviation

   wxBoxSizer *mSizer;

 public:

   DECLARE_CLASS(ControlToolBar)
   DECLARE_EVENT_TABLE()
};

#endif

