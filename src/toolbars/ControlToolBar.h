/**********************************************************************

  Audacity: A Digital Audio Editor


  ControlToolbar.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_CONTROL_TOOLBAR__
#define __AUDACITY_CONTROL_TOOLBAR__

#include "../MemoryX.h"
#include "ToolBar.h"
#include "../Theme.h"

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
class TrackList;
class TimeTrack;

struct AudioIOStartStreamOptions;
class SelectedRegion;

// Defined in Project.h
enum class PlayMode : int;

// In the GUI, ControlToolBar appears as the "Transport Toolbar". "Control Toolbar" is historic.
class ControlToolBar final : public ToolBar {

 public:

   ControlToolBar();
   virtual ~ControlToolBar();

   void Create(wxWindow *parent);

   void UpdatePrefs();
   void OnKeyEvent(wxKeyEvent & event);

   // msmeyer: These are public, but it's far better to
   // call the "real" interface functions like PlayCurrentRegion() and
   // StopPlaying() which are defined below.
   void OnRewind(wxCommandEvent & evt);
   void OnPlay(wxCommandEvent & evt);
   void OnStop(wxCommandEvent & evt);
   void OnRecord(wxCommandEvent & evt);
   void OnFF(wxCommandEvent & evt);
   void OnPause(wxCommandEvent & evt);

   // Choice among the appearances of the play button:
   enum class PlayAppearance {
      Straight, Looped, CutPreview, Scrub, Seek
   };

   //These allow buttons to be controlled externally:
   void SetPlay(bool down, PlayAppearance appearance = PlayAppearance::Straight);
   void SetStop(bool down);
   void SetRecord(bool down, bool append=false);

   bool IsPauseDown() const;
   bool IsRecordDown() const;

   // A project is only allowed to stop an audio stream that it owns.
   bool CanStopAudioStream ();

   // Play currently selected region, or if nothing selected,
   // play from current cursor.
   void PlayCurrentRegion(bool looped = false, bool cutpreview = false);
   // Play the region [t0,t1]
   // Return the Audio IO token or -1 for failure
   int PlayPlayRegion(const SelectedRegion &selectedRegion,
                      const AudioIOStartStreamOptions &options,
                      PlayMode playMode,
                      PlayAppearance appearance = PlayAppearance::Straight,
                      bool backwards = false,
                      // Allow t0 and t1 to be beyond end of tracks
                      bool playWhiteSpace = false);
   void PlayDefault();

   // Stop playing
   void StopPlaying(bool stopStream = true);

   // Pause - used by AudioIO to pause sound activate recording
   void Pause();

   void Populate();
   void Repaint(wxDC *dc) override;
   void EnableDisableButtons() override;

   void ReCreateButtons() override;
   void RegenerateTooltips() override;

   int WidthForStatusBar(wxStatusBar* const);
   void UpdateStatusBar(AudacityProject *pProject);

   // Starting and stopping of scrolling display
   void StartScrollingIfPreferred();
   void StartScrolling();
   void StopScrolling();

 private:

   AButton *MakeButton(teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
      int id,
      bool processdownevents,
      const wxChar *label);

   static
   void MakeAlternateImages(AButton &button, int idx,
                            teBmps eEnabledUp,
                            teBmps eEnabledDown,
                            teBmps eDisabled);

   void ArrangeButtons();
   void SetupCutPreviewTracks(double playStart, double cutStart,
                             double cutEnd, double playEnd);
   void ClearCutPreviewTracks();
   wxString StateForStatusBar();

   enum
   {
      ID_PLAY_BUTTON = 11000,
      ID_RECORD_BUTTON,
      ID_PAUSE_BUTTON,
      ID_STOP_BUTTON,
      ID_FF_BUTTON,
      ID_REW_BUTTON,
      BUTTON_COUNT,
   };

   AButton *mRewind;
   AButton *mPlay;
   AButton *mRecord;
   AButton *mPause;
   AButton *mStop;
   AButton *mFF;

   static AudacityProject *mBusyProject;

   // Maybe button state values shouldn't be duplicated in this toolbar?
   bool mPaused;         //Play or record is paused or not paused?

   // Activate ergonomic order for transport buttons
   bool mErgonomicTransportButtons;

   wxString mStrLocale; // standard locale abbreviation

   wxBoxSizer *mSizer;

   std::unique_ptr<TrackList> mCutPreviewTracks;

   // strings for status bar
   wxString mStatePlay;
   wxString mStateStop;
   wxString mStateRecord;
   wxString mStatePause;

 public:

   DECLARE_CLASS(ControlToolBar)
   DECLARE_EVENT_TABLE()
};

#endif

