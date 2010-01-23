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
#include "../Theme.h"

class wxBoxSizer;
class wxCommandEvent;
class wxDC;
class wxKeyEvent;
class wxWindow;

class AButton;
class AudacityProject;
class TrackList;
class TimeTrack;

class ControlToolBar:public ToolBar {

 public:

   ControlToolBar();
   virtual ~ControlToolBar();

   void Create(wxWindow *parent);

   void UpdatePrefs();
   virtual void OnKeyEvent(wxKeyEvent & event);

   // msmeyer: These are public, but it's far better to
   // call the "real" interface functions like PlayCurrentRegion() and
   // StopPlaying() which are defined below.
   void OnRewind(wxCommandEvent & evt);
   void OnPlay(wxCommandEvent & evt);
   void OnStop(wxCommandEvent & evt);
   void OnBatch(wxCommandEvent & evt);
   void OnRecord(wxCommandEvent & evt);
   void OnFF(wxCommandEvent & evt);
   void OnPause(wxCommandEvent & evt);

   //These allow buttons to be controlled externally:
   void SetPlay(bool down);
   void SetStop(bool down);
   void SetRecord(bool down);

   bool IsRecordDown();

   // Play currently selected region, or if nothing selected,
   // play from current cursor.
   void PlayCurrentRegion(bool looped = false, bool cutpreview = false);
   // Play the region [t0,t1]
   void PlayPlayRegion(double t0, double t1,
                       bool looped = false,
                       bool cutpreview = false,
                       TimeTrack *timetrack = NULL);
   void PlayDefault();
   
   // Stop playing
   void StopPlaying(bool stopStream = true);

   void Populate();
   virtual void Repaint(wxDC *dc);
   virtual void EnableDisableButtons();
   void OnKeyDown(wxKeyEvent & event);
   void OnKeyUp(wxKeyEvent & event);

   void SetVUMeters(AudacityProject *p);

   virtual void ReCreateButtons();

 private:

   AButton *MakeButton(teBmps eFore, teBmps eDisabled,
      int id,
      bool processdownevents,
      const wxChar *label);
   void MakeLoopImage();
   void ArrangeButtons();
   void RegenerateToolsTooltips();
   void SetupCutPreviewTracks(double playStart, double cutStart,
                             double cutEnd, double playEnd);
   void ClearCutPreviewTracks();

   enum
   {
      ID_PLAY_BUTTON,
      ID_RECORD_BUTTON,
      ID_PAUSE_BUTTON,
      ID_STOP_BUTTON,
      ID_FF_BUTTON,
      ID_REW_BUTTON,
      ID_BATCH_BUTTON,

      BUTTON_COUNT
   };

   AButton *mRewind;
   AButton *mPlay;
   AButton *mBatch;
   AButton *mRecord;
   AButton *mPause;
   AButton *mStop;
   AButton *mFF;

   static AudacityProject *mBusyProject;
   // Maybe button state values shouldn't be duplicated in this toolbar?
   bool mPaused;         //Play or record is paused or not paused?

   // Activate ergonomic order for transport buttons
   bool mErgonomicTransportButtons;

   // Show/hide cleanspeech button
   bool mCleanSpeechMode;

   wxBoxSizer *mBatchGroup;
   wxBoxSizer *mSizer;

   TrackList* mCutPreviewTracks;

 public:

   DECLARE_CLASS(ControlToolBar);
   DECLARE_EVENT_TABLE();
};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: bb2858b8-2c70-48df-9d72-bcdef94be4e3

