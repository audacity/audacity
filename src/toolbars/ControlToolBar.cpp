/**********************************************************************

  Audacity: A Digital Audio Editor

  ControlToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
  James Crook
  Leland Lucius

*******************************************************************//**

\class ControlToolBar
\brief A ToolBar that has the main Transport buttons.

  In the GUI, this is referred to as "Transport Toolbar", as
  it corresponds to commands in the Transport menu.
  "Control Toolbar" is historic.
  This class, which is a child of Toolbar, creates the
  window containing the Transport (rewind/play/stop/record/ff)
  buttons. The window can be embedded within a
  normal project window, or within a ToolbarFrame that is
  managed by a global ToolBarStub called
  gControlToolBarStub.

  All of the controls in this window were custom-written for
  Audacity - they are not native controls on any platform -
  however, it is intended that the images could be easily
  replaced to allow "skinning" or just customization to
  match the look and feel of each platform.

*//*******************************************************************/

#include <algorithm>

#include "../Audacity.h"
#include "../Experimental.h"
#include "ControlToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/dc.h>
#include <wx/event.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/statusbr.h>
#include <wx/timer.h>
#endif
#include <wx/tooltip.h>
#include <wx/datetime.h>

#include "TranscriptionToolBar.h"
#include "MeterToolBar.h"

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../WaveTrack.h"
#include "../widgets/AButton.h"
#include "../widgets/Meter.h"

#include "../tracks/ui/Scrubbing.h"

IMPLEMENT_CLASS(ControlToolBar, ToolBar);

//static
AudacityProject *ControlToolBar::mBusyProject = NULL;

////////////////////////////////////////////////////////////
/// Methods for ControlToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(ControlToolBar, ToolBar)
   EVT_CHAR(ControlToolBar::OnKeyEvent)
   EVT_BUTTON(ID_PLAY_BUTTON,   ControlToolBar::OnPlay)
   EVT_BUTTON(ID_STOP_BUTTON,   ControlToolBar::OnStop)
   EVT_BUTTON(ID_RECORD_BUTTON, ControlToolBar::OnRecord)
   EVT_BUTTON(ID_REW_BUTTON,    ControlToolBar::OnRewind)
   EVT_BUTTON(ID_FF_BUTTON,     ControlToolBar::OnFF)
   EVT_BUTTON(ID_PAUSE_BUTTON,  ControlToolBar::OnPause)
END_EVENT_TABLE()

//Standard constructor
// This was called "Control" toolbar in the GUI before - now it is "Transport".
// Note that we use the legacy "Control" string as the section because this
// gets written to prefs and cannot be changed in prefs to maintain backwards
// compatibility
ControlToolBar::ControlToolBar()
: ToolBar(TransportBarID, _("Transport"), wxT("Control"))
{
   mPaused = false;

   gPrefs->Read(wxT("/GUI/ErgonomicTransportButtons"), &mErgonomicTransportButtons, true);
   mStrLocale = gPrefs->Read(wxT("/Locale/Language"), wxT(""));

   mSizer = NULL;
   mCutPreviewTracks = NULL;

   /* i18n-hint: These are strings for the status bar, and indicate whether Audacity
   is playing or recording or stopped, and whether it is paused. */
   mStatePlay = XO("Playing");
   mStateStop = XO("Stopped");
   mStateRecord = XO("Recording");
   mStatePause = XO("Paused");
}

ControlToolBar::~ControlToolBar()
{
}


void ControlToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
}

// This is a convenience function that allows for button creation in
// MakeButtons() with fewer arguments
AButton *ControlToolBar::MakeButton(teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
                                    int id,
                                    bool processdownevents,
                                    const wxChar *label)
{
   AButton *r = ToolBar::MakeButton(
      bmpRecoloredUpLarge, bmpRecoloredDownLarge, bmpRecoloredHiliteLarge,
      eEnabledUp, eEnabledDown, eDisabled,
      wxWindowID(id),
      wxDefaultPosition, processdownevents,
      theTheme.ImageSize( bmpRecoloredUpLarge ));
   r->SetLabel(label);
   r->SetFocusRect( r->GetRect().Deflate( 12, 12 ) );

   return r;
}

// static
void ControlToolBar::MakeAlternateImages(AButton &button, int idx,
                                         teBmps eEnabledUp,
                                         teBmps eEnabledDown,
                                         teBmps eDisabled)
{
   ToolBar::MakeAlternateImages(button, idx,
      bmpRecoloredUpLarge, bmpRecoloredDownLarge, bmpRecoloredHiliteLarge,
      eEnabledUp, eEnabledDown, eDisabled,
      theTheme.ImageSize( bmpRecoloredUpLarge ));
}

void ControlToolBar::Populate()
{
   MakeButtonBackgroundsLarge();

   mPause = MakeButton(bmpPause, bmpPause, bmpPauseDisabled,
      ID_PAUSE_BUTTON,  true,  _("Pause"));

   mPlay = MakeButton( bmpPlay, bmpPlay, bmpPlayDisabled,
      ID_PLAY_BUTTON, true, _("Play"));
   MakeAlternateImages(*mPlay, 1, bmpLoop, bmpLoop, bmpLoopDisabled);
   MakeAlternateImages(*mPlay, 2,
      bmpCutPreview, bmpCutPreview, bmpCutPreviewDisabled);
   MakeAlternateImages(*mPlay, 3,
                       bmpScrub, bmpScrub, bmpScrubDisabled);
   mPlay->FollowModifierKeys();

   mStop = MakeButton( bmpStop, bmpStop, bmpStopDisabled ,
      ID_STOP_BUTTON, false, _("Stop"));

   mRewind = MakeButton(bmpRewind, bmpRewind, bmpRewindDisabled,
      ID_REW_BUTTON, false, _("Skip to Start"));

   mFF = MakeButton(bmpFFwd, bmpFFwd, bmpFFwdDisabled,
      ID_FF_BUTTON, false, _("Skip to End"));

   mRecord = MakeButton(bmpRecord, bmpRecord, bmpRecordDisabled,
      ID_RECORD_BUTTON, true, _("Record"));
   MakeAlternateImages(*mRecord, 1, bmpAppendRecord, bmpAppendRecord,
      bmpAppendRecordDisabled);
   mRecord->FollowModifierKeys();

#if wxUSE_TOOLTIPS
   RegenerateToolsTooltips();
   wxToolTip::Enable(true);
   wxToolTip::SetDelay(1000);
#endif

   // Set default order and mode
   ArrangeButtons();
}

void ControlToolBar::RegenerateToolsTooltips()
{
#if wxUSE_TOOLTIPS
   for (long iWinID = ID_PLAY_BUTTON; iWinID < BUTTON_COUNT; iWinID++)
   {
      wxWindow* pCtrl = this->FindWindow(iWinID);
      wxString strToolTip = pCtrl->GetLabel();
      AudacityProject* pProj = GetActiveProject();
      CommandManager* pCmdMgr = (pProj) ? pProj->GetCommandManager() : NULL;
      if (pCmdMgr)
      {
         wxString strKey(wxT(" ("));
         switch (iWinID)
         {
            case ID_PLAY_BUTTON:
               strKey += pCmdMgr->GetKeyFromName(wxT("Play"));
               strKey += _(") / Loop Play (");
               strKey += pCmdMgr->GetKeyFromName(wxT("PlayLooped"));
               break;
            case ID_RECORD_BUTTON:
               strKey += pCmdMgr->GetKeyFromName(wxT("Record"));
               strKey += _(") / Append Record (");
               strKey += pCmdMgr->GetKeyFromName(wxT("RecordAppend"));
               break;
            case ID_PAUSE_BUTTON:
               strKey += pCmdMgr->GetKeyFromName(wxT("Pause"));
               break;
            case ID_STOP_BUTTON:
               strKey += pCmdMgr->GetKeyFromName(wxT("Stop"));
               break;
            case ID_FF_BUTTON:
               strKey += pCmdMgr->GetKeyFromName(wxT("SkipEnd"));
               break;
            case ID_REW_BUTTON:
               strKey += pCmdMgr->GetKeyFromName(wxT("SkipStart"));
               break;
         }
         strKey += wxT(")");
         strToolTip += strKey;
      }
      pCtrl->SetToolTip(strToolTip);
   }
#endif
}

void ControlToolBar::UpdatePrefs()
{
   bool updated = false;
   bool active;

   gPrefs->Read( wxT("/GUI/ErgonomicTransportButtons"), &active, true );
   if( mErgonomicTransportButtons != active )
   {
      mErgonomicTransportButtons = active;
      updated = true;
   }
   wxString strLocale = gPrefs->Read(wxT("/Locale/Language"), wxT(""));
   if (mStrLocale != strLocale)
   {
      mStrLocale = strLocale;
      updated = true;
   }

   if( updated )
   {
      ReCreateButtons(); // side effect: calls RegenerateToolsTooltips()
      Updated();
   }
   else
      // The other reason to regenerate tooltips is if keyboard shortcuts for
      // transport buttons changed, but that's too much work to check for, so just
      // always do it. (Much cheaper than calling ReCreateButtons() in all cases.
      RegenerateToolsTooltips();


   // Set label to pull in language change
   SetLabel(_("Transport"));

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void ControlToolBar::ArrangeButtons()
{
   int flags = wxALIGN_CENTER | wxRIGHT;

   // (Re)allocate the button sizer
   if( mSizer )
   {
      Detach( mSizer );
      delete mSizer;
   }

   Add((mSizer = safenew wxBoxSizer(wxHORIZONTAL)), 1, wxEXPAND);

   // Start with a little extra space
   mSizer->Add( 5, 55 );

   // Add the buttons in order based on ergonomic setting
   if( mErgonomicTransportButtons )
   {
      mPause->MoveBeforeInTabOrder( mRecord );
      mPlay->MoveBeforeInTabOrder( mRecord );
      mStop->MoveBeforeInTabOrder( mRecord );
      mRewind->MoveBeforeInTabOrder( mRecord );
      mFF->MoveBeforeInTabOrder( mRecord );

      mSizer->Add( mPause,  0, flags, 2 );
      mSizer->Add( mPlay,   0, flags, 2 );
      mSizer->Add( mStop,   0, flags, 2 );
      mSizer->Add( mRewind, 0, flags, 2 );
      mSizer->Add( mFF,     0, flags, 10 );
      mSizer->Add( mRecord, 0, flags, 5 );
   }
   else
   {
      mRewind->MoveBeforeInTabOrder( mFF );
      mPlay->MoveBeforeInTabOrder( mFF );
      mRecord->MoveBeforeInTabOrder( mFF );
      mPause->MoveBeforeInTabOrder( mFF );
      mStop->MoveBeforeInTabOrder( mFF );

      mSizer->Add( mRewind, 0, flags, 2 );
      mSizer->Add( mPlay,   0, flags, 2 );
      mSizer->Add( mRecord, 0, flags, 2 );
      mSizer->Add( mPause,  0, flags, 2 );
      mSizer->Add( mStop,   0, flags, 2 );
      mSizer->Add( mFF,     0, flags, 5 );
   }

   // Layout the sizer
   mSizer->Layout();

   // Layout the toolbar
   Layout();

   // (Re)Establish the minimum size
   SetMinSize( GetSizer()->GetMinSize() );
}

void ControlToolBar::ReCreateButtons()
{
   bool playDown = false;
   bool playShift = false;
   bool pauseDown = false;
   bool recordDown = false;
   bool recordShift = false;

   // ToolBar::ReCreateButtons() will get rid of its sizer and
   // since we've attached our sizer to it, ours will get deleted too
   // so clean ours up first.
   if( mSizer )
   {
      playDown = mPlay->IsDown();
      playShift = mPlay->WasShiftDown();
      pauseDown = mPause->IsDown();
      recordDown = mRecord->IsDown();
      recordShift = mRecord->WasShiftDown();
      Detach( mSizer );

      delete mSizer;
      mSizer = NULL;
   }

   ToolBar::ReCreateButtons();

   if (playDown)
   {
      ControlToolBar::PlayAppearance appearance =
         playShift ? ControlToolBar::PlayAppearance::Looped
         : ControlToolBar::PlayAppearance::Straight;
      SetPlay(playDown, appearance);
   }

   if (pauseDown)
   {
      mPause->PushDown();
   }

   if (recordDown)
   {
      SetRecord(recordDown, recordShift);
   }

   EnableDisableButtons();

   RegenerateToolsTooltips();
}

void ControlToolBar::Repaint( wxDC *dc )
{
#ifndef USE_AQUA_THEME
   wxSize s = mSizer->GetSize();
   wxPoint p = mSizer->GetPosition();

   wxRect bevelRect( p.x, p.y, s.GetWidth() - 1, s.GetHeight() - 1 );
   AColor::Bevel( *dc, true, bevelRect );
#endif
}

void ControlToolBar::EnableDisableButtons()
{
   AudacityProject *p = GetActiveProject();
   bool tracks = false;

   bool playing = mPlay->IsDown();
   bool recording = mRecord->IsDown();
   bool notBusy = !gAudioIO->IsBusy();

   // Only interested in audio type tracks
   if (p) {
      TrackListIterator iter( p->GetTracks() );
      for (Track *t = iter.First(); t; t = iter.Next()) {
         if (t->GetKind() == Track::Wave
#if defined(USE_MIDI)
         || t->GetKind() == Track::Note
#endif
         ) {
            tracks = true;
            break;
         }
      }
   }

   if (p) {
      TranscriptionToolBar *const playAtSpeedTB = p->GetTranscriptionToolBar();
      if (playAtSpeedTB)
         playAtSpeedTB->SetEnabled(CanStopAudioStream() && tracks && !recording);
   }

   mPlay->SetEnabled(CanStopAudioStream() && tracks && !recording);
   mRecord->SetEnabled(CanStopAudioStream() && notBusy && !playing);
   mStop->SetEnabled(CanStopAudioStream() && (playing || recording));
   mRewind->SetEnabled(!playing && !recording);
   mFF->SetEnabled(tracks && !playing && !recording);

   auto pProject = GetActiveProject();
   mPause->SetEnabled(CanStopAudioStream() &&
                      !(pProject &&
                        pProject->GetScrubber().HasStartedScrubbing()));
}

void ControlToolBar::SetPlay(bool down, PlayAppearance appearance)
{
   if (down) {
      mPlay->SetShift(appearance == PlayAppearance::Looped);
      mPlay->SetControl(appearance == PlayAppearance::CutPreview);
      mPlay->SetAlternateIdx(static_cast<int>(appearance));
      mPlay->PushDown();
   }
   else {
      mPlay->PopUp();
      mPlay->SetAlternateIdx(0);
   }
   EnableDisableButtons();
   UpdateStatusBar(GetActiveProject());
}

void ControlToolBar::SetStop(bool down)
{
   if (down)
      mStop->PushDown();
   else {
      if(FindFocus() == mStop)
         mPlay->SetFocus();
      mStop->PopUp();
   }
   EnableDisableButtons();
}

void ControlToolBar::SetRecord(bool down, bool append)
{
   if (down)
   {
      mRecord->SetAlternateIdx(append ? 1 : 0);
      mRecord->PushDown();
   }
   else
   {
      mRecord->SetAlternateIdx(0);
      mRecord->PopUp();
   }
   EnableDisableButtons();
}

bool ControlToolBar::IsRecordDown()
{
   return mRecord->IsDown();
}

int ControlToolBar::PlayPlayRegion(const SelectedRegion &selectedRegion,
                                   const AudioIOStartStreamOptions &options,
                                   PlayMode mode,
                                   PlayAppearance appearance, /* = PlayOption::Straight */
                                   bool backwards, /* = false */
                                   bool playWhiteSpace /* = false */)
{
   if (!CanStopAudioStream())
      return -1;

   // Uncomment this for laughs!
   // backwards = true;

   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();
   // SelectedRegion guarantees t0 <= t1, so we need another boolean argument
   // to indicate backwards play.
   const bool looped = options.playLooped;

   if (backwards)
      std::swap(t0, t1);

   SetPlay(true, appearance);

   if (gAudioIO->IsBusy()) {
      SetPlay(false);
      return -1;
   }

   const bool cutpreview = appearance == PlayAppearance::CutPreview;
   if (cutpreview && t0==t1) {
      SetPlay(false);
      return -1; /* msmeyer: makes no sense */
   }

   AudacityProject *p = GetActiveProject();
   if (!p) {
      SetPlay(false);
      return -1;  // Should never happen, but...
   }

   TrackList *t = p->GetTracks();
   if (!t) {
      mPlay->PopUp();
      return -1;  // Should never happen, but...
   }

   p->mLastPlayMode = mode;

   bool hasaudio = false;
   TrackListIterator iter(t);
   for (Track *trk = iter.First(); trk; trk = iter.Next()) {
      if (trk->GetKind() == Track::Wave
#ifdef EXPERIMENTAL_MIDI_OUT
         || trk->GetKind() == Track::Note
#endif
         ) {
         hasaudio = true;
         break;
      }
   }

   double latestEnd = (playWhiteSpace)? t1 : t->GetEndTime();

   if (!hasaudio) {
      SetPlay(false);
      return -1;  // No need to continue without audio tracks
   }

#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
   double init_seek = 0.0;
#endif

   if (t1 == t0) {
      if (looped) {
         // play selection if there is one, otherwise
         // set start of play region to project start, 
         // and loop the project from current play position.

         if ((t0 > p->GetSel0()) && (t0 < p->GetSel1())) {
            t0 = p->GetSel0();
            t1 = p->GetSel1();
         }
         else {
            // loop the entire project
            t0 = t->GetStartTime();
            t1 = t->GetEndTime();
         }
      } else {
         // move t0 to valid range
         if (t0 < 0) {
            t0 = t->GetStartTime();
         }
         else if (t0 > t->GetEndTime()) {
            t0 = t->GetEndTime();
         }
#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
         else {
            init_seek = t0;         //AC: init_seek is where playback will 'start'
            t0 = t->GetStartTime();
         }
#endif
      }
      t1 = t->GetEndTime();
   }
   else {
      // maybe t1 < t0, with backwards scrubbing for instance
      if (backwards)
         std::swap(t0, t1);

      t0 = std::max(0.0, std::min(t0, latestEnd));
      t1 = std::max(0.0, std::min(t1, latestEnd));

      if (backwards)
         std::swap(t0, t1);
   }

   int token = -1;
   bool success = false;
   if (t1 != t0) {
      if (cutpreview) {
         const double tless = std::min(t0, t1);
         const double tgreater = std::max(t0, t1);
         double beforeLen, afterLen;
         gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);
         gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);
         double tcp0 = tless-beforeLen;
         double diff = tgreater - tless;
         double tcp1 = (tgreater+afterLen) - diff;
         SetupCutPreviewTracks(tcp0, tless, tgreater, tcp1);
         if (backwards)
            std::swap(tcp0, tcp1);
         if (mCutPreviewTracks)
         {
            AudioIOStartStreamOptions myOptions = options;
            myOptions.cutPreviewGapStart = t0;
            myOptions.cutPreviewGapLen = t1 - t0;
            token = gAudioIO->StartStream(
               mCutPreviewTracks->GetWaveTrackArray(false),
               WaveTrackArray(),
#ifdef EXPERIMENTAL_MIDI_OUT
               NoteTrackArray(),
#endif
               p->GetRate(), tcp0, tcp1, myOptions);
         } else
         {
            // Cannot create cut preview tracks, clean up and exit
            SetPlay(false);
            SetStop(false);
            SetRecord(false);
            return -1;
         }
      } else {
         // Lifted the following into AudacityProject::GetDefaultPlayOptions()
         /*
         if (!timetrack) {
            timetrack = t->GetTimeTrack();
         }
         */
         token = gAudioIO->StartStream(t->GetWaveTrackArray(false),
                                       WaveTrackArray(),
#ifdef EXPERIMENTAL_MIDI_OUT
                                       t->GetNoteTrackArray(false),
#endif
                                       p->GetRate(), t0, t1, options);
      }
      if (token != 0) {
         success = true;
         p->SetAudioIOToken(token);
         mBusyProject = p;
#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
         //AC: If init_seek was set, now's the time to make it happen.
         gAudioIO->SeekStream(init_seek);
#endif
      }
      else {
         // msmeyer: Show error message if stream could not be opened
         wxMessageBox(
            _("Error while opening sound device. "
            "Please check the playback device settings and the project sample rate."),
            _("Error"), wxOK | wxICON_EXCLAMATION, this);
      }
   }

   if (!success) {
      SetPlay(false);
      SetStop(false);
      SetRecord(false);
      return -1;
   }

   return token;
}

void ControlToolBar::PlayCurrentRegion(bool looped /* = false */,
                                       bool cutpreview /* = false */)
{
   if (!CanStopAudioStream())
      return;

   AudacityProject *p = GetActiveProject();

   if (p)
   {

      double playRegionStart, playRegionEnd;
      p->GetPlayRegion(&playRegionStart, &playRegionEnd);

      AudioIOStartStreamOptions options(p->GetDefaultPlayOptions());
      options.playLooped = looped;
      if (cutpreview)
         options.timeTrack = NULL;
      ControlToolBar::PlayAppearance appearance =
        cutpreview ? ControlToolBar::PlayAppearance::CutPreview
           : looped ? ControlToolBar::PlayAppearance::Looped
           : ControlToolBar::PlayAppearance::Straight;
      PlayPlayRegion(SelectedRegion(playRegionStart, playRegionEnd),
                     options,
                     (looped ? PlayMode::loopedPlay : PlayMode::normalPlay),
                     appearance);
   }
}

void ControlToolBar::OnKeyEvent(wxKeyEvent & event)
{
   if (event.ControlDown() || event.AltDown()) {
      event.Skip();
      return;
   }

   // Does not appear to be needed on Linux. Perhaps on some other platform?
   // If so, "!CanStopAudioStream()" should probably apply.
   if (event.GetKeyCode() == WXK_SPACE) {
      if (gAudioIO->IsStreamActive(GetActiveProject()->GetAudioIOToken())) {
         SetPlay(false);
         SetStop(true);
         StopPlaying();
      }
      else if (!gAudioIO->IsBusy()) {
         //SetPlay(true);// Not needed as done in PlayPlayRegion
         SetStop(false);
         PlayCurrentRegion();
      }
      return;
   }
   event.Skip();
}

void ControlToolBar::OnPlay(wxCommandEvent & WXUNUSED(evt))
{
   if (!CanStopAudioStream())
      return;

   StopPlaying();

   AudacityProject *p = GetActiveProject();
   if (p) p->TP_DisplaySelection();

   PlayDefault();
   UpdateStatusBar(GetActiveProject());
}

void ControlToolBar::OnStop(wxCommandEvent & WXUNUSED(evt))
{
   if (CanStopAudioStream()) {
      StopPlaying();
      UpdateStatusBar(GetActiveProject());
   }
}

bool ControlToolBar::CanStopAudioStream()
{
   return (!gAudioIO->IsStreamActive() ||
           gAudioIO->IsMonitoring() ||
           gAudioIO->GetOwningProject() == GetActiveProject());
}

void ControlToolBar::PlayDefault()
{
   // Let control have precedence over shift
   const bool cutPreview = mPlay->WasControlDown();
   const bool looped = !cutPreview &&
      mPlay->WasShiftDown();
   PlayCurrentRegion(looped, cutPreview);
}

void ControlToolBar::StopPlaying(bool stopStream /* = true*/)
{
   AudacityProject *project = GetActiveProject();

   if(project)
      // Let scrubbing code do some appearance change
      project->GetScrubber().StopScrubbing();

   if (!CanStopAudioStream())
      return;

   mStop->PushDown();

   SetStop(false);
   if(stopStream)
      gAudioIO->StopStream();
   SetPlay(false);
   SetRecord(false);

   #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
      gAudioIO->AILADisable();
   #endif

   mPause->PopUp();
   mPaused=false;
   //Make sure you tell gAudioIO to unpause
   gAudioIO->SetPaused(mPaused);

   ClearCutPreviewTracks();

   mBusyProject = NULL;
   // So that we continue monitoring after playing or recording.
   // also clean the MeterQueues
   if( project ) {
      project->MayStartMonitoring();

      Meter *meter = project->GetPlaybackMeter();
      if( meter ) {
         meter->Clear();
      }
      
      meter = project->GetCaptureMeter();
      if( meter ) {
         meter->Clear();
      }
   }
}

void ControlToolBar::Pause()
{
   if (!CanStopAudioStream())
      gAudioIO->SetPaused(!gAudioIO->IsPaused());
   else {
      wxCommandEvent dummy;
      OnPause(dummy);
   }
}

void ControlToolBar::OnRecord(wxCommandEvent &evt)
{
   if (gAudioIO->IsBusy()) {
      mRecord->PopUp();
      return;
   }
   AudacityProject *p = GetActiveProject();

   if( evt.GetInt() == 1 ) // used when called by keyboard shortcut. Default (0) ignored.
      mRecord->SetShift(true);
   if( evt.GetInt() == 2 )
      mRecord->SetShift(false);

   SetRecord(true, mRecord->WasShiftDown());

   if (p) {
      TrackList *trackList = p->GetTracks();
      TrackListIterator it(trackList);
      if(it.First() == NULL)
         mRecord->SetShift(false);
      double t0 = p->GetSel0();
      double t1 = p->GetSel1();
      if (t1 == t0)
         t1 = 1000000000.0;     // record for a long, long time (tens of years)

      /* TODO: set up stereo tracks if that is how the user has set up
       * their preferences, and choose sample format based on prefs */
      WaveTrackArray newRecordingTracks, playbackTracks;
#ifdef EXPERIMENTAL_MIDI_OUT
      NoteTrackArray midiTracks;
#endif
      bool duplex;
      gPrefs->Read(wxT("/AudioIO/Duplex"), &duplex, true);

      if(duplex){
         playbackTracks = trackList->GetWaveTrackArray(false);
#ifdef EXPERIMENTAL_MIDI_OUT
         midiTracks = trackList->GetNoteTrackArray(false);
#endif
     }
      else {
         playbackTracks = WaveTrackArray();
#ifdef EXPERIMENTAL_MIDI_OUT
         midiTracks = NoteTrackArray();
#endif
     }

      // If SHIFT key was down, the user wants append to tracks
      int recordingChannels = 0;
      TrackList tracksCopy{};
      bool tracksCopied = false;
      bool shifted = mRecord->WasShiftDown();
      if (shifted) {
         bool sel = false;
         double allt0 = t0;

         // Find the maximum end time of selected and all wave tracks
         // Find whether any tracks were selected.  (If any are selected,
         // record only into them; else if tracks exist, record into all.)
         for (Track *tt = it.First(); tt; tt = it.Next()) {
            if (tt->GetKind() == Track::Wave) {
               WaveTrack *wt = static_cast<WaveTrack *>(tt);
               if (wt->GetEndTime() > allt0) {
                  allt0 = wt->GetEndTime();
               }

               if (tt->GetSelected()) {
                  sel = true;
                  if (wt->GetEndTime() > t0) {
                     t0 = wt->GetEndTime();
                  }
               }
            }
         }

         // Use end time of all wave tracks if none selected
         if (!sel) {
            t0 = allt0;
         }

         // Pad selected/all wave tracks to make them all the same length
         // Remove recording tracks from the list of tracks for duplex ("overdub")
         // playback.
         for (Track *tt = it.First(); tt; tt = it.Next()) {
            if (tt->GetKind() == Track::Wave && (tt->GetSelected() || !sel)) {
               WaveTrack *wt = static_cast<WaveTrack *>(tt);
               if (duplex) {
                  auto end = playbackTracks.end();
                  auto it = std::find(playbackTracks.begin(), end, wt);
                  if (it != end)
                     playbackTracks.erase(it);
               }
               t1 = wt->GetEndTime();
               if (t1 < t0) {
                  if (!tracksCopied) {
                     tracksCopied = true;
                     tracksCopy = *trackList;
                  }

                  auto newTrack = p->GetTrackFactory()->NewWaveTrack();
                  newTrack->InsertSilence(0.0, t0 - t1);
                  newTrack->Flush();
                  wt->Clear(t1, t0);
                  bool bResult = wt->Paste(t1, newTrack.get());
                  wxASSERT(bResult); // TO DO: Actually handle this.
                  wxUnusedVar(bResult);
               }
               newRecordingTracks.push_back(wt);
            }
         }

         t1 = 1000000000.0;     // record for a long, long time (tens of years)
      }
      else {
         bool recordingNameCustom, useTrackNumber, useDateStamp, useTimeStamp;
         wxString defaultTrackName, defaultRecordingTrackName;
         int numTracks = 0;

         for (Track *tt = it.First(); tt; tt = it.Next()) {
            if (tt->GetKind() == Track::Wave && !tt->GetLinked())
               numTracks++;
         }
         numTracks++;
         
         recordingChannels = gPrefs->Read(wxT("/AudioIO/RecordChannels"), 2);

         gPrefs->Read(wxT("/GUI/TrackNames/RecordingNameCustom"), &recordingNameCustom, false);
         gPrefs->Read(wxT("/GUI/TrackNames/TrackNumber"), &useTrackNumber, false);
         gPrefs->Read(wxT("/GUI/TrackNames/DateStamp"), &useDateStamp, false);
         gPrefs->Read(wxT("/GUI/TrackNames/TimeStamp"), &useTimeStamp, false);
         /* i18n-hint: The default name for an audio track. */
         gPrefs->Read(wxT("/GUI/TrackNames/DefaultTrackName"),&defaultTrackName, _("Audio Track"));
         gPrefs->Read(wxT("/GUI/TrackNames/RecodingTrackName"), &defaultRecordingTrackName, defaultTrackName);

         wxString baseTrackName = recordingNameCustom? defaultRecordingTrackName : defaultTrackName;

         for (int c = 0; c < recordingChannels; c++) {
            auto newTrack = p->GetTrackFactory()->NewWaveTrack();

            newTrack->SetOffset(t0);
            wxString nameSuffix = wxString(wxT(""));

            if (useTrackNumber) {
               nameSuffix += wxString::Format(wxT("%d"), numTracks + c);
            }

            if (useDateStamp) {
               if (!nameSuffix.IsEmpty()) {
                  nameSuffix += wxT("_");
               }
               nameSuffix += wxDateTime::Now().FormatISODate();
            }

            if (useTimeStamp) {
               if (!nameSuffix.IsEmpty()) {
                  nameSuffix += wxT("_");
               }
               nameSuffix += wxDateTime::Now().FormatISOTime();
            }

            // ISO standard would be nice, but ":" is unsafe for file name.
            nameSuffix.Replace(wxT(":"), wxT("-"));

            if (baseTrackName.IsEmpty()) {
               newTrack->SetName(nameSuffix);
            }
            else if (nameSuffix.IsEmpty()) {
               newTrack->SetName(baseTrackName);
            }
            else {
               newTrack->SetName(baseTrackName + wxT("_") + nameSuffix);
            }

            if (recordingChannels > 2)
              newTrack->SetMinimized(true);

            if (recordingChannels == 2) {
               if (c == 0) {
                  newTrack->SetChannel(Track::LeftChannel);
                  newTrack->SetLinked(true);
               }
               else {
                  newTrack->SetChannel(Track::RightChannel);
               }
            }
            else {
               newTrack->SetChannel( Track::MonoChannel );
            }

            // Let the list hold the track, and keep a pointer to it
            newRecordingTracks.push_back(
               static_cast<WaveTrack*>(
                  trackList->Add(
                     std::move(newTrack))));
         }
      }

      //Automated Input Level Adjustment Initialization
      #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
         gAudioIO->AILAInitialize();
      #endif

      AudioIOStartStreamOptions options(p->GetDefaultPlayOptions());
      int token = gAudioIO->StartStream(playbackTracks,
                                        newRecordingTracks,
#ifdef EXPERIMENTAL_MIDI_OUT
                                        midiTracks,
#endif
                                        p->GetRate(), t0, t1, options);

      bool success = (token != 0);

      if (success) {
         p->SetAudioIOToken(token);
         mBusyProject = p;
      }
      else {
         if (shifted) {
            // Restore the tracks to remove any inserted silence
            if (tracksCopied)
               *trackList = std::move(tracksCopy);
         }
         else {
            // msmeyer: Delete recently added tracks if opening stream fails
            for (unsigned int i = 0; i < newRecordingTracks.size(); i++) {
               trackList->Remove(newRecordingTracks[i]);
            }
         }

         // msmeyer: Show error message if stream could not be opened
         wxMessageBox(_("Error while opening sound device. Please check the recording device settings and the project sample rate."),
                      _("Error"), wxOK | wxICON_EXCLAMATION, this);

         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }
   }
   UpdateStatusBar(GetActiveProject());
}


void ControlToolBar::OnPause(wxCommandEvent & WXUNUSED(evt))
{
   if (!CanStopAudioStream()) {
      return;
   }

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   if (gAudioIO->IsScrubbing())
      // Pausing does not make sense.  Force the button
      // to pop up below.
      mPaused = true;
#endif

   if(mPaused)
   {
      mPause->PopUp();
      mPaused=false;
   }
   else
   {
      mPause->PushDown();
      mPaused=true;
   }

   gAudioIO->SetPaused(mPaused);
   UpdateStatusBar(GetActiveProject());
}

void ControlToolBar::OnRewind(wxCommandEvent & WXUNUSED(evt))
{
   mRewind->PushDown();
   mRewind->PopUp();

   AudacityProject *p = GetActiveProject();
   if (p) {
      p->Rewind(mRewind->WasShiftDown());
   }
}

void ControlToolBar::OnFF(wxCommandEvent & WXUNUSED(evt))
{
   mFF->PushDown();
   mFF->PopUp();

   AudacityProject *p = GetActiveProject();

   if (p) {
      p->SkipEnd(mFF->WasShiftDown());
   }
}

void ControlToolBar::SetupCutPreviewTracks(double WXUNUSED(playStart), double cutStart,
                                           double cutEnd, double  WXUNUSED(playEnd))
{
   ClearCutPreviewTracks();
   AudacityProject *p = GetActiveProject();
   if (p) {
      // Find first selected track (stereo or mono) and duplicate it
      Track *track1 = NULL, *track2 = NULL;
      TrackListIterator it(p->GetTracks());
      for (Track *t = it.First(); t; t = it.Next())
      {
         if (t->GetKind() == Track::Wave && t->GetSelected())
         {
            track1 = t;
            track2 = t->GetLink();
            break;
         }
      }

      if (track1)
      {
         // Duplicate and change tracks
         auto new1 = track1->Duplicate();
         new1->Clear(cutStart, cutEnd);
         decltype(new1) new2{};
         if (track2)
         {
            new2 = track2->Duplicate();
            new2->Clear(cutStart, cutEnd);
         }

         mCutPreviewTracks = new TrackList();
         mCutPreviewTracks->Add(std::move(new1));
         if (track2)
            mCutPreviewTracks->Add(std::move(new2));
      }
   }
}

void ControlToolBar::ClearCutPreviewTracks()
{
   if (mCutPreviewTracks)
   {
      mCutPreviewTracks->Clear(); /* DELETE track contents too */
      delete mCutPreviewTracks;
      mCutPreviewTracks = NULL;
   }
}

// works out the width of the field in the status bar needed for the state (eg play, record pause)
int ControlToolBar::WidthForStatusBar(wxStatusBar* const sb)
{
   int xMax = 0;
   const auto pauseString = wxT(" ") + wxGetTranslation(mStatePause);

   auto update = [&] (const wxString &state, bool pauseToo) {
      int x, y;
      sb->GetTextExtent(
         wxGetTranslation(state) + ( pauseToo ? pauseString : wxString{} ) + wxT("."),
         &x, &y
      );
      xMax = std::max(x, xMax);
   };

   update(mStatePlay, true);
   update(mStateStop, true);
   update(mStateRecord, true);

   // Note that Scrubbing + Paused is not allowed.
   for(const auto &state : Scrubber::GetAllUntranslatedStatusStrings())
      update(state, false);

   return xMax + 30;    // added constant needed because xMax isn't large enough for some reason, plus some space.
}

wxString ControlToolBar::StateForStatusBar()
{
   wxString state;

   auto pProject = GetActiveProject();
   auto scrubState =
      pProject ? pProject->GetScrubber().GetUntranslatedStateString() : wxString();
   if (!scrubState.IsEmpty())
      state = wxGetTranslation(scrubState);
   else if (mPlay->IsDown())
      state = wxGetTranslation(mStatePlay);
   else if (mRecord->IsDown())
      state = wxGetTranslation(mStateRecord);
   else
      state = wxGetTranslation(mStateStop);

   if (mPause->IsDown())
   {
      state.Append(wxT(" "));
      state.Append(wxGetTranslation(mStatePause));
   }

   state.Append(wxT("."));

   return state;
}

void ControlToolBar::UpdateStatusBar(AudacityProject *pProject)
{
   pProject->GetStatusBar()->SetStatusText(StateForStatusBar(), stateStatusBarField);
}

