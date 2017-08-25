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
#include <cfloat>

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
#include "../widgets/LinkingHtmlWindow.h"
#include "../widgets/ErrorDialog.h"
#include "../FileNames.h"

#include "../tracks/ui/Scrubbing.h"
#include "../prefs/TracksPrefs.h"
#include "../toolbars/ToolManager.h"

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
   AButton *r = ToolBar::MakeButton(this,
      bmpRecoloredUpLarge, bmpRecoloredDownLarge, bmpRecoloredHiliteLarge,
      eEnabledUp, eEnabledDown, eDisabled,
      wxWindowID(id),
      wxDefaultPosition, processdownevents,
      theTheme.ImageSize( bmpRecoloredUpLarge ));
   r->SetLabel(label);
   enum { deflation =
#ifdef __WXMAC__
      6
#else
      12
#endif
   };
   r->SetFocusRect( r->GetClientRect().Deflate( deflation, deflation ) );

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
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
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
   MakeAlternateImages(*mPlay, 4,
                       bmpSeek, bmpSeek, bmpSeekDisabled);
   mPlay->FollowModifierKeys();

   mStop = MakeButton( bmpStop, bmpStop, bmpStopDisabled ,
      ID_STOP_BUTTON, false, _("Stop"));

   mRewind = MakeButton(bmpRewind, bmpRewind, bmpRewindDisabled,
      ID_REW_BUTTON, false, _("Skip to Start"));

   mFF = MakeButton(bmpFFwd, bmpFFwd, bmpFFwdDisabled,
      ID_FF_BUTTON, false, _("Skip to End"));

   mRecord = MakeButton(bmpRecord, bmpRecord, bmpRecordDisabled,
      ID_RECORD_BUTTON, true, _("Record"));

   bool bPreferNewTrack;
   gPrefs->Read("/GUI/PreferNewTrackRecord",&bPreferNewTrack, false);
   if( !bPreferNewTrack )
      MakeAlternateImages(*mRecord, 1, bmpRecordBelow, bmpRecordBelow,
         bmpRecordBelowDisabled);
   else
      MakeAlternateImages(*mRecord, 1, bmpRecordBeside, bmpRecordBeside,
         bmpRecordBesideDisabled);

   mRecord->FollowModifierKeys();

#if wxUSE_TOOLTIPS
   RegenerateTooltips();
   wxToolTip::Enable(true);
   wxToolTip::SetDelay(1000);
#endif

   // Set default order and mode
   ArrangeButtons();
}

void ControlToolBar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   std::vector<wxString> commands;
   for (long iWinID = ID_PLAY_BUTTON; iWinID < BUTTON_COUNT; iWinID++)
   {
      commands.clear();
      auto pCtrl = static_cast<AButton*>(this->FindWindow(iWinID));
      commands.push_back(pCtrl->GetLabel());
      switch (iWinID)
      {
         case ID_PLAY_BUTTON:
            // Without shift
            commands.push_back(wxT("PlayStop"));
            // With shift
            commands.push_back(_("Loop Play"));
            // For the shortcut tooltip.
            commands.push_back(wxT("PlayLooped"));
            break;
         case ID_RECORD_BUTTON:
            // Without shift
            //commands.push_back(wxT("Record"));
            commands.push_back(wxT("Record1stChoice"));
            {  bool bPreferNewTrack;
               gPrefs->Read("/GUI/PreferNewTrackRecord",&bPreferNewTrack, false);
               if( !bPreferNewTrack ){
                  commands.push_back(_("Record New Track"));
               } else {
                  commands.push_back(_("Append Record"));
               }
               // For the shortcut tooltip.
               commands.push_back(wxT("Record2ndChoice"));
            }
            break;
         case ID_PAUSE_BUTTON:
            commands.push_back(wxT("Pause"));
            break;
         case ID_STOP_BUTTON:
            commands.push_back(wxT("Stop"));
            break;
         case ID_FF_BUTTON:
            commands.push_back(wxT("CursProjectEnd"));
            // With shift
            commands.push_back(_("Select to End"));
            // For the shortcut tooltip.
            commands.push_back(wxT("SelEnd"));
            break;
         case ID_REW_BUTTON:
            commands.push_back(wxT("CursProjectStart"));
            // With shift
            commands.push_back(_("Select to Start"));
            // For the shortcut tooltip.
            commands.push_back(wxT("SelStart"));
            break;
      }
      ToolBar::SetButtonToolTip(*pCtrl, commands);
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
      ReCreateButtons(); // side effect: calls RegenerateTooltips()
      Updated();
   }
   else
      // The other reason to regenerate tooltips is if keyboard shortcuts for
      // transport buttons changed, but that's too much work to check for, so just
      // always do it. (Much cheaper than calling ReCreateButtons() in all cases.
      RegenerateTooltips();


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
      std::unique_ptr < wxSizer > {mSizer}; // DELETE it
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

      std::unique_ptr < wxSizer > {mSizer}; // DELETE it
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

   RegenerateTooltips();
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

   bool paused = mPause->IsDown();
   bool playing = mPlay->IsDown();
   bool recording = mRecord->IsDown();
   bool busy = gAudioIO->IsBusy();

   // Only interested in audio type tracks
   if (p) {
      TrackListIterator iter( p->GetTracks() );
      for (Track *t = iter.First(); t; t = iter.Next()) {
         if (dynamic_cast<const AudioTrack*>(t)) {
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
   mRecord->SetEnabled(
      CanStopAudioStream() &&
      !(busy && !recording && !paused) &&
      !(playing && !paused)
   );
   mStop->SetEnabled(CanStopAudioStream() && (playing || recording));
   mRewind->SetEnabled(IsPauseDown() || (!playing && !recording));
   mFF->SetEnabled(tracks && (IsPauseDown() || (!playing && !recording)));

   //auto pProject = GetActiveProject();
   mPause->SetEnabled(CanStopAudioStream());
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

bool ControlToolBar::IsPauseDown() const
{
   return mPause->IsDown();
}

bool ControlToolBar::IsRecordDown() const
{
   return mRecord->IsDown();
}

int ControlToolBar::PlayPlayRegion(const SelectedRegion &selectedRegion,
                                   const AudioIOStartStreamOptions &options,
                                   PlayMode mode,
                                   PlayAppearance appearance, /* = PlayOption::Straight */
                                   bool backwards, /* = false */
                                   bool playWhiteSpace /* = false */)
// STRONG-GUARANTEE (for state of mCutPreviewTracks)
{
   if (!CanStopAudioStream())
      return -1;

   bool useMidi = true;

   // Remove these lines to experiment with scrubbing/seeking of note tracks
   if (options.pScrubbingOptions)
      useMidi = false;

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

   bool success = false;
   auto cleanup = finally( [&] {
      if (!success) {
         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }
   } );

   if (gAudioIO->IsBusy())
      return -1;

   const bool cutpreview = appearance == PlayAppearance::CutPreview;
   if (cutpreview && t0==t1)
      return -1; /* msmeyer: makes no sense */

   AudacityProject *p = GetActiveProject();
   if (!p)
      return -1;  // Should never happen, but...

   TrackList *t = p->GetTracks();
   if (!t)
      return -1;  // Should never happen, but...

   p->mLastPlayMode = mode;

   bool hasaudio = false;
   TrackListIterator iter(t);
   for (Track *trk = iter.First(); trk; trk = iter.Next()) {
      if (trk->GetKind() == Track::Wave
#ifdef EXPERIMENTAL_MIDI_OUT
         || (trk->GetKind() == Track::Note && useMidi)
#endif
         ) {
         hasaudio = true;
         break;
      }
   }

   double latestEnd = (playWhiteSpace)? t1 : t->GetEndTime();

   if (!hasaudio)
      return -1;  // No need to continue without audio tracks

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
               mCutPreviewTracks->GetWaveTrackConstArray(false),
               WaveTrackArray(),
#ifdef EXPERIMENTAL_MIDI_OUT
               useMidi
                  ? mCutPreviewTracks->GetNoteTrackArray(false)
                  : NoteTrackArray(),
#endif
               tcp0, tcp1, myOptions);
         }
         else
            // Cannot create cut preview tracks, clean up and exit
            return -1;
      }
      else {
         // Lifted the following into AudacityProject::GetDefaultPlayOptions()
         /*
         if (!timetrack) {
            timetrack = t->GetTimeTrack();
         }
         */
         token = gAudioIO->StartStream(t->GetWaveTrackConstArray(false),
                                       WaveTrackArray(),
#ifdef EXPERIMENTAL_MIDI_OUT
                                       useMidi
                                          ? t->GetNoteTrackArray(false)
                                          : NoteTrackArray(),
#endif
                                       t0, t1, options);
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
         // Bug1627 (part of it):
         // infinite error spew when trying to start scrub:
         // Problem was that the error dialog yields to events,
         // causing recursion to this function in the scrub timer
         // handler!  Easy fix, just delay the user alert instead.
         CallAfter( [=]{
         // Show error message if stream could not be opened
         ShowErrorDialog(this, _("Error"),
                         _("Error opening sound device.\nTry changing the audio host, playback device and the project sample rate."),
                         wxT("faq_errors.html#sound_device"), false);
         });
      }
   }

   if (!success)
      return -1;

   StartScrollingIfPreferred();

   // Let other UI update appearance
   if (p)
      p->GetRulerPanel()->HideQuickPlayIndicator();

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
   auto p = GetActiveProject();

   if (!CanStopAudioStream())
      return;

   StopPlaying();

   if (p) p->TP_DisplaySelection();

   auto cleanup = finally( [&]{ UpdateStatusBar(p); } );
   PlayDefault();
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
   StopScrolling();

   AudacityProject *project = GetActiveProject();

   if(project) {
      // Let scrubbing code do some appearance change
      project->GetScrubber().StopScrubbing();
   }

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

   const auto toolbar = project->GetToolManager()->GetToolBar(ScrubbingBarID);
   toolbar->EnableDisableButtons();
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
// STRONG-GUARANTEE (for state of current project's tracks)
{
   // TODO: It would be neater if Menu items and Toolbar buttons used the same code for
   // enabling/disabling, and all fell into the same action routines.
   // Here instead we reduplicate some logic (from CommandHandler) because it isn't
   // normally used for buttons.

   // Code from CommandHandler start...
   AudacityProject * proj = GetActiveProject();
   wxASSERT( proj );
   if( !proj )
      return;

   CommandFlag flags = AlwaysEnabledFlag; // 0 means recalc flags.

   // NB: The call may have the side effect of changing flags.
   bool allowed = proj->TryToMakeActionAllowed(
      flags, 
      AudioIONotBusyFlag | CanStopAudioStreamFlag, 
      AudioIONotBusyFlag | CanStopAudioStreamFlag);

   if( !allowed )
      return;
   // ...end of code from CommandHandler.

   if (gAudioIO->IsBusy()) {
      if (!CanStopAudioStream() || 0 == gAudioIO->GetNumCaptureChannels())
         mRecord->PopUp();
      else
         mRecord->PushDown();
      return;
   }
   AudacityProject *p = GetActiveProject();

   if( evt.GetInt() == 1 ) // used when called by keyboard shortcut. Default (0) ignored.
      mRecord->SetShift(true);
   if( evt.GetInt() == 2 )
      mRecord->SetShift(false);

   SetRecord(true, mRecord->WasShiftDown());

   bool success = false;

   bool shifted = mRecord->WasShiftDown();

   bool bPreferNewTrack;
   gPrefs->Read("/GUI/PreferNewTrackRecord",&bPreferNewTrack, false);
   if( !bPreferNewTrack )
      shifted = !shifted;

   TrackList *trackList = p->GetTracks();
   auto pTracksCopy = TrackList::Create();
   auto &tracksCopy = *pTracksCopy;
   bool tracksCopied = false;

   WaveTrackArray recordingTracks;

   auto cleanup = finally( [&] {
      if (!success) {
         if (tracksCopied)
            // Restore the tracks to remove any inserted silence
            *trackList = std::move(tracksCopy);

         if ( ! shifted ) {
            // msmeyer: Delete recently added tracks if opening stream fails
            for ( auto track : recordingTracks )
               trackList->Remove(track);
         }

         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }

      // Success or not:
      UpdateStatusBar(GetActiveProject());
   } );

   if (p) {
      TrackListIterator it(trackList);

      bool hasWave = false;
      for (auto t = it.First(); t; t = it.Next()) {
         if (t->GetKind() == Track::Wave) {
            hasWave = true;
            break;
         }
      }
      if(!hasWave)
         // Treat append-record like record, when there was no given wave track
         // to append onto.
         shifted = false;

      double t0 = p->GetSel0();
      double t1 = p->GetSel1();
      if (t1 == t0)
         t1 = DBL_MAX;     // record for a long, long time

      /* TODO: set up stereo tracks if that is how the user has set up
       * their preferences, and choose sample format based on prefs */
      WaveTrackConstArray playbackTracks;
#ifdef EXPERIMENTAL_MIDI_OUT
      NoteTrackArray midiTracks;
#endif
      bool duplex;
#ifdef EXPERIMENTAL_DA
      gPrefs->Read(wxT("/AudioIO/Duplex"), &duplex, false);
#else
      gPrefs->Read(wxT("/AudioIO/Duplex"), &duplex, true);
#endif
      if(duplex){
         playbackTracks = trackList->GetWaveTrackConstArray(false);
#ifdef EXPERIMENTAL_MIDI_OUT
         midiTracks = trackList->GetNoteTrackArray(false);
#endif
     }
      else {
         playbackTracks = WaveTrackConstArray();
#ifdef EXPERIMENTAL_MIDI_OUT
         midiTracks = NoteTrackArray();
#endif
     }

      // If SHIFT key was down, the user wants append to tracks
      int recordingChannels = 0;
      if (shifted) {
         recordingChannels = gPrefs->Read(wxT("/AudioIO/RecordChannels"), 2);
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
               // less than or equal, not just less than, to ensure a clip boundary.
               // when append recording.
               if (t1 <= t0) {
                  if (!tracksCopied) {
                     // Duplicate all tracks before modifying any of them.
                     // The duplicates are used to restore state in case
                     // of failure.
                     tracksCopied = true;
                     tracksCopy = *trackList;
                  }

                  // Pad the recording track with silence, up to the
                  // maximum time.
                  auto newTrack = p->GetTrackFactory()->NewWaveTrack();
                  newTrack->InsertSilence(0.0, t0 - t1);
                  newTrack->Flush();
                  wt->Clear(t1, t0);
                  wt->Paste(t1, newTrack.get());
               }
               recordingTracks.push_back(wt);
               // Don't record more channels than configured recording pref.
               if( (int)recordingTracks.size() >= recordingChannels ){
                  break;
               }
            }
         }

         t1 = DBL_MAX;     // record for a long, long time
      }
      else {
         bool recordingNameCustom, useTrackNumber, useDateStamp, useTimeStamp;
         wxString defaultTrackName, defaultRecordingTrackName;

         // Count the tracks.  
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
         defaultTrackName = TracksPrefs::GetDefaultAudioTrackNamePreference();
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
            recordingTracks.push_back(
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
                                        recordingTracks,
#ifdef EXPERIMENTAL_MIDI_OUT
                                        midiTracks,
#endif
                                        t0, t1, options);

      success = (token != 0);

      if (success) {
         p->SetAudioIOToken(token);
         mBusyProject = p;

         StartScrollingIfPreferred();
      }
      else {
         // Show error message if stream could not be opened
         ShowErrorDialog(this, _("Error"),
                         _("Error opening sound device.\nTry changing the audio host, recording device and the project sample rate."),
                         wxT("faq_errors.html#sound_device"), false);
      }
   }
}


void ControlToolBar::OnPause(wxCommandEvent & WXUNUSED(evt))
{
   if (!CanStopAudioStream()) {
      return;
   }


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

#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   if (gAudioIO->IsScrubbing())
      GetActiveProject()->GetScrubber().Pause(mPaused);
   else
#endif
   {
      gAudioIO->SetPaused(mPaused);
   }

   UpdateStatusBar(GetActiveProject());
}

void ControlToolBar::OnRewind(wxCommandEvent & WXUNUSED(evt))
{
   mRewind->PushDown();
   mRewind->PopUp();

   AudacityProject *p = GetActiveProject();
   if (p) {
      p->StopIfPaused();
      p->Rewind(mRewind->WasShiftDown());
   }
}

void ControlToolBar::OnFF(wxCommandEvent & WXUNUSED(evt))
{
   mFF->PushDown();
   mFF->PopUp();

   AudacityProject *p = GetActiveProject();

   if (p) {
      p->StopIfPaused();
      p->SkipEnd(mFF->WasShiftDown());
   }
}

void ControlToolBar::SetupCutPreviewTracks(double WXUNUSED(playStart), double cutStart,
                                           double cutEnd, double  WXUNUSED(playEnd))

// STRONG-GUARANTEE (for state of mCutPreviewTracks)
{
   ClearCutPreviewTracks();
   AudacityProject *p = GetActiveProject();
   if (p) {
      // Find first selected track (stereo or mono) and duplicate it
      const Track *track1 = NULL, *track2 = NULL;
      TrackListIterator it(p->GetTracks());
      for (Track *t = it.First(); t; t = it.Next())
      {
         if (t->GetSelected() &&
            (t->GetKind() == Track::Wave
#ifdef EXPERIMENTAL_MIDI_OUT
         || t->GetKind() == Track::Note
#endif
            ))
         {
            track1 = t;
            track2 = t->GetLink();
            break;
         }
      }

      if (track1)
      {
         // Duplicate and change tracks
         // Clear has a very small chance of throwing
         auto new1 = track1->Duplicate();
         new1->Clear(cutStart, cutEnd);
         decltype(new1) new2{};
         if (track2)
         {
            new2 = track2->Duplicate();
            new2->Clear(cutStart, cutEnd);
         }

         // use NOTHROW-GUARANTEE:

         mCutPreviewTracks = TrackList::Create();
         mCutPreviewTracks->Add(std::move(new1));
         if (track2)
            mCutPreviewTracks->Add(std::move(new2));
      }
   }
}

void ControlToolBar::ClearCutPreviewTracks()
{
   if (mCutPreviewTracks)
      mCutPreviewTracks->Clear();
   mCutPreviewTracks.reset();
}

// works out the width of the field in the status bar needed for the state (eg play, record pause)
int ControlToolBar::WidthForStatusBar(wxStatusBar* const sb)
{
   int xMax = 0;
   const auto pauseString = wxT(" ") + wxGetTranslation(mStatePause);

   auto update = [&] (const wxString &state) {
      int x, y;
      sb->GetTextExtent(
         wxGetTranslation(state) + pauseString + wxT("."),
         &x, &y
      );
      xMax = std::max(x, xMax);
   };

   update(mStatePlay);
   update(mStateStop);
   update(mStateRecord);

   // Note that Scrubbing + Paused is not allowed.
   for(const auto &state : Scrubber::GetAllUntranslatedStatusStrings())
      update(state);

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

void ControlToolBar::StartScrollingIfPreferred()
{
   if (TracksPrefs::GetPinnedHeadPreference())
      StartScrolling();
#ifdef __WXMAC__
   else if (::GetActiveProject()->GetScrubber().HasStartedScrubbing()) {
      // PRL:  cause many "unnecessary" refreshes.  For reasons I don't understand,
      // doing this causes wheel rotation events (mapped from the double finger vertical
      // swipe) to be delivered more uniformly to the application, so that speed control
      // works better.
      ::GetActiveProject()->GetPlaybackScroller().Activate
         (AudacityProject::PlaybackScroller::Mode::Refresh);
   }
#endif
   else
      StopScrolling();
}

void ControlToolBar::StartScrolling()
{
   using Mode = AudacityProject::PlaybackScroller::Mode;
   const auto project = GetActiveProject();
   if (project) {
      auto mode = Mode::Centered;

#if 0
      // Enable these lines to pin the playhead right instead of center,
      // when recording but not overdubbing.
      if (gAudioIO->GetNumCaptureChannels() > 0) {
         // recording

         // Display a fixed recording head while scrolling the waves continuously.
         // If you overdub, you may want to anticipate some context in existing tracks,
         // so center the head.  If not, put it rightmost to display as much wave as we can.
         bool duplex;
#ifdef EXPERIMENTAL_DA
         gPrefs->Read(wxT("/AudioIO/Duplex"), &duplex, false);
#else
         gPrefs->Read(wxT("/AudioIO/Duplex"), &duplex, true);
#endif
         if (duplex) {
            // See if there is really anything being overdubbed
            if (gAudioIO->GetNumPlaybackChannels() == 0)
               // No.
               duplex = false;
         }

         if (!duplex)
            mode = Mode::Right;
      }
#endif

      project->GetPlaybackScroller().Activate(mode);
   }
}

void ControlToolBar::StopScrolling()
{
   const auto project = GetActiveProject();
   if(project)
      project->GetPlaybackScroller().Activate
         (AudacityProject::PlaybackScroller::Mode::Off);
}
