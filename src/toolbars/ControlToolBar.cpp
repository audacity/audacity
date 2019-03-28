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

#include "../Audacity.h" // for USE_* macros
#include "ControlToolBar.h"

#include "../Experimental.h"

#include <algorithm>
#include <cfloat>

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/dc.h>
#include <wx/event.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statusbr.h>
#include <wx/timer.h>
#endif
#include <wx/tooltip.h>
#include <wx/datetime.h>

#include "TranscriptionToolBar.h"
#include "MeterToolBar.h"

#include "../AColor.h"
#include "../AdornedRulerPanel.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Menus.h"
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
#include "../TrackPanel.h"

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
AButton *ControlToolBar::MakeButton(ControlToolBar *pBar,
                                    teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
                                    int id,
                                    bool processdownevents,
                                    const wxChar *label)
{
   AButton *r = ToolBar::MakeButton(pBar,
      bmpRecoloredUpLarge, bmpRecoloredDownLarge, bmpRecoloredUpHiliteLarge, bmpRecoloredHiliteLarge,
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
      bmpRecoloredUpLarge, bmpRecoloredDownLarge, bmpRecoloredUpHiliteLarge, bmpRecoloredHiliteLarge,
      eEnabledUp, eEnabledDown, eDisabled,
      theTheme.ImageSize( bmpRecoloredUpLarge ));
}

void ControlToolBar::Populate()
{
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
   MakeButtonBackgroundsLarge();

   mPause = MakeButton(this, bmpPause, bmpPause, bmpPauseDisabled,
      ID_PAUSE_BUTTON,  true,  _("Pause"));

   mPlay = MakeButton(this, bmpPlay, bmpPlay, bmpPlayDisabled,
      ID_PLAY_BUTTON, true, _("Play"));
   MakeAlternateImages(*mPlay, 1, bmpLoop, bmpLoop, bmpLoopDisabled);
   MakeAlternateImages(*mPlay, 2,
      bmpCutPreview, bmpCutPreview, bmpCutPreviewDisabled);
   MakeAlternateImages(*mPlay, 3,
                       bmpScrub, bmpScrub, bmpScrubDisabled);
   MakeAlternateImages(*mPlay, 4,
                       bmpSeek, bmpSeek, bmpSeekDisabled);
   mPlay->FollowModifierKeys();

   mStop = MakeButton(this, bmpStop, bmpStop, bmpStopDisabled ,
      ID_STOP_BUTTON, false, _("Stop"));

   mRewind = MakeButton(this, bmpRewind, bmpRewind, bmpRewindDisabled,
      ID_REW_BUTTON, false, _("Skip to Start"));

   mFF = MakeButton(this, bmpFFwd, bmpFFwd, bmpFFwdDisabled,
      ID_FF_BUTTON, false, _("Skip to End"));

   mRecord = MakeButton(this, bmpRecord, bmpRecord, bmpRecordDisabled,
      ID_RECORD_BUTTON, false, _("Record"));

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
   for (long iWinID = ID_PLAY_BUTTON; iWinID < BUTTON_COUNT; iWinID++)
   {
      auto pCtrl = static_cast<AButton*>(this->FindWindow(iWinID));
      CommandID name;
      switch (iWinID)
      {
         case ID_PLAY_BUTTON:
            // Without shift
            name = wxT("PlayStop");
            break;
         case ID_RECORD_BUTTON:
            // Without shift
            //name = wxT("Record");
            name = wxT("Record1stChoice");
            break;
         case ID_PAUSE_BUTTON:
            name = wxT("Pause");
            break;
         case ID_STOP_BUTTON:
            name = wxT("Stop");
            break;
         case ID_FF_BUTTON:
            name = wxT("CursProjectEnd");
            break;
         case ID_REW_BUTTON:
            name = wxT("CursProjectStart");
            break;
      }
      std::vector<TranslatedInternalString> commands(
         1u, { name, pCtrl->GetLabel() } );

      // Some have a second
      switch (iWinID)
      {
         case ID_PLAY_BUTTON:
            // With shift
            commands.push_back( { wxT("PlayLooped"), _("Loop Play") } );
            break;
         case ID_RECORD_BUTTON:
            // With shift
            {  bool bPreferNewTrack;
               gPrefs->Read("/GUI/PreferNewTrackRecord",&bPreferNewTrack, false);
               // For the shortcut tooltip.
               commands.push_back( {
                  wxT("Record2ndChoice"),
                  !bPreferNewTrack
                     ? _("Record New Track")
                     : _("Append Record")
               } );
            }
            break;
         case ID_PAUSE_BUTTON:
            break;
         case ID_STOP_BUTTON:
            break;
         case ID_FF_BUTTON:
            // With shift
            commands.push_back( {
               wxT("SelEnd"), _("Select to End") } );
            break;
         case ID_REW_BUTTON:
            // With shift
            commands.push_back( {
               wxT("SelStart"), _("Select to Start") } );
            break;
      }
      ToolBar::SetButtonToolTip(*pCtrl, commands.data(), commands.size());
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

   bool paused = mPause->IsDown();
   bool playing = mPlay->IsDown();
   bool recording = mRecord->IsDown();
   bool busy = gAudioIO->IsBusy();

   // Only interested in audio type tracks
   bool tracks = p && p->GetTracks()->Any<AudioTrack>(); // PRL:  PlayableTrack ?

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

void ControlToolBar::SetRecord(bool down, bool altAppearance)
{
   if (down)
   {
      mRecord->SetAlternateIdx(altAppearance ? 1 : 0);
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

   bool hasaudio;
   if (useMidi)
      hasaudio = ! p->GetTracks()->Any<PlayableTrack>().empty();
   else
      hasaudio = ! p->GetTracks()->Any<WaveTrack>().empty();

   double latestEnd = (playWhiteSpace)? t1 : t->GetEndTime();

   if (!hasaudio)
      return -1;  // No need to continue without audio tracks

#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
   double init_seek = 0.0;
#endif

   if (t1 == t0) {
      if (looped) {
         const auto &selectedRegion = p->GetViewInfo().selectedRegion;
         // play selection if there is one, otherwise
         // set start of play region to project start, 
         // and loop the project from current play position.

         if ((t0 > selectedRegion.t0()) && (t0 < selectedRegion.t1())) {
            t0 = selectedRegion.t0();
            t1 = selectedRegion.t1();
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
               GetAllPlaybackTracks(*mCutPreviewTracks, false, useMidi),
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
         token = gAudioIO->StartStream(
            GetAllPlaybackTracks(*t, false, useMidi),
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
                         wxT("Error_opening_sound_device"));
         });
      }
   }

   if (!success)
      return -1;

   StartScrollingIfPreferred();

   // Let other UI update appearance
   if (p)
      p->GetRulerPanel()->DrawBothOverlays();

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

      MeterPanel *meter = project->GetPlaybackMeter();
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

WaveTrackArray ControlToolBar::ChooseExistingRecordingTracks(
   AudacityProject &proj, bool selectedOnly)
{
   auto p = &proj;
   size_t recordingChannels =
      std::max(0L, gPrefs->Read(wxT("/AudioIO/RecordChannels"), 2));
   bool strictRules = (recordingChannels <= 2);

   // Iterate over all wave tracks, or over selected wave tracks only.
   //
   // In the usual cases of one or two recording channels, seek a first-fit
   // unbroken sub-sequence for which the total number of channels matches the
   // required number exactly.  Never drop inputs or fill only some channels
   // of a track.
   //
   // In case of more than two recording channels, choose tracks only among the
   // selected.  Simply take the earliest wave tracks, until the number of
   // channels is enough.  If there are fewer channels than inputs, but at least
   // one channel, then some of the input channels will be dropped.
   //
   // Resulting tracks may be non-consecutive within the list of all tracks
   // (there may be non-wave tracks between, or non-selected tracks when
   // considering selected tracks only.)

   if (!strictRules && !selectedOnly)
      return {};

   auto trackList = p->GetTracks();
   std::vector<unsigned> channelCounts;
   WaveTrackArray candidates;
   const auto range = trackList->Leaders<WaveTrack>();
   for ( auto candidate : selectedOnly ? range + &Track::IsSelected : range ) {
      // count channels in this track
      const auto channels = TrackList::Channels( candidate );
      unsigned nChannels = channels.size();

      if (strictRules && nChannels > recordingChannels) {
         // The recording would under-fill this track's channels
         // Can't use any partial accumulated results
         // either.  Keep looking.
         candidates.clear();
         channelCounts.clear();
         continue;
      }
      else {
         // Might use this but may have to discard some of the accumulated
         while(strictRules &&
               nChannels + candidates.size() > recordingChannels) {
            auto nOldChannels = channelCounts[0];
            wxASSERT(nOldChannels > 0);
            channelCounts.erase(channelCounts.begin());
            candidates.erase(candidates.begin(),
                             candidates.begin() + nOldChannels);
         }
         channelCounts.push_back(nChannels);
         for ( auto channel : channels ) {
            candidates.push_back(channel->SharedPointer<WaveTrack>());
            if(candidates.size() == recordingChannels)
               // Done!
               return candidates;
         }
      }
   }

   if (!strictRules && !candidates.empty())
      // good enough
      return candidates;

   // If the loop didn't exit early, we could not find enough channels
   return {};
}

void ControlToolBar::OnRecord(wxCommandEvent &evt)
// STRONG-GUARANTEE (for state of current project's tracks)
{
   // TODO: It would be neater if Menu items and Toolbar buttons used the same code for
   // enabling/disabling, and all fell into the same action routines.
   // Here instead we reduplicate some logic (from CommandHandler) because it isn't
   // normally used for buttons.

   // Code from CommandHandler start...
   AudacityProject * p = GetActiveProject();
   wxASSERT(p);
   if (!p)
      return;

   bool altAppearance = mRecord->WasShiftDown();
   if (evt.GetInt() == 1) // used when called by keyboard shortcut. Default (0) ignored.
      altAppearance = true;
   if (evt.GetInt() == 2)
      altAppearance = false;

   bool bPreferNewTrack;
   gPrefs->Read("/GUI/PreferNewTrackRecord", &bPreferNewTrack, false);
   const bool appendRecord = (altAppearance == bPreferNewTrack);

   if (p) {
      const auto &selectedRegion = p->GetViewInfo().selectedRegion;
      double t0 = selectedRegion.t0();
      double t1 = selectedRegion.t1();
      // When no time selection, recording duration is 'unlimited'.
      if (t1 == t0)
         t1 = DBL_MAX;

      WaveTrackArray existingTracks;

      if (appendRecord) {
         const auto trackRange = p->GetTracks()->Any< const WaveTrack >();

         // Try to find wave tracks to record into.  (If any are selected,
         // try to choose only from them; else if wave tracks exist, may record into any.)
         existingTracks = ChooseExistingRecordingTracks(*p, true);
         if ( !existingTracks.empty() )
            t0 = std::max( t0,
               ( trackRange + &Track::IsSelected ).max( &Track::GetEndTime ) );
         else {
            existingTracks = ChooseExistingRecordingTracks(*p, false);
            t0 = std::max( t0, trackRange.max( &Track::GetEndTime ) );
            // If suitable tracks still not found, will record into NEW ones,
            // but the choice of t0 does not depend on that.
         }

         // Whether we decided on NEW tracks or not:
         if (t1 <= selectedRegion.t0() && selectedRegion.t1() > selectedRegion.t0()) {
            t1 = selectedRegion.t1();   // record within the selection
         }
         else {
            t1 = DBL_MAX;        // record for a long, long time
         }
      }

      TransportTracks transportTracks;
      if (UseDuplex()) {
         // Remove recording tracks from the list of tracks for duplex ("overdub")
         // playback.
         /* TODO: set up stereo tracks if that is how the user has set up
          * their preferences, and choose sample format based on prefs */
         transportTracks = GetAllPlaybackTracks(*p->GetTracks(), false, true);
         for (const auto &wt : existingTracks) {
            auto end = transportTracks.playbackTracks.end();
            auto it = std::find(transportTracks.playbackTracks.begin(), end, wt);
            if (it != end)
               transportTracks.playbackTracks.erase(it);
         }
      }

      transportTracks.captureTracks = existingTracks;
      AudioIOStartStreamOptions options(p->GetDefaultPlayOptions());
      DoRecord(*p, transportTracks, t0, t1, altAppearance, options);
   }
}

bool ControlToolBar::UseDuplex()
{
   bool duplex;
   gPrefs->Read(wxT("/AudioIO/Duplex"), &duplex,
#ifdef EXPERIMENTAL_DA
      false
#else
      true
#endif
      );
   return duplex;
}

bool ControlToolBar::DoRecord(AudacityProject &project,
   const TransportTracks &tracks,
   double t0, double t1,
   bool altAppearance,
   const AudioIOStartStreamOptions &options)
{
   CommandFlag flags = AlwaysEnabledFlag; // 0 means recalc flags.

   // NB: The call may have the side effect of changing flags.
   bool allowed = GetMenuManager(project).TryToMakeActionAllowed(
      project,
      flags,
      AudioIONotBusyFlag | CanStopAudioStreamFlag,
      AudioIONotBusyFlag | CanStopAudioStreamFlag);

   if (!allowed)
      return false;
   // ...end of code from CommandHandler.

   if (gAudioIO->IsBusy()) {
      if (!CanStopAudioStream() || 0 == gAudioIO->GetNumCaptureChannels())
         mRecord->PopUp();
      else
         mRecord->PushDown();
      return false;
   }

   SetRecord(true, altAppearance);

   bool success = false;
   auto cleanup = finally([&] {
      if (!success) {
         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }

      // Success or not:
      UpdateStatusBar(GetActiveProject());
   });

   auto transportTracks = tracks;

   // Will replace any given capture tracks with temporaries
   transportTracks.captureTracks.clear();

   const auto p = &project;

   bool appendRecord = !tracks.captureTracks.empty();

   {
      if (appendRecord) {
         // Append recording:
         // Pad selected/all wave tracks to make them all the same length
         for (const auto &wt : tracks.captureTracks)
         {
            auto endTime = wt->GetEndTime();

            // If the track was chosen for recording and playback both,
            // remember the original in preroll tracks, before making the
            // pending replacement.
            bool prerollTrack = make_iterator_range(transportTracks.playbackTracks).contains(wt);
            if (prerollTrack)
                  transportTracks.prerollTracks.push_back(wt);

            // A function that copies all the non-sample data between
            // wave tracks; in case the track recorded to changes scale
            // type (for instance), during the recording.
            auto updater = [](Track &d, const Track &s){
               auto &dst = static_cast<WaveTrack&>(d);
               auto &src = static_cast<const WaveTrack&>(s);
               dst.Reinit(src);
            };

            // Get a copy of the track to be appended, to be pushed into
            // undo history only later.
            auto pending = std::static_pointer_cast<WaveTrack>(
               p->GetTracks()->RegisterPendingChangedTrack(
                  updater, wt.get() ) );

            // End of current track is before or at recording start time.
            // Less than or equal, not just less than, to ensure a clip boundary.
            // when append recording.
            if (endTime <= t0) {

               // Pad the recording track with silence, up to the
               // maximum time.
               auto newTrack = p->GetTrackFactory()->NewWaveTrack();
               newTrack->SetWaveColorIndex( wt->GetWaveColorIndex() );
               newTrack->InsertSilence(0.0, t0 - endTime);
               newTrack->Flush();
               pending->Clear(endTime, t0);
               pending->Paste(endTime, newTrack.get());
            }
            transportTracks.captureTracks.push_back(pending);
         }
         p->GetTracks()->UpdatePendingTracks();
      }

      if( transportTracks.captureTracks.empty() )
      {   // recording to NEW track(s).
         bool recordingNameCustom, useTrackNumber, useDateStamp, useTimeStamp;
         wxString defaultTrackName, defaultRecordingTrackName;

         // Count the tracks.
         const auto trackList = p->GetTracks();
         auto numTracks = trackList->Leaders<const WaveTrack>().size();

         auto recordingChannels = std::max(1L, gPrefs->Read(wxT("/AudioIO/RecordChannels"), 2));

         gPrefs->Read(wxT("/GUI/TrackNames/RecordingNameCustom"), &recordingNameCustom, false);
         gPrefs->Read(wxT("/GUI/TrackNames/TrackNumber"), &useTrackNumber, false);
         gPrefs->Read(wxT("/GUI/TrackNames/DateStamp"), &useDateStamp, false);
         gPrefs->Read(wxT("/GUI/TrackNames/TimeStamp"), &useTimeStamp, false);
         defaultTrackName = TracksPrefs::GetDefaultAudioTrackNamePreference();
         gPrefs->Read(wxT("/GUI/TrackNames/RecodingTrackName"), &defaultRecordingTrackName, defaultTrackName);

         wxString baseTrackName = recordingNameCustom? defaultRecordingTrackName : defaultTrackName;

         Track *first {};
         for (int c = 0; c < recordingChannels; c++) {
            auto newTrack = p->GetTrackFactory()->NewWaveTrack();
            if (!first)
               first = newTrack.get();

            // Quantize bounds to the rate of the new track.
            if (c == 0) {
               if (t0 < DBL_MAX)
                  t0 = newTrack->LongSamplesToTime(newTrack->TimeToLongSamples(t0));
               if (t1 < DBL_MAX)
                  t1 = newTrack->LongSamplesToTime(newTrack->TimeToLongSamples(t1));
            }

            newTrack->SetOffset(t0);
            wxString nameSuffix = wxString(wxT(""));

            if (useTrackNumber) {
               nameSuffix += wxString::Format(wxT("%d"), 1 + numTracks + c);
            }

            if (useDateStamp) {
               if (!nameSuffix.empty()) {
                  nameSuffix += wxT("_");
               }
               nameSuffix += wxDateTime::Now().FormatISODate();
            }

            if (useTimeStamp) {
               if (!nameSuffix.empty()) {
                  nameSuffix += wxT("_");
               }
               nameSuffix += wxDateTime::Now().FormatISOTime();
            }

            // ISO standard would be nice, but ":" is unsafe for file name.
            nameSuffix.Replace(wxT(":"), wxT("-"));

            if (baseTrackName.empty()) {
               newTrack->SetName(nameSuffix);
            }
            else if (nameSuffix.empty()) {
               newTrack->SetName(baseTrackName);
            }
            else {
               newTrack->SetName(baseTrackName + wxT("_") + nameSuffix);
            }

            if ((recordingChannels > 2) && !(p->GetTracksFitVerticallyZoomed())) {
               newTrack->SetMinimized(true);
            }

            p->GetTracks()->RegisterPendingNewTrack( newTrack );
            transportTracks.captureTracks.push_back(newTrack);
            // Bug 1548.  New track needs the focus.
            p->GetTrackPanel()->SetFocusedTrack( newTrack.get() );
         }
         p->GetTracks()->GroupChannels(*first, recordingChannels);
      }
      
      //Automated Input Level Adjustment Initialization
      #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
         gAudioIO->AILAInitialize();
      #endif

      int token = gAudioIO->StartStream(transportTracks, t0, t1, options);

      success = (token != 0);

      if (success) {
         p->SetAudioIOToken(token);
         mBusyProject = p;

         StartScrollingIfPreferred();
      }
      else {
         CancelRecording();

         // Show error message if stream could not be opened
         wxString msg = wxString::Format(_("Error opening recording device.\nError code: %s"), gAudioIO->LastPaErrorString());
         ShowErrorDialog(this, _("Error"), msg, wxT("Error_opening_sound_device"));
      }
   }

   return success;
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

   // Bug 1494 - Pausing a seek or scrub should just STOP as
   // it is confusing to be in a paused scrub state.
   bool bStopInstead = mPaused && 
      gAudioIO->IsScrubbing() && 
      !GetActiveProject()->GetScrubber().IsSpeedPlaying();

   if (bStopInstead) {
      wxCommandEvent dummy;
      OnStop(dummy);
      return;
   }
   
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
      auto trackRange = p->GetTracks()->Selected<const PlayableTrack>();
      if( !trackRange.empty() ) {
         auto cutPreviewTracks = TrackList::Create();
         for (const auto track1 : trackRange) {
            // Duplicate and change tracks
            // Clear has a very small chance of throwing

            auto newTrack = track1->Duplicate();
            newTrack->Clear(cutStart, cutEnd);
            cutPreviewTracks->Add( newTrack );
         }
         // use NOTHROW-GUARANTEE:
         mCutPreviewTracks = cutPreviewTracks;
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
   if (!scrubState.empty())
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

bool ControlToolBar::IsTransportingPinned()
{
   if (!TracksPrefs::GetPinnedHeadPreference())
      return false;
   const auto &scrubber = ::GetActiveProject()->GetScrubber();
   return
     !(scrubber.HasMark() &&
       !scrubber.WasSpeedPlaying() &&
       !Scrubber::ShouldScrubPinned());
}

void ControlToolBar::StartScrollingIfPreferred()
{
   if (IsTransportingPinned())
      StartScrolling();
#ifdef __WXMAC__
   else if (::GetActiveProject()->GetScrubber().HasMark()) {
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
      auto mode = Mode::Pinned;

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

void ControlToolBar::CommitRecording()
{
   const auto project = GetActiveProject();
   project->GetTracks()->ApplyPendingTracks();
}

void ControlToolBar::CancelRecording()
{
   const auto project = GetActiveProject();
   project->GetTracks()->ClearPendingTracks();
}
