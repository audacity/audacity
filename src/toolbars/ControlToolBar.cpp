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
  normal project window, or within a ToolBarFrame.

  All of the controls in this window were custom-written for
  Audacity - they are not native controls on any platform -
  however, it is intended that the images could be easily
  replaced to allow "skinning" or just customization to
  match the look and feel of each platform.

*//*******************************************************************/


#include "ControlToolBar.h"

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

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "Prefs.h"
#include "../Project.h"
#include "../ProjectAudioIO.h"
#include "../ProjectAudioManager.h"
#include "../ProjectSettings.h"
#include "../ProjectStatus.h"
#include "../ProjectWindow.h"
#include "../Track.h"
#include "../widgets/AButton.h"
#include "FileNames.h"

#include "../tracks/ui/Scrubbing.h"
#include "../toolbars/ToolManager.h"

IMPLEMENT_CLASS(ControlToolBar, ToolBar);

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
   EVT_IDLE(ControlToolBar::OnIdle)
END_EVENT_TABLE()

static const TranslatableString
   /* i18n-hint: These are strings for the status bar, and indicate whether Audacity
   is playing or recording or stopped, and whether it is paused;
   progressive verb form */
     sStatePlay = XO("Playing")
   /* i18n-hint: These are strings for the status bar, and indicate whether Audacity
   is playing or recording or stopped, and whether it is paused;
   progressive verb form */
   , sStateStop = XO("Stopped")
   /* i18n-hint: These are strings for the status bar, and indicate whether Audacity
   is playing or recording or stopped, and whether it is paused;
   progressive verb form */
   , sStateRecord = XO("Recording")
;

//Standard constructor
// This was called "Control" toolbar in the GUI before - now it is "Transport".
// Note that we use the legacy "Control" string as the section because this
// gets written to prefs and cannot be changed in prefs to maintain backwards
// compatibility
ControlToolBar::ControlToolBar( AudacityProject &project )
: ToolBar(project, TransportBarID, XO("Transport"), wxT("Control"))
{
   gPrefs->Read(wxT("/GUI/ErgonomicTransportButtons"), &mErgonomicTransportButtons, true);
   mStrLocale = gPrefs->Read(wxT("/Locale/Language"), wxT(""));

   mSizer = NULL;
}

ControlToolBar::~ControlToolBar()
{
}


ControlToolBar *ControlToolBar::Find( AudacityProject &project )
{
   auto &toolManager = ToolManager::Get( project );
   return static_cast<ControlToolBar*>(
      toolManager.GetToolBar(TransportBarID) );
}

ControlToolBar &ControlToolBar::Get( AudacityProject &project )
{
   auto &toolManager = ToolManager::Get( project );
   return *static_cast<ControlToolBar*>(
      toolManager.GetToolBar(TransportBarID) );
}

const ControlToolBar &ControlToolBar::Get( const AudacityProject &project )
{
   return Get( const_cast<AudacityProject&>( project )) ;
}

void ControlToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
}

// This is a convenience function that allows for button creation in
// MakeButtons() with fewer arguments
AButton *ControlToolBar::MakeButton(ControlToolBar *pBar,
                                    teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
                                    int id,
                                    bool processdownevents,
                                    const TranslatableString &label)
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
      ID_PAUSE_BUTTON,  true,  XO("Pause"));

   mPlay = MakeButton(this, bmpPlay, bmpPlay, bmpPlayDisabled,
      ID_PLAY_BUTTON, true, XO("Play"));
   MakeAlternateImages(*mPlay, 1, bmpLoop, bmpLoop, bmpLoopDisabled);
   MakeAlternateImages(*mPlay, 2,
      bmpCutPreview, bmpCutPreview, bmpCutPreviewDisabled);
   MakeAlternateImages(*mPlay, 3,
                       bmpScrub, bmpScrub, bmpScrubDisabled);
   MakeAlternateImages(*mPlay, 4,
                       bmpSeek, bmpSeek, bmpSeekDisabled);
   mPlay->FollowModifierKeys();

   mStop = MakeButton(this, bmpStop, bmpStop, bmpStopDisabled ,
      ID_STOP_BUTTON, false, XO("Stop"));

   mRewind = MakeButton(this, bmpRewind, bmpRewind, bmpRewindDisabled,
      ID_REW_BUTTON, false, XO("Skip to Start"));

   mFF = MakeButton(this, bmpFFwd, bmpFFwd, bmpFFwdDisabled,
      ID_FF_BUTTON, false, XO("Skip to End"));

   mRecord = MakeButton(this, bmpRecord, bmpRecord, bmpRecordDisabled,
      ID_RECORD_BUTTON, false, XO("Record"));

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
   for (long iWinID = ID_PAUSE_BUTTON; iWinID < BUTTON_COUNT; iWinID++)
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
      std::vector<ComponentInterfaceSymbol> commands(
         1u, { name, Verbatim( pCtrl->GetLabel() ) } );

      // Some have a second
      switch (iWinID)
      {
         case ID_PLAY_BUTTON:
            // With shift
            commands.push_back( { wxT("PlayLooped"), XO("Loop Play") } );
            break;
         case ID_RECORD_BUTTON:
            // With shift
            {  bool bPreferNewTrack;
               gPrefs->Read("/GUI/PreferNewTrackRecord",&bPreferNewTrack, false);
               // For the shortcut tooltip.
               commands.push_back( {
                  wxT("Record2ndChoice"),
                  !bPreferNewTrack
                     ? XO("Record New Track")
                     : XO("Append Record")
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
               wxT("SelEnd"), XO("Select to End") } );
            break;
         case ID_REW_BUTTON:
            // With shift
            commands.push_back( {
               wxT("SelStart"), XO("Select to Start") } );
            break;
      }
      ToolBar::SetButtonToolTip(
         mProject, *pCtrl, commands.data(), commands.size());
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
   SetLabel(XO("Transport"));

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
      mRecord->SetAlternateIdx(recordShift ? 1 : 0);
      mRecord->PushDown();
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
   AudacityProject *p = &mProject;
   auto &projectAudioManager = ProjectAudioManager::Get( mProject );
   bool canStop = projectAudioManager.CanStopAudioStream();

   bool paused = mPause->IsDown();
   bool playing = mPlay->IsDown();
   bool recording = mRecord->IsDown();
   auto gAudioIO = AudioIO::Get();
   bool busy = gAudioIO->IsBusy();

   // Only interested in audio type tracks
   bool tracks = p && TrackList::Get( *p ).Any<AudioTrack>(); // PRL:  PlayableTrack ?

   mPlay->SetEnabled( canStop && tracks && !recording );
   mRecord->SetEnabled(
      canStop &&
      !(busy && !recording && !paused) &&
      !(playing && !paused)
   );
   mStop->SetEnabled(canStop && (playing || recording));
   mRewind->SetEnabled(paused || (!playing && !recording));
   mFF->SetEnabled(tracks && (paused || (!playing && !recording)));

   mPause->SetEnabled(canStop);
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
}

void ControlToolBar::SetStop()
{
   mStop->PushDown();
   EnableDisableButtons();
}

void ControlToolBar::OnKeyEvent(wxKeyEvent & event)
{
   // PRL: is this handler really ever reached?  Is the ControlToolBar ever
   // focused?  Isn't there a global event filter that interprets the spacebar
   // key (or other key chosen in preferences) and dispatches to DoPlayStop,
   // according to CommandManager's table, before we come to this redundant
   // function?

   if (event.ControlDown() || event.AltDown()) {
      event.Skip();
      return;
   }

   auto gAudioIO = AudioIOBase::Get();
   auto &projectAudioManager = ProjectAudioManager::Get( mProject );

   // Does not appear to be needed on Linux. Perhaps on some other platform?
   // If so, "!CanStopAudioStream()" should probably apply.
   if (event.GetKeyCode() == WXK_SPACE) {
      if ( projectAudioManager.Playing() || projectAudioManager.Recording() ) {
         SetStop();
         projectAudioManager.Stop();
      }
      else if (!gAudioIO->IsBusy()) {
         projectAudioManager.PlayCurrentRegion();
      }
      return;
   }
   event.Skip();
}

void ControlToolBar::OnPlay(wxCommandEvent & WXUNUSED(evt))
{
   auto p = &mProject;
   auto &projectAudioManager = ProjectAudioManager::Get( mProject );
   bool canStop = projectAudioManager.CanStopAudioStream();

   if ( !canStop )
      return;

   projectAudioManager.Stop();

   PlayDefault();
}

void ControlToolBar::OnStop(wxCommandEvent & WXUNUSED(evt))
{
   auto &projectAudioManager = ProjectAudioManager::Get( mProject );
   bool canStop = projectAudioManager.CanStopAudioStream();

   if ( canStop ) {
      projectAudioManager.Stop();
   }
}

void ControlToolBar::PlayDefault()
{
   // Let control have precedence over shift
   const bool cutPreview = mPlay->WasControlDown();
   const bool looped = !cutPreview &&
      mPlay->WasShiftDown();
   ProjectAudioManager::Get( mProject ).PlayCurrentRegion(looped, cutPreview);
}

/*! @excsafety{Strong} -- For state of current project's tracks */
void ControlToolBar::OnRecord(wxCommandEvent &evt)
{
   // TODO: It would be neater if Menu items and Toolbar buttons used the same code for
   // enabling/disabling, and all fell into the same action routines.
   // Here instead we reduplicate some logic (from CommandHandler) because it isn't
   // normally used for buttons.

   bool altAppearance = mRecord->WasShiftDown();
   ProjectAudioManager::Get( mProject ).OnRecord( altAppearance );
}

void ControlToolBar::OnPause(wxCommandEvent & WXUNUSED(evt))
{
   ProjectAudioManager::Get( mProject ).OnPause();
}

void ControlToolBar::OnIdle(wxIdleEvent & event)
{
   event.Skip();

   if (!wxTheApp->IsActive())
      return;

   auto &projectAudioManager = ProjectAudioManager::Get( mProject );
   if ( projectAudioManager.Paused() )
      mPause->PushDown();
   else
      mPause->PopUp();

   bool recording = projectAudioManager.Recording();
   if (!recording) {
      mRecord->PopUp();
      mRecord->SetAlternateIdx( wxGetKeyState(WXK_SHIFT) ? 1 : 0 );
   }
   else {
      mRecord->PushDown();
      mRecord->SetAlternateIdx( projectAudioManager.Appending() ? 0 : 1 );
   }

   bool playing = projectAudioManager.Playing();
   if ( !(playing || Scrubber::Get(mProject).HasMark()) ) {
      mPlay->PopUp();
      mPlay->SetAlternateIdx(
         wxGetKeyState(WXK_CONTROL)
         ? 2
         : wxGetKeyState(WXK_SHIFT)
            ? 1
            : 0
      );
   }
   else {
      mPlay->PushDown();
      mPlay->SetAlternateIdx(
         projectAudioManager.Cutting()
         ? 2
         : projectAudioManager.Looping()
            ? 1
            : 0
      );
   }

   if ( recording || playing )
      StartScrollingIfPreferred();
   else
      StopScrolling();

   if ( projectAudioManager.Stopping() )
      mStop->PushDown();
   else
      // push-downs of the stop button are only momentary and always pop up now
      mStop->PopUp();
   
   UpdateStatusBar();
   EnableDisableButtons();
}

void ControlToolBar::OnRewind(wxCommandEvent & WXUNUSED(evt))
{
   mRewind->PushDown();
   mRewind->PopUp();

   AudacityProject *p = &mProject;
   {
      ProjectAudioManager::Get( *p ).StopIfPaused();
      ProjectWindow::Get( *p ).Rewind(mRewind->WasShiftDown());
   }
}

void ControlToolBar::OnFF(wxCommandEvent & WXUNUSED(evt))
{
   mFF->PushDown();
   mFF->PopUp();

   AudacityProject *p = &mProject;

   {
      ProjectAudioManager::Get( *p ).StopIfPaused();
      ProjectWindow::Get( *p ).SkipEnd(mFF->WasShiftDown());
   }
}

// works out the width of the field in the status bar needed for the state (eg play, record pause)
static ProjectStatus::RegisteredStatusWidthFunction
registeredStatusWidthFunction{
   []( const AudacityProject &, StatusBarField field )
      -> ProjectStatus::StatusWidthResult
   {
      if ( field == stateStatusBarField ) {
         TranslatableStrings strings;
         for ( auto pString :
            { &sStatePlay, &sStateStop, &sStateRecord } )
         {
            strings.push_back(
   /* i18n-hint: These are strings for the status bar, and indicate whether Audacity
   is playing or recording or stopped, and whether it is paused. */
               XO("%s Paused.").Format(*pString) );
         }

         // added constant needed because xMax isn't large enough for some reason, plus some space.
         return { std::move( strings ), 30 };
      }
      return {};
   }
};

TranslatableString ControlToolBar::StateForStatusBar()
{
   TranslatableString state;
   auto &projectAudioManager = ProjectAudioManager::Get( mProject );

   auto pProject = &mProject;
   auto scrubState = pProject
      ? Scrubber::Get( *pProject ).GetUntranslatedStateString()
      : TranslatableString{};
   if (!scrubState.empty())
      state = scrubState;
   else if (mPlay->IsDown())
      state = sStatePlay;
   else if (projectAudioManager.Recording())
      state = sStateRecord;
   else
      state = sStateStop;

   return ((mPause->IsDown()) ? XO("%s Paused.") : XO("%s."))
      .Format( state );
}

void ControlToolBar::UpdateStatusBar()
{
   ProjectStatus::Get( mProject ).Set(
      StateForStatusBar(), stateStatusBarField
   );
}

void ControlToolBar::StartScrollingIfPreferred()
{
   if ( Scrubber::Get( mProject ).IsTransportingPinned() )
      StartScrolling();
#ifdef __WXMAC__
   else if (Scrubber::Get( mProject ).HasMark()) {
      // PRL:  cause many "unnecessary" refreshes.  For reasons I don't understand,
      // doing this causes wheel rotation events (mapped from the double finger vertical
      // swipe) to be delivered more uniformly to the application, so that speed control
      // works better.
      ProjectWindow::Get( mProject ).GetPlaybackScroller().Activate
         (ProjectWindow::PlaybackScroller::Mode::Refresh);
   }
#endif
   else
      StopScrolling();
}

void ControlToolBar::StartScrolling()
{
   using Mode = ProjectWindow::PlaybackScroller::Mode;
   const auto project = &mProject;
   if (project) {
      auto mode = Mode::Pinned;

#if 0
      // Enable these lines to pin the playhead right instead of center,
      // when recording but not overdubbing.
      auto gAudioIO = AudioIO::Get();
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

      ProjectWindow::Get( *project ).GetPlaybackScroller().Activate(mode);
   }
}

void ControlToolBar::StopScrolling()
{
   const auto project = &mProject;
   if(project)
      ProjectWindow::Get( *project ).GetPlaybackScroller().Activate
         (ProjectWindow::PlaybackScroller::Mode::Off);
}

static RegisteredToolbarFactory factory{ TransportBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{ safenew ControlToolBar{ project } }; }
};

namespace {
AttachedToolBarMenuItem sAttachment{
   /* i18n-hint: Clicking this menu item shows the toolbar
      with the big buttons on it (play record etc) */
   TransportBarID, wxT("ShowTransportTB"), XXO("&Transport Toolbar")
};
}
