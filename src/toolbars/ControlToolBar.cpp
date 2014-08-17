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

#include "../Audacity.h"
#include "../Experimental.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/dc.h>
#include <wx/event.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/timer.h>
#endif
#include <wx/tooltip.h>

#include "ControlToolBar.h"
#include "MeterToolBar.h"

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../Track.h"
#include "../widgets/AButton.h"

IMPLEMENT_CLASS(ControlToolBar, ToolBar);

//static
AudacityProject *ControlToolBar::mBusyProject = NULL;

////////////////////////////////////////////////////////////
/// Methods for ControlToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(ControlToolBar, ToolBar)
   EVT_CHAR(ControlToolBar::OnKeyEvent)
   EVT_TIMER(wxID_ANY, ControlToolBar::OnTimer)
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
   mShiftKeyTimer.SetOwner(this);

   mPaused = false;

   gPrefs->Read(wxT("/GUI/ErgonomicTransportButtons"), &mErgonomicTransportButtons, true);
   mStrLocale = gPrefs->Read(wxT("/Locale/Language"), wxT(""));

   mSizer = NULL;
   mCutPreviewTracks = NULL;
}

ControlToolBar::~ControlToolBar()
{
   wxTheApp->Disconnect( wxEVT_KEY_DOWN,
                         wxKeyEventHandler( ControlToolBar::OnKeyDown ),
                         NULL,
                         this );

   wxTheApp->Disconnect( wxEVT_KEY_UP,
                         wxKeyEventHandler( ControlToolBar::OnKeyUp ),
                         NULL,
                         this );
}


void ControlToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);

   wxTheApp->Connect( wxEVT_KEY_DOWN,
                      wxKeyEventHandler( ControlToolBar::OnKeyDown ),
                      NULL,
                      this );

   wxTheApp->Connect( wxEVT_KEY_UP,
                      wxKeyEventHandler( ControlToolBar::OnKeyUp ),
                      NULL,
                      this );
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

void ControlToolBar::MakeLoopImage()
{
   // JKC: See ToolBar::MakeButton() for almost identical code.  Condense??

   wxSize Size1( theTheme.ImageSize( bmpRecoloredUpLarge ));
   wxSize Size2( theTheme.ImageSize( bmpLoop ));

   int xoff = (Size1.GetWidth()  - Size2.GetWidth())/2;
   int yoff = (Size1.GetHeight() - Size2.GetHeight())/2;

   wxImage * up2        = OverlayImage(bmpRecoloredUpLarge,     bmpLoop, xoff, yoff);
   wxImage * hilite2    = OverlayImage(bmpRecoloredHiliteLarge, bmpLoop, xoff, yoff);
   wxImage * down2      = OverlayImage(bmpRecoloredDownLarge,   bmpLoop, xoff + 1, yoff + 1);
   wxImage * disable2   = OverlayImage(bmpRecoloredUpLarge,     bmpLoopDisabled, xoff, yoff);

   mPlay->SetAlternateImages(*up2, *hilite2, *down2, *disable2);

   delete up2;
   delete hilite2;
   delete down2;
   delete disable2;
}

void ControlToolBar::Populate()
{
   MakeButtonBackgroundsLarge();

   mPause = MakeButton(bmpPause, bmpPause, bmpPauseDisabled,
      ID_PAUSE_BUTTON,  true,  _("Pause"));

   mPlay = MakeButton( bmpPlay, bmpPlay, bmpPlayDisabled,
      ID_PLAY_BUTTON, true, _("Play"));

   MakeLoopImage();

   mStop = MakeButton( bmpStop, bmpStop, bmpStopDisabled ,
      ID_STOP_BUTTON, false, _("Stop"));

   mRewind = MakeButton(bmpRewind, bmpRewind, bmpRewindDisabled,
      ID_REW_BUTTON, false, _("Skip to Start"));

   mFF = MakeButton(bmpFFwd, bmpFFwd, bmpFFwdDisabled,
      ID_FF_BUTTON, false, _("Skip to End"));

   mRecord = MakeButton(bmpRecord, bmpRecord, bmpRecordDisabled,
      ID_RECORD_BUTTON, true, _("Record"));

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
   mSizer = new wxBoxSizer( wxHORIZONTAL );
   Add( mSizer, 1, wxEXPAND );

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
   // ToolBar::ReCreateButtons() will get rid of its sizer and
   // since we've attached our sizer to it, ours will get deleted too
   // so clean ours up first.
   if( mSizer )
   {
      Detach( mSizer );
      delete mSizer;
      mSizer = NULL;
   }

   ToolBar::ReCreateButtons();

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
   //TIDY-ME: Button logic could be neater.
   AudacityProject *p = GetActiveProject();
   bool tracks = false;
   bool playing = mPlay->IsDown();
   bool recording = mRecord->IsDown();
   bool busy = gAudioIO->IsBusy() || playing || recording;

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

   mPlay->SetEnabled((!recording) || (tracks && !busy));
   mRecord->SetEnabled(!busy && !playing);

   mStop->SetEnabled(busy);
   mRewind->SetEnabled(!busy);
   mFF->SetEnabled(tracks && !busy);
   mPause->SetEnabled(true);
}

void ControlToolBar::SetPlay(bool down, bool looped)
{
   if (down) {
      mPlay->SetAlternate(looped);
      mPlay->PushDown();
   } else {
      mPlay->PopUp();
      mPlay->SetAlternate(false);
   }
   EnableDisableButtons();
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

void ControlToolBar::SetRecord(bool down)
{
   if (down)
      mRecord->PushDown();
   else {
      mRecord->PopUp();
   }
   EnableDisableButtons();
}

bool ControlToolBar::IsRecordDown()
{
   return mRecord->IsDown();
}

void ControlToolBar::PlayPlayRegion(double t0, double t1,
                                    bool looped /* = false */,
                                    bool cutpreview /* = false */,
                                    TimeTrack *timetrack /* = NULL */)
{
   SetPlay(true, looped);

   if (gAudioIO->IsBusy()) {
      SetPlay(false);
      return;
   }

   if (cutpreview && t0==t1) {
      SetPlay(false);
      return; /* msmeyer: makes no sense */
   }

   AudacityProject *p = GetActiveProject();
   if (!p) {
      SetPlay(false);
      return;  // Should never happen, but...
   }

   TrackList *t = p->GetTracks();
   if (!t) {
      mPlay->PopUp();
      return;  // Should never happen, but...
   }

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

   if (!hasaudio) {
      SetPlay(false);
      return;  // No need to continue without audio tracks
   }

   double maxofmins,minofmaxs;
#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
   double init_seek = 0.0;
#endif

   // JS: clarified how the final play region is computed;
   if (t1 == t0) {
      // msmeyer: When playing looped, we play the whole file, if
      // no range is selected. Otherwise, we play from t0 to end
      if (looped) {
         // msmeyer: always play from start
         t0 = t->GetStartTime();
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

      // always play to end
      t1 = t->GetEndTime();
   }
   else {
      // always t0 < t1 right?

      // the set intersection between the play region and the
      // valid range maximum of lower bounds
      if (t0 < t->GetStartTime())
         maxofmins = t->GetStartTime();
      else
         maxofmins = t0;

      // minimum of upper bounds
      if (t1 > t->GetEndTime())
         minofmaxs = t->GetEndTime();
      else
         minofmaxs = t1;

      // we test if the intersection has no volume
      if (minofmaxs <= maxofmins) {
         // no volume; play nothing
         return;
      }
      else {
         t0 = maxofmins;
         t1 = minofmaxs;
      }
   }

   // Can't play before 0...either shifted or latencey corrected tracks
   if (t0 < 0.0) {
      t0 = 0.0;
   }

   bool success = false;
   if (t1 > t0) {
      int token;
      if (cutpreview) {
         double beforeLen, afterLen;
         gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);
         gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);
         double tcp0 = t0-beforeLen;
         double tcp1 = (t1+afterLen) - (t1-t0);
         SetupCutPreviewTracks(tcp0, t0, t1, tcp1);
         if (mCutPreviewTracks)
         {
            token = gAudioIO->StartStream(
               mCutPreviewTracks->GetWaveTrackArray(false),
               WaveTrackArray(),
#ifdef EXPERIMENTAL_MIDI_OUT
               NoteTrackArray(),
#endif
               NULL, p->GetRate(), tcp0, tcp1, p, false,
               t0, t1-t0);
         } else
         {
            // Cannot create cut preview tracks, clean up and exit
            SetPlay(false);
            SetStop(false);
            SetRecord(false);
            return;
         }
      } else {
         if (!timetrack) {
            timetrack = t->GetTimeTrack();
         }
         token = gAudioIO->StartStream(t->GetWaveTrackArray(false),
                                       WaveTrackArray(),
#ifdef EXPERIMENTAL_MIDI_OUT
                                       t->GetNoteTrackArray(false),
#endif
                                       timetrack,
                                       p->GetRate(), t0, t1, p, looped);
      }
      if (token != 0) {
         success = true;
         p->SetAudioIOToken(token);
         mBusyProject = p;
#if defined(EXPERIMENTAL_SEEK_BEHIND_CURSOR)
         //AC: If init_seek was set, now's the time to make it happen.
         gAudioIO->SeekStream(init_seek);
#endif
         SetVUMeters(p);
      }
      else {
         // msmeyer: Show error message if stream could not be opened
         wxMessageBox(_(
            "Error while opening sound device. "
            wxT("Please check the playback device settings and the project sample rate.")),
            _("Error"), wxOK | wxICON_EXCLAMATION, this);
      }
   }

   if (!success) {
      SetPlay(false);
      SetStop(false);
      SetRecord(false);
   }
}

void ControlToolBar::SetVUMeters(AudacityProject *p)
{
   MeterToolBar *bar;
   bar = p->GetMeterToolBar();
   if (bar) {
      Meter *play, *record;
      bar->GetMeters(&play, &record);
      gAudioIO->SetMeters(record, play);
   }
}

void ControlToolBar::PlayCurrentRegion(bool looped /* = false */,
                                       bool cutpreview /* = false */)
{
   AudacityProject *p = GetActiveProject();

   if (p)
   {
      if (looped)
         p->mLastPlayMode = loopedPlay;
      else
         p->mLastPlayMode = normalPlay;

      double playRegionStart, playRegionEnd;
      p->GetPlayRegion(&playRegionStart, &playRegionEnd);

      PlayPlayRegion(playRegionStart,
                     playRegionEnd,
                     looped, cutpreview);
   }
}

void ControlToolBar::OnKeyEvent(wxKeyEvent & event)
{
   if (event.ControlDown() || event.AltDown()) {
      event.Skip();
      return;
   }

   if (event.GetKeyCode() == WXK_SPACE) {
      if (gAudioIO->IsStreamActive(GetActiveProject()->GetAudioIOToken())) {
         SetPlay(false);
         SetStop(true);
         StopPlaying();
      }
      else if (!gAudioIO->IsBusy()) {
         SetPlay(true);// Not needed as done in PlayPlayRegion
         SetStop(false);
         PlayCurrentRegion();
      }
      return;
   }
   event.Skip();
}

void ControlToolBar::OnKeyDown(wxKeyEvent & event)
{
   event.Skip();

   if (event.GetKeyCode() == WXK_SHIFT)
   {
      // Turn the "Play" button into a "Loop" button
      if (!mPlay->IsDown())
         mPlay->SetAlternate(true);
      mShiftKeyTimer.Start(100);
   }
}

void ControlToolBar::OnKeyUp(wxKeyEvent & event)
{
   event.Skip();

   if (event.GetKeyCode() == WXK_SHIFT)
   {
      // Turn the "Loop" button into a "Play" button
      if (!mPlay->IsDown())
         mPlay->SetAlternate(false);
   }
}

void ControlToolBar::OnTimer(wxTimerEvent & event)
{
   event.Skip();

   // bug 307 fix:
   // Shift key-up events get swallowed if a command with a Shift in its keyboard
   // shortcut opens a dialog, and ControlToolBar::OnKeyUp() doesn't get called.
   if (!wxGetKeyState(WXK_SHIFT))
   {
      wxKeyEvent dummyEvent;
      dummyEvent.m_keyCode = WXK_SHIFT;
      this->OnKeyUp(dummyEvent);
      mShiftKeyTimer.Stop();
   }
}


void ControlToolBar::OnPlay(wxCommandEvent & WXUNUSED(evt))
{
   StopPlaying();

   AudacityProject *p = GetActiveProject();
   if (p) p->TP_DisplaySelection();

   PlayDefault();
}

void ControlToolBar::OnStop(wxCommandEvent & WXUNUSED(evt))
{
   StopPlaying();
}

void ControlToolBar::PlayDefault()
{
   if(mPlay->WasControlDown())
      PlayCurrentRegion(false, true); /* play with cut preview */
   else if(mPlay->WasShiftDown())
      PlayCurrentRegion(true); /* play looped */
   else
      PlayCurrentRegion(false); /* play normal */
}

void ControlToolBar::StopPlaying(bool stopStream /* = true*/)
{
   mStop->PushDown();

   SetStop(false);
   if(stopStream)
      gAudioIO->StopStream();
   SetPlay(false);
   SetRecord(false);

   #ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
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
   AudacityProject *project = GetActiveProject();
   if( project ) {
      project->MayStartMonitoring();
      project->GetMeterToolBar()->Clear();
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

   SetRecord(true);

   if (p) {
      TrackList *t = p->GetTracks();
      TrackListIterator it(t);
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
         playbackTracks = t->GetWaveTrackArray(false);
#ifdef EXPERIMENTAL_MIDI_OUT
         midiTracks = t->GetNoteTrackArray(false);
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
      bool shifted = mRecord->WasShiftDown();
      if (shifted) {
         WaveTrack *wt;
         bool sel = false;
         double allt0 = t0;

         // Find the maximum end time of selected and all wave tracks
         for (Track *tt = it.First(); tt; tt = it.Next()) {
            if (tt->GetKind() == Track::Wave) {
               wt = (WaveTrack *)tt;
               if (wt->GetEndTime() > allt0) {
                  allt0 = wt->GetEndTime();
               }

               if (tt->GetSelected()) {
                  sel = true;
                  if (duplex)
                     playbackTracks.Remove(wt);
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
         for (Track *tt = it.First(); tt; tt = it.Next()) {
            if (tt->GetKind() == Track::Wave && (tt->GetSelected() || !sel)) {
               wt = (WaveTrack *)tt;
               t1 = wt->GetEndTime();
               if (t1 < t0) {
                  WaveTrack *newTrack = p->GetTrackFactory()->NewWaveTrack();
                  newTrack->InsertSilence(0.0, t0 - t1);
                  newTrack->Flush();
                  wt->Clear(t1, t0);
                  bool bResult = wt->Paste(t1, newTrack);
                  wxASSERT(bResult); // TO DO: Actually handle this.
                  delete newTrack;
               }
               newRecordingTracks.Add(wt);
            }
         }

         t1 = 1000000000.0;     // record for a long, long time (tens of years)
      }
      else {
         recordingChannels = gPrefs->Read(wxT("/AudioIO/RecordChannels"), 2);
         for (int c = 0; c < recordingChannels; c++) {
            WaveTrack *newTrack = p->GetTrackFactory()->NewWaveTrack();

            newTrack->SetOffset(t0);

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

            newRecordingTracks.Add(newTrack);
         }

         // msmeyer: StartStream calls a callback which triggers auto-save, so
         // we add the tracks where recording is done into now. We remove them
         // later if starting the stream fails
         for (unsigned int i = 0; i < newRecordingTracks.GetCount(); i++)
            t->Add(newRecordingTracks[i]);
      }

      //Automated Input Level Adjustment Initialization
      #ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
         gAudioIO->AILAInitialize();
      #endif

      int token = gAudioIO->StartStream(playbackTracks,
                                        newRecordingTracks,
#ifdef EXPERIMENTAL_MIDI_OUT
                                        midiTracks,
#endif
                                        t->GetTimeTrack(),
                                        p->GetRate(), t0, t1, p);

      bool success = (token != 0);

      if (success) {
         p->SetAudioIOToken(token);
         mBusyProject = p;
         SetVUMeters(p);
      }
      else {
         // msmeyer: Delete recently added tracks if opening stream fails
         if (!shifted) {
            for (unsigned int i = 0; i < newRecordingTracks.GetCount(); i++) {
               t->Remove(newRecordingTracks[i]);
               delete newRecordingTracks[i];
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
}


void ControlToolBar::OnPause(wxCommandEvent & WXUNUSED(evt))
{
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
         track1 = track1->Duplicate();
         track1->Clear(cutStart, cutEnd);
         if (track2)
         {
            track2 = track2->Duplicate();
            track2->Clear(cutStart, cutEnd);
         }

         mCutPreviewTracks = new TrackList();
         mCutPreviewTracks->Add(track1);
         if (track2)
            mCutPreviewTracks->Add(track2);
      }
   }
}

void ControlToolBar::ClearCutPreviewTracks()
{
   if (mCutPreviewTracks)
   {
      mCutPreviewTracks->Clear(true); /* delete track contents too */
      delete mCutPreviewTracks;
      mCutPreviewTracks = NULL;
   }
}

