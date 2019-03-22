#include "../Audacity.h" // for USE_* macros
#include "../AdornedRulerPanel.h"
#include "../AudacityApp.h" // for EVT_CLIPBOARD_CHANGE
#include "../LabelTrack.h"
#include "../Menus.h"
#include "../NoteTrack.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../TimeTrack.h"
#include "../TrackPanel.h"
#include "../UndoManager.h"
#include "../WaveTrack.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../commands/ScreenshotCommand.h"
#include "../prefs/PrefsDialog.h"
#include "../prefs/SpectrogramSettings.h"
#include "../prefs/WaveformSettings.h"

// private helper classes and functions
namespace {
void FinishCopy
   (const Track *n, const Track::Holder &dest, TrackList &list)
{
   Track::FinishCopy( n, dest.get() );
   if (dest)
      list.Add( dest );
}

// Handle text paste (into active label), if any. Return true if did paste.
// (This was formerly the first part of overly-long OnPaste.)
bool DoPasteText(AudacityProject &project)
{
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   for (auto pLabelTrack : tracks->Any<LabelTrack>())
   {
      // Does this track have an active label?
      if (pLabelTrack->HasSelection()) {

         // Yes, so try pasting into it
         if (pLabelTrack->PasteSelectedText(selectedRegion.t0(),
                                            selectedRegion.t1()))
         {
            project.PushState(_("Pasted text from the clipboard"), _("Paste"));

            // Make sure caret is in view
            int x;
            if (pLabelTrack->CalcCursorX(&x)) {
               trackPanel->ScrollIntoView(x);
            }

            // Redraw everyting (is that necessary???) and bail
            project.RedrawProject();
            return true;
         }
      }
   }
   return false;
}

// Return true if nothing selected, regardless of paste result.
// If nothing was selected, create and paste into NEW tracks.
// (This was formerly the second part of overly-long OnPaste.)
bool DoPasteNothingSelected(AudacityProject &project)
{
   auto tracks = project.GetTracks();
   auto trackFactory = project.GetTrackFactory();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   // First check whether anything's selected.
   if (tracks->Selected())
      return false;
   else
   {
      auto clipTrackRange = AudacityProject::msClipboard->Any< const Track >();
      if (clipTrackRange.empty())
         return true; // nothing to paste

      Track* pFirstNewTrack = NULL;
      for (auto pClip : clipTrackRange) {
         Maybe<WaveTrack::Locker> locker;

         Track::Holder uNewTrack;
         Track *pNewTrack;
         pClip->TypeSwitch(
            [&](const WaveTrack *wc) {
               if ((AudacityProject::msClipProject != &project))
                  // Cause duplication of block files on disk, when copy is
                  // between projects
                  locker.create(wc);
               uNewTrack = trackFactory->NewWaveTrack(
                  wc->GetSampleFormat(), wc->GetRate()),
               pNewTrack = uNewTrack.get();
            },
#ifdef USE_MIDI
            [&](const NoteTrack *) {
               uNewTrack = trackFactory->NewNoteTrack(),
               pNewTrack = uNewTrack.get();
            },
#endif
            [&](const LabelTrack *) {
               uNewTrack = trackFactory->NewLabelTrack(),
               pNewTrack = uNewTrack.get();
            },
            [&](const TimeTrack *) {
               // Maintain uniqueness of the time track!
               pNewTrack = tracks->GetTimeTrack();
               if (!pNewTrack)
                  uNewTrack = trackFactory->NewTimeTrack(),
                  pNewTrack = uNewTrack.get();
            }
         );

         wxASSERT(pClip);

         pNewTrack->Paste(0.0, pClip);

         if (!pFirstNewTrack)
            pFirstNewTrack = pNewTrack;

         pNewTrack->SetSelected(true);
         if (uNewTrack)
            FinishCopy(pClip, uNewTrack, *tracks);
         else
            Track::FinishCopy(pClip, pNewTrack);
      }

      // Select some pasted samples, which is probably impossible to get right
      // with various project and track sample rates.
      // So do it at the sample rate of the project
      AudacityProject *p = GetActiveProject();
      double projRate = p->GetRate();
      double quantT0 = QUANTIZED_TIME(AudacityProject::msClipT0, projRate);
      double quantT1 = QUANTIZED_TIME(AudacityProject::msClipT1, projRate);
      selectedRegion.setTimes(
         0.0,   // anywhere else and this should be
                // half a sample earlier
         quantT1 - quantT0);

      project.PushState(_("Pasted from the clipboard"), _("Paste"));

      project.RedrawProject();

      if (pFirstNewTrack)
         trackPanel->EnsureVisible(pFirstNewTrack);

      return true;
   }
}

}

namespace EditActions {

// exported helper functions

void DoReloadPreferences( AudacityProject &project )
{
   {
      SpectrogramSettings::defaults().LoadPrefs();
      WaveformSettings::defaults().LoadPrefs();

      GlobalPrefsDialog dialog(&project /* parent */ );
      wxCommandEvent Evt;
      //dialog.Show();
      dialog.OnOK(Evt);
   }

   // LL:  Moved from PrefsDialog since wxWidgets on OSX can't deal with
   //      rebuilding the menus while the PrefsDialog is still in the modal
   //      state.
   for (size_t i = 0; i < gAudacityProjects.size(); i++) {
      AudacityProject *p = gAudacityProjects[i].get();

      GetMenuManager(*p).RebuildMenuBar(*p);
// TODO: The comment below suggests this workaround is obsolete.
#if defined(__WXGTK__)
      // Workaround for:
      //
      //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
      //
      // This workaround should be removed when Audacity updates to wxWidgets
      // 3.x which has a fix.
      wxRect r = p->GetRect();
      p->SetSize(wxSize(1,1));
      p->SetSize(r.GetSize());
#endif
   }
}

bool DoEditMetadata
(AudacityProject &project,
 const wxString &title, const wxString &shortUndoDescription, bool force)
{
   auto tags = project.GetTags();

   // Back up my tags
   auto newTags = tags->Duplicate();

   if (newTags->ShowEditDialog(&project, title, force)) {
      if (*tags != *newTags) {
         // Commit the change to project state only now.
         project.SetTags( newTags );
         project.PushState(title, shortUndoDescription);
      }

      return true;
   }

   return false;
}

void DoUndo(AudacityProject &project)
{
   auto trackPanel = project.GetTrackPanel();
   auto &undoManager = *project.GetUndoManager();

   if (!project.UndoAvailable()) {
      AudacityMessageBox(_("Nothing to undo"));
      return;
   }

   // can't undo while dragging
   if (trackPanel->IsMouseCaptured()) {
      return;
   }

   undoManager.Undo(
      [&]( const UndoState &state ){ project.PopState( state ); } );

   trackPanel->EnsureVisible(trackPanel->GetFirstSelectedTrack());

   project.RedrawProject();

   MenuManager::ModifyUndoMenuItems(project);
}

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnUndo(const CommandContext &context)
{
   DoUndo(context.project);
}

void OnRedo(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto &undoManager = *project.GetUndoManager();

   if (!project.RedoAvailable()) {
      AudacityMessageBox(_("Nothing to redo"));
      return;
   }
   // Can't redo whilst dragging
   if (trackPanel->IsMouseCaptured()) {
      return;
   }

   undoManager.Redo(
      [&]( const UndoState &state ){ project.PopState( state ); } );

   trackPanel->EnsureVisible(trackPanel->GetFirstSelectedTrack());

   project.RedrawProject();

   MenuManager::ModifyUndoMenuItems(project);
}

void OnCut(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;
   auto ruler = project.GetRulerPanel();

   // This doesn't handle cutting labels, it handles
   // cutting the _text_ inside of labels, i.e. if you're
   // in the middle of editing the label text and select "Cut".

   for (auto lt : tracks->Selected< LabelTrack >()) {
      if (lt->CutSelectedText()) {
         trackPanel->Refresh(false);
         return;
      }
   }

   AudacityProject::ClearClipboard();

   auto pNewClipboard = TrackList::Create();
   auto &newClipboard = *pNewClipboard;

   tracks->Selected().Visit(
#if defined(USE_MIDI)
      [&](NoteTrack *n) {
         // Since portsmf has a built-in cut operator, we use that instead
         auto dest = n->Cut(selectedRegion.t0(),
                selectedRegion.t1());
         FinishCopy(n, dest, newClipboard);
      },
#endif
      [&](Track *n) {
         auto dest = n->Copy(selectedRegion.t0(),
                 selectedRegion.t1());
         FinishCopy(n, dest, newClipboard);
      }
   );

   // Survived possibility of exceptions.  Commit changes to the clipboard now.
   newClipboard.Swap(*AudacityProject::msClipboard);
   wxTheApp->AddPendingEvent( wxCommandEvent{ EVT_CLIPBOARD_CHANGE } );

   // Proceed to change the project.  If this throws, the project will be
   // rolled back by the top level handler.

   (tracks->Any() + &Track::IsSelectedOrSyncLockSelected).Visit(
#if defined(USE_MIDI)
      [](NoteTrack*) {
         //if NoteTrack, it was cut, so do not clear anything

         // PRL:  But what if it was sync lock selected only, not selected?
      },
#endif
      [&](WaveTrack *wt, const Track::Fallthrough &fallthrough) {
         if (gPrefs->Read(wxT("/GUI/EnableCutLines"), (long)0)) {
            wt->ClearAndAddCutLine(
               selectedRegion.t0(),
               selectedRegion.t1());
         }
         else
            fallthrough();
      },
      [&](Track *n) {
         n->Clear(selectedRegion.t0(),
                  selectedRegion.t1());
      }
   );

   AudacityProject::msClipT0 = selectedRegion.t0();
   AudacityProject::msClipT1 = selectedRegion.t1();
   AudacityProject::msClipProject = &project;

   selectedRegion.collapseToT0();

   project.PushState(_("Cut to the clipboard"), _("Cut"));

   // Bug 1663
   //mRuler->ClearPlayRegion();
   ruler->DrawOverlays( true );

   project.RedrawProject();
}

void OnDelete(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = *project.GetTracks();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   for (auto n : tracks.Any()) {
      if (n->GetSelected() || n->IsSyncLockSelected()) {
         n->Clear(selectedRegion.t0(), selectedRegion.t1());
      }
   }

   double seconds = selectedRegion.duration();

   selectedRegion.collapseToT0();

   project.PushState(wxString::Format(_("Deleted %.2f seconds at t=%.2f"),
                              seconds,
                              selectedRegion.t0()),
             _("Delete"));

   project.RedrawProject();
}


void OnCopy(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   for (auto lt : tracks->Selected< LabelTrack >()) {
      if (lt->CopySelectedText()) {
         //trackPanel->Refresh(false);
         return;
      }
   }

   AudacityProject::ClearClipboard();

   auto pNewClipboard = TrackList::Create();
   auto &newClipboard = *pNewClipboard;

   for (auto n : tracks->Selected()) {
      auto dest = n->Copy(selectedRegion.t0(),
              selectedRegion.t1());
      FinishCopy(n, dest, newClipboard);
   }

   // Survived possibility of exceptions.  Commit changes to the clipboard now.
   newClipboard.Swap(*AudacityProject::msClipboard);
   wxTheApp->AddPendingEvent( wxCommandEvent{ EVT_CLIPBOARD_CHANGE } );

   AudacityProject::msClipT0 = selectedRegion.t0();
   AudacityProject::msClipT1 = selectedRegion.t1();
   AudacityProject::msClipProject = &project;

   //Make sure the menus/toolbar states get updated
   trackPanel->Refresh(false);
}

void OnPaste(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto trackFactory = project.GetTrackFactory();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;
   auto isSyncLocked = project.IsSyncLocked();

   // Handle text paste (into active label) first.
   if (DoPasteText(project))
      return;

   // If nothing's selected, we just insert NEW tracks.
   if (DoPasteNothingSelected(project))
      return;

   auto clipTrackRange = AudacityProject::msClipboard->Any< const Track >();
   if (clipTrackRange.empty())
      return;

   // Otherwise, paste into the selected tracks.
   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();

   auto pN = tracks->Any().begin();

   Track *ff = NULL;
   const Track *lastClipBeforeMismatch = NULL;
   const Track *mismatchedClip = NULL;
   const Track *prevClip = NULL;

   bool bAdvanceClipboard = true;
   bool bPastedSomething = false;

   auto pC = clipTrackRange.begin();
   size_t nnChannels=0, ncChannels=0;
   while (*pN && *pC) {
      auto n = *pN;
      auto c = *pC;
      if (n->GetSelected()) {
         bAdvanceClipboard = true;
         if (mismatchedClip)
            c = mismatchedClip;
         if (!c->SameKindAs(*n)) {
            if (!mismatchedClip) {
               lastClipBeforeMismatch = prevClip;
               mismatchedClip = c;
            }
            bAdvanceClipboard = false;
            c = lastClipBeforeMismatch;


            // If the types still don't match...
            while (c && !c->SameKindAs(*n)) {
               prevClip = c;
               c = * ++ pC;
            }
         }

         // Handle case where the first track in clipboard
         // is of different type than the first selected track
         if (!c) {
            c = mismatchedClip;
            while (n && (!c->SameKindAs(*n) || !n->GetSelected()))
            {
               // Must perform sync-lock adjustment before incrementing n
               if (n->IsSyncLockSelected()) {
                  auto newT1 = t0 +
                     (AudacityProject::msClipT1 - AudacityProject::msClipT0);
                  if (t1 != newT1 && t1 <= n->GetEndTime()) {
                     n->SyncLockAdjust(t1, newT1);
                     bPastedSomething = true;
                  }
               }
               n = * ++ pN;
            }
            if (!n)
               c = NULL;
         }

         // The last possible case for cross-type pastes: triggered when we try
         // to paste 1+ tracks from one type into 1+ tracks of another type. If
         // there's a mix of types, this shouldn't run.
         if (!c)
            // Throw, so that any previous changes to the project in this loop
            // are discarded.
            throw SimpleMessageBoxException{
               _("Pasting one type of track into another is not allowed.")
            };

         // We should need this check only each time we visit the leading
         // channel
         if ( n->IsLeader() ) {
            wxASSERT( c->IsLeader() ); // the iteration logic should ensure this

            auto cChannels = TrackList::Channels(c);
            ncChannels = cChannels.size();
            auto nChannels = TrackList::Channels(n);
            nnChannels = nChannels.size();

            // When trying to copy from stereo to mono track, show error and
            // exit
            // TODO: Automatically offer user to mix down to mono (unfortunately
            //       this is not easy to implement
            if (ncChannels > nnChannels)
            {
               if (ncChannels > 2) {
                  // TODO: more-than-two-channels-message
                  // Re-word the error message
               }
               // else

               // Throw, so that any previous changes to the project in this
               // loop are discarded.
               throw SimpleMessageBoxException{
                  _("Copying stereo audio into a mono track is not allowed.")
               };
            }
         }

         if (!ff)
            ff = n;
         
         wxASSERT( n && c && n->SameKindAs(*c) );
         Maybe<WaveTrack::Locker> locker;

         n->TypeSwitch(
            [&](WaveTrack *wn){
               const auto wc = static_cast<const WaveTrack *>(c);
               if (AudacityProject::msClipProject != &project)
                  // Cause duplication of block files on disk, when copy is
                  // between projects
                  locker.create(wc);
               bPastedSomething = true;
               wn->ClearAndPaste(t0, t1, wc, true, true);
            },
            [&](LabelTrack *ln){
               // Per Bug 293, users expect labels to move on a paste into
               // a label track.
               ln->Clear(t0, t1);

               ln->ShiftLabelsOnInsert(
                  AudacityProject::msClipT1 - AudacityProject::msClipT0, t0);

               bPastedSomething |= ln->PasteOver(t0, c);
            },
            [&](Track *){
               bPastedSomething = true;
               n->Clear(t0, t1);
               n->Paste(t0, c);
            }
         );

         --nnChannels;
         --ncChannels;

         // When copying from mono to stereo track, paste the wave form
         // to both channels
         // TODO: more-than-two-channels
         // This will replicate the last pasted channel as many times as needed
         while (nnChannels > 0 && ncChannels == 0)
         {
            n = * ++ pN;
            --nnChannels;

            n->TypeSwitch(
               [&](WaveTrack *wn){
                  bPastedSomething = true;
                  // Note:  rely on locker being still be in scope!
                  wn->ClearAndPaste(t0, t1, c, true, true);
               },
               [&](Track *){
                  n->Clear(t0, t1);
                  bPastedSomething = true;
                  n->Paste(t0, c);
               }
            );
         }

         if (bAdvanceClipboard) {
            prevClip = c;
            c = * ++ pC;
         }
      } // if (n->GetSelected())
      else if (n->IsSyncLockSelected())
      {
         auto newT1 = t0 +
            (AudacityProject::msClipT1 - AudacityProject::msClipT0);
         if (t1 != newT1 && t1 <= n->GetEndTime()) {
            n->SyncLockAdjust(t1, newT1);
            bPastedSomething = true;
         }
      }
      ++pN;
   }

   // This block handles the cases where our clipboard is smaller
   // than the amount of selected destination tracks. We take the
   // last wave track, and paste that one into the remaining
   // selected tracks.
   if ( *pN && ! *pC )
   {
      const auto wc =
         *AudacityProject::msClipboard->Any< const WaveTrack >().rbegin();
      Maybe<WaveTrack::Locker> locker;
      if (AudacityProject::msClipProject != &project && wc)
         // Cause duplication of block files on disk, when copy is
         // between projects
         locker.create(static_cast<const WaveTrack*>(wc));

      tracks->Any().StartingWith(*pN).Visit(
         [&](WaveTrack *wt, const Track::Fallthrough &fallthrough) {
            if (!wt->GetSelected())
               return fallthrough();

            if (wc) {
               bPastedSomething = true;
               wt->ClearAndPaste(t0, t1, wc, true, true);
            }
            else {
               auto tmp = trackFactory->NewWaveTrack(
                  wt->GetSampleFormat(), wt->GetRate());
               tmp->InsertSilence(0.0,
                  // MJS: Is this correct?
                  AudacityProject::msClipT1 - AudacityProject::msClipT0);
               tmp->Flush();

               bPastedSomething = true;
               wt->ClearAndPaste(t0, t1, tmp.get(), true, true);
            }
         },
         [&](LabelTrack *lt, const Track::Fallthrough &fallthrough) {
            if (!lt->GetSelected())
               return fallthrough();

            lt->Clear(t0, t1);

            // As above, only shift labels if sync-lock is on.
            if (isSyncLocked)
               lt->ShiftLabelsOnInsert(
                  AudacityProject::msClipT1 - AudacityProject::msClipT0, t0);
         },
         [&](Track *n) {
            if (n->IsSyncLockSelected())
               n->SyncLockAdjust(t1, t0 +
                  AudacityProject::msClipT1 - AudacityProject::msClipT0);
         }
      );
   }

   // TODO: What if we clicked past the end of the track?

   if (bPastedSomething)
   {
      selectedRegion.setT1(
         t0 + AudacityProject::msClipT1 - AudacityProject::msClipT0);

      project.PushState(_("Pasted from the clipboard"), _("Paste"));

      project.RedrawProject();

      if (ff)
         trackPanel->EnsureVisible(ff);
   }
}

void OnDuplicate(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   // This iteration is unusual because we add to the list inside the loop
   auto range = tracks->Selected();
   auto last = *range.rbegin();
   for (auto n : range) {
      // Make copies not for clipboard but for direct addition to the project
      auto dest = n->Copy(selectedRegion.t0(),
              selectedRegion.t1(), false);
      dest->Init(*n);
      dest->SetOffset(wxMax(selectedRegion.t0(), n->GetOffset()));
      tracks->Add( dest );

      // This break is really needed, else we loop infinitely
      if (n == last)
         break;
   }

   project.PushState(_("Duplicated"), _("Duplicate"));

   project.RedrawProject();
}

void OnSplitCut(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   AudacityProject::ClearClipboard();

   auto pNewClipboard = TrackList::Create();
   auto &newClipboard = *pNewClipboard;

   Track::Holder dest;

   tracks->Selected().Visit(
      [&](WaveTrack *n) {
         dest = n->SplitCut(
            selectedRegion.t0(),
            selectedRegion.t1());
         if (dest)
            FinishCopy(n, dest, newClipboard);
      },
      [&](Track *n) {
         dest = n->Copy(selectedRegion.t0(),
                 selectedRegion.t1());
         n->Silence(selectedRegion.t0(),
                    selectedRegion.t1());
         if (dest)
            FinishCopy(n, dest, newClipboard);
      }
   );

   // Survived possibility of exceptions.  Commit changes to the clipboard now.
   newClipboard.Swap(*AudacityProject::msClipboard);
   wxTheApp->AddPendingEvent( wxCommandEvent{ EVT_CLIPBOARD_CHANGE } );

   AudacityProject::msClipT0 = selectedRegion.t0();
   AudacityProject::msClipT1 = selectedRegion.t1();
   AudacityProject::msClipProject = &project;

   project.PushState(_("Split-cut to the clipboard"), _("Split Cut"));

   project.RedrawProject();
}

void OnSplitDelete(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   tracks->Selected().Visit(
      [&](WaveTrack *wt) {
         wt->SplitDelete(selectedRegion.t0(),
                         selectedRegion.t1());
      },
      [&](Track *n) {
         n->Silence(selectedRegion.t0(),
                    selectedRegion.t1());
      }
   );

   project.PushState(
      wxString::Format(_("Split-deleted %.2f seconds at t=%.2f"),
         selectedRegion.duration(),
         selectedRegion.t0()),
      _("Split Delete"));

   project.RedrawProject();
}

void OnSilence(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   for ( auto n : tracks->Selected< AudioTrack >() )
      n->Silence(selectedRegion.t0(), selectedRegion.t1());

   project.PushState(
      wxString::Format(_("Silenced selected tracks for %.2f seconds at %.2f"),
         selectedRegion.duration(),
         selectedRegion.t0()),
      _("Silence"));

   trackPanel->Refresh(false);
}

void OnTrim(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if (selectedRegion.isPoint())
      return;

   tracks->Selected().Visit(
#ifdef USE_MIDI
      [&](NoteTrack *nt) {
         nt->Trim(selectedRegion.t0(),
            selectedRegion.t1());
      },
#endif
      [&](WaveTrack *wt) {
         //Delete the section before the left selector
         wt->Trim(selectedRegion.t0(),
            selectedRegion.t1());
      }
   );

   project.PushState(
      wxString::Format(
         _("Trim selected audio tracks from %.2f seconds to %.2f seconds"),
         selectedRegion.t0(), selectedRegion.t1()),
         _("Trim Audio"));

   project.RedrawProject();
}

void OnSplit(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   double sel0 = selectedRegion.t0();
   double sel1 = selectedRegion.t1();

   for (auto wt : tracks->Selected< WaveTrack >())
      wt->Split( sel0, sel1 );

   project.PushState(_("Split"), _("Split"));
   trackPanel->Refresh(false);
#if 0
//ANSWER-ME: Do we need to keep this commented out OnSplit() code?
// This whole section no longer used...
   /*
    * Previous (pre-multiclip) implementation of "Split" command
    * This does work only when a range is selected!
    *
   TrackListIterator iter(tracks);

   Track *n = iter.First();
   Track *dest;

   TrackList newTracks;

   while (n) {
      if (n->GetSelected()) {
         double sel0 = selectedRegion.t0();
         double sel1 = selectedRegion.t1();

         dest = n->Copy(sel0, sel1);
         dest->Init(*n);
         dest->SetOffset(wxMax(sel0, n->GetOffset()));

         if (sel1 >= n->GetEndTime())
            n->Clear(sel0, sel1);
         else if (sel0 <= n->GetOffset()) {
            n->Clear(sel0, sel1);
            n->SetOffset(sel1);
         } else
            n->Silence(sel0, sel1);

         newTracks.Add(dest);
      }
      n = iter.Next();
   }

   TrackListIterator nIter(&newTracks);
   n = nIter.First();
   while (n) {
      tracks->Add(n);
      n = nIter.Next();
   }

   PushState(_("Split"), _("Split"));

   FixScrollbars();
   trackPanel->Refresh(false);
   */
#endif
}

void OnSplitNew(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   Track::Holder dest;

   // This iteration is unusual because we add to the list inside the loop
   auto range = tracks->Selected();
   auto last = *range.rbegin();
   for (auto track : range) {
      track->TypeSwitch(
         [&](WaveTrack *wt) {
            // Clips must be aligned to sample positions or the NEW clip will
            // not fit in the gap where it came from
            double offset = wt->GetOffset();
            offset = wt->LongSamplesToTime(wt->TimeToLongSamples(offset));
            double newt0 = wt->LongSamplesToTime(wt->TimeToLongSamples(
               selectedRegion.t0()));
            double newt1 = wt->LongSamplesToTime(wt->TimeToLongSamples(
               selectedRegion.t1()));
            dest = wt->SplitCut(newt0, newt1);
            if (dest) {
               dest->SetOffset(wxMax(newt0, offset));
               FinishCopy(wt, dest, *tracks);
            }
         }
#if 0
         ,
         // LL:  For now, just skip all non-wave tracks since the other do not
         //      yet support proper splitting.
         [&](Track *n) {
            dest = n->Cut(viewInfo.selectedRegion.t0(),
                   viewInfo.selectedRegion.t1());
            if (dest) {
               dest->SetOffset(wxMax(0, n->GetOffset()));
               FinishCopy(n, dest, *tracks);
            }
         }
#endif
      );
      if (track == last)
         break;
   }

   project.PushState(_("Split to new track"), _("Split New"));

   project.RedrawProject();
}

void OnJoin(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   for (auto wt : tracks->Selected< WaveTrack >())
      wt->Join(selectedRegion.t0(),
               selectedRegion.t1());

   project.PushState(
      wxString::Format(_("Joined %.2f seconds at t=%.2f"),
         selectedRegion.duration(),
         selectedRegion.t0()),
      _("Join"));

   project.RedrawProject();
}

void OnDisjoin(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   for (auto wt : tracks->Selected< WaveTrack >())
      wt->Disjoin(selectedRegion.t0(),
                  selectedRegion.t1());

   project.PushState(
      wxString::Format(_("Detached %.2f seconds at t=%.2f"),
         selectedRegion.duration(),
         selectedRegion.t0()),
      _("Detach"));

   project.RedrawProject();
}

void OnEditMetadata(const CommandContext &context)
{
   auto &project = context.project;
   (void)DoEditMetadata( project,
      _("Edit Metadata Tags"), _("Metadata Tags"), true);
}

void OnPreferences(const CommandContext &context)
{
   auto &project = context.project;

   GlobalPrefsDialog dialog(&project /* parent */ );

   if( ScreenshotCommand::MayCapture( &dialog ) )
      return;

   if (!dialog.ShowModal()) {
      // Canceled
      return;
   }

   // LL:  Moved from PrefsDialog since wxWidgets on OSX can't deal with
   //      rebuilding the menus while the PrefsDialog is still in the modal
   //      state.
   for (size_t i = 0; i < gAudacityProjects.size(); i++) {
      AudacityProject *p = gAudacityProjects[i].get();

      GetMenuManager(*p).RebuildMenuBar(*p);
// TODO: The comment below suggests this workaround is obsolete.
#if defined(__WXGTK__)
      // Workaround for:
      //
      //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
      //
      // This workaround should be removed when Audacity updates to wxWidgets
      // 3.x which has a fix.
      wxRect r = p->GetRect();
      p->SetSize(wxSize(1,1));
      p->SetSize(r.GetSize());
#endif
   }
}

// Legacy functions, not used as of version 2.3.0

#if 0
void OnPasteOver(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   if((AudacityProject::msClipT1 - AudacityProject::msClipT0) > 0.0)
   {
      selectedRegion.setT1(
         selectedRegion.t0() +
         (AudacityProject::msClipT1 - AudacityProject::msClipT0));
         // MJS: pointless, given what we do in OnPaste?
   }
   OnPaste(context);

   return;
}
#endif

}; // struct Handler

} // namespace

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static EditActions::Handler instance;
   return instance;
};

// Menu definitions

#define FN(X) findCommandHandler, \
   static_cast<CommandFunctorPointer>(& EditActions::Handler :: X)
#define XXO(X) _(X), wxString{X}.Contains("...")

MenuTable::BaseItemPtr LabelEditMenus( AudacityProject &project );

MenuTable::BaseItemPtr EditMenu( AudacityProject & )
{
   using namespace MenuTable;
   using Options = CommandManager::Options;

   constexpr auto NotBusyTimeAndTracksFlags =
      AudioIONotBusyFlag | TimeSelectedFlag | TracksSelectedFlag;

   // The default shortcut key for Redo is different on different platforms.
   static constexpr auto redoKey =
#ifdef __WXMSW__
      wxT("Ctrl+Y")
#else
      wxT("Ctrl+Shift+Z")
#endif
   ;

      // The default shortcut key for Preferences is different on different
      // platforms.
   static constexpr auto prefKey =
#ifdef __WXMAC__
      wxT("Ctrl+,")
#else
      wxT("Ctrl+P")
#endif
   ;

   return Menu( _("&Edit"),
      Command( wxT("Undo"), XXO("&Undo"), FN(OnUndo),
         AudioIONotBusyFlag | UndoAvailableFlag, wxT("Ctrl+Z") ),

      Command( wxT("Redo"), XXO("&Redo"), FN(OnRedo),
         AudioIONotBusyFlag | RedoAvailableFlag, redoKey ),
         
      Special( [](AudacityProject &project, wxMenu&) {
         // Change names in the CommandManager as a side-effect
         MenuManager::ModifyUndoMenuItems(project);
      }),

      Separator(),

      // Basic Edit commands
      /* i18n-hint: (verb)*/
      Command( wxT("Cut"), XXO("Cu&t"), FN(OnCut),
         AudioIONotBusyFlag | CutCopyAvailableFlag | NoAutoSelect,
         Options{ wxT("Ctrl+X") }
            .Mask( AudioIONotBusyFlag | CutCopyAvailableFlag ) ),
      Command( wxT("Delete"), XXO("&Delete"), FN(OnDelete),
         AudioIONotBusyFlag | NoAutoSelect,
         Options{ wxT("Ctrl+K") }
            .Mask( AudioIONotBusyFlag ) ),
      /* i18n-hint: (verb)*/
      Command( wxT("Copy"), XXO("&Copy"), FN(OnCopy),
         AudioIONotBusyFlag | CutCopyAvailableFlag, wxT("Ctrl+C") ),
      /* i18n-hint: (verb)*/
      Command( wxT("Paste"), XXO("&Paste"), FN(OnPaste),
         AudioIONotBusyFlag, wxT("Ctrl+V") ),
      /* i18n-hint: (verb)*/
      Command( wxT("Duplicate"), XXO("Duplic&ate"), FN(OnDuplicate),
         NotBusyTimeAndTracksFlags, wxT("Ctrl+D") ),

      Separator(),

      Menu( _("R&emove Special"),
         /* i18n-hint: (verb) Do a special kind of cut*/
         Command( wxT("SplitCut"), XXO("Spl&it Cut"), FN(OnSplitCut),
            NotBusyTimeAndTracksFlags, wxT("Ctrl+Alt+X") ),
         /* i18n-hint: (verb) Do a special kind of DELETE*/
         Command( wxT("SplitDelete"), XXO("Split D&elete"), FN(OnSplitDelete),
            NotBusyTimeAndTracksFlags, wxT("Ctrl+Alt+K") ),

         Separator(),

         /* i18n-hint: (verb)*/
         Command( wxT("Silence"), XXO("Silence Audi&o"), FN(OnSilence),
            AudioIONotBusyFlag | TimeSelectedFlag | AudioTracksSelectedFlag,
            wxT("Ctrl+L") ),
         /* i18n-hint: (verb)*/
         Command( wxT("Trim"), XXO("Tri&m Audio"), FN(OnTrim),
            AudioIONotBusyFlag | TimeSelectedFlag | AudioTracksSelectedFlag,
            wxT("Ctrl+T") )
      ),

      Separator(),

      //////////////////////////////////////////////////////////////////////////

      Menu( _("Clip B&oundaries"),
         /* i18n-hint: (verb) It's an item on a menu. */
         Command( wxT("Split"), XXO("Sp&lit"), FN(OnSplit),
            AudioIONotBusyFlag | WaveTracksSelectedFlag, wxT("Ctrl+I") ),
         Command( wxT("SplitNew"), XXO("Split Ne&w"), FN(OnSplitNew),
            AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
            wxT("Ctrl+Alt+I") ),

         Separator(),

         /* i18n-hint: (verb)*/
         Command( wxT("Join"), XXO("&Join"), FN(OnJoin),
            NotBusyTimeAndTracksFlags, wxT("Ctrl+J") ),
         Command( wxT("Disjoin"), XXO("Detac&h at Silences"), FN(OnDisjoin),
            NotBusyTimeAndTracksFlags, wxT("Ctrl+Alt+J") )
      ),

      //////////////////////////////////////////////////////////////////////////

      LabelEditMenus,

      Command( wxT("EditMetaData"), XXO("&Metadata..."), FN(OnEditMetadata),
         AudioIONotBusyFlag ),

      //////////////////////////////////////////////////////////////////////////

#ifndef __WXMAC__
      Separator(),
#endif

      Command( wxT("Preferences"), XXO("Pre&ferences..."), FN(OnPreferences),
         AudioIONotBusyFlag, prefKey )
   );
}

MenuTable::BaseItemPtr ExtraEditMenu( AudacityProject & )
{
   using namespace MenuTable;
   using Options = CommandManager::Options;
   constexpr auto flags =
      AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag;
   return Menu( _("&Edit"),
      Command( wxT("DeleteKey"), XXO("&Delete Key"), FN(OnDelete),
         (flags | NoAutoSelect),
         Options{ wxT("Backspace") }.Mask( flags ) ),
      Command( wxT("DeleteKey2"), XXO("Delete Key&2"), FN(OnDelete),
         (flags | NoAutoSelect),
         Options{ wxT("Delete") }.Mask( flags ) )
   );
}

#undef XXO
#undef FN
