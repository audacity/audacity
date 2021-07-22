
#include "../AdornedRulerPanel.h"
#include "../Clipboard.h"
#include "../CommonCommandFlags.h"
#include "../LabelTrack.h"
#include "../Menus.h"
#include "../NoteTrack.h"
#include "Prefs.h"
#include "../Project.h"
#include "../ProjectHistory.h"
#include "../ProjectSettings.h"
#include "../ProjectWindow.h"
#include "../SelectUtilities.h"
#include "../TrackPanel.h"
#include "../TrackPanelAx.h"
#include "../UndoManager.h"
#include "../ViewInfo.h"
#include "../WaveTrack.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../commands/ScreenshotCommand.h"
#include "../effects/TimeWarper.h"
#include "../export/Export.h"
#include "../prefs/PrefsDialog.h"
#include "../tracks/labeltrack/ui/LabelTrackView.h"
#include "../widgets/AudacityMessageBox.h"

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
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   for (auto pLabelTrack : tracks.Any<LabelTrack>())
   {
      // Does this track have an active label?
      if (LabelTrackView::Get( *pLabelTrack ).GetTextEditIndex(project) != -1) {

         // Yes, so try pasting into it
         auto &view = LabelTrackView::Get( *pLabelTrack );
         if (view.PasteSelectedText( project, selectedRegion.t0(),
                                            selectedRegion.t1() ))
         {
            ProjectHistory::Get( project )
               .PushState(XO("Pasted text from the clipboard"), XO("Paste"));

            // Make sure caret is in view
            int x;
            if (view.CalcCursorX( project, &x )) {
               window.ScrollIntoView(x);
            }

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
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &viewInfo = ViewInfo::Get( project );
   auto &window = ProjectWindow::Get( project );

   // First check whether anything's selected.
   if (tracks.Selected())
      return false;
   else
   {
      const auto &clipboard = Clipboard::Get();
      auto clipTrackRange = clipboard.GetTracks().Any< const Track >();
      if (clipTrackRange.empty())
         return true; // nothing to paste

      Track* pFirstNewTrack = NULL;
      for (auto pClip : clipTrackRange) {
         auto pNewTrack = pClip->PasteInto( project );
         bool newTrack = (pNewTrack.use_count() == 1);
         wxASSERT(pClip);

         if (!pFirstNewTrack)
            pFirstNewTrack = pNewTrack.get();

         pNewTrack->SetSelected(true);
         if (newTrack)
            FinishCopy(pClip, pNewTrack, tracks);
         else
            Track::FinishCopy(pClip, pNewTrack.get());
      }

      // Select some pasted samples, which is probably impossible to get right
      // with various project and track sample rates.
      // So do it at the sample rate of the project
      double projRate = ProjectSettings::Get( project ).GetRate();
      double quantT0 = QUANTIZED_TIME(clipboard.T0(), projRate);
      double quantT1 = QUANTIZED_TIME(clipboard.T1(), projRate);
      selectedRegion.setTimes(
         0.0,   // anywhere else and this should be
                // half a sample earlier
         quantT1 - quantT0);

      ProjectHistory::Get( project )
         .PushState(XO("Pasted from the clipboard"), XO("Paste"));

      if (pFirstNewTrack) {
         TrackFocus::Get(project).Set(pFirstNewTrack);
         pFirstNewTrack->EnsureVisible();
      }

      return true;
   }
}

}

namespace EditActions {

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnUndo(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &undoManager = UndoManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   if (!ProjectHistory::Get( project ).UndoAvailable()) {
      AudacityMessageBox( XO("Nothing to undo") );
      return;
   }

   // can't undo while dragging
   if (trackPanel.IsMouseCaptured()) {
      return;
   }

   undoManager.Undo(
      [&]( const UndoStackElem &elem ){
         ProjectHistory::Get( project ).PopState( elem.state ); } );

   auto t = *tracks.Selected().begin();
   if (!t)
      t = *tracks.Any().begin();
   if (t) {
      TrackFocus::Get(project).Set(t);
      t->EnsureVisible();
   }
}

void OnRedo(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &undoManager = UndoManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   if (!ProjectHistory::Get( project ).RedoAvailable()) {
      AudacityMessageBox( XO("Nothing to redo") );
      return;
   }
   // Can't redo whilst dragging
   if (trackPanel.IsMouseCaptured()) {
      return;
   }

   undoManager.Redo(
      [&]( const UndoStackElem &elem ){
         ProjectHistory::Get( project ).PopState( elem.state ); } );

   auto t = *tracks.Selected().begin();
   if (!t)
      t = *tracks.Any().begin();
   if (t) {
      TrackFocus::Get(project).Set(t);
      t->EnsureVisible();
   }
}

void OnCut(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &ruler = AdornedRulerPanel::Get( project );
   auto &window = ProjectWindow::Get( project );

   // This doesn't handle cutting labels, it handles
   // cutting the _text_ inside of labels, i.e. if you're
   // in the middle of editing the label text and select "Cut".

   for (auto lt : tracks.Selected< LabelTrack >()) {
      auto &view = LabelTrackView::Get( *lt );
      if (view.CutSelectedText( context.project )) {
         trackPanel.Refresh(false);
         return;
      }
   }

   auto &clipboard = Clipboard::Get();
   clipboard.Clear();

   auto pNewClipboard = TrackList::Create( nullptr );
   auto &newClipboard = *pNewClipboard;

   tracks.Selected().Visit(
#if defined(USE_MIDI)
      [&](NoteTrack *n) {
         // Since portsmf has a built-in cut operator, we use that instead
         auto dest = n->Cut(selectedRegion.t0(),
                selectedRegion.t1());
         FinishCopy(n, dest, newClipboard);
      },
#endif
      [&](Track *n) {
         if (n->SupportsBasicEditing()) {
            auto dest = n->Copy(selectedRegion.t0(),
                    selectedRegion.t1());
            FinishCopy(n, dest, newClipboard);
         }
      }
   );

   // Survived possibility of exceptions.  Commit changes to the clipboard now.
   clipboard.Assign(
       std::move( newClipboard ),
       selectedRegion.t0(),
       selectedRegion.t1(),
       project.shared_from_this()
   );

   // Proceed to change the project.  If this throws, the project will be
   // rolled back by the top level handler.

   (tracks.Any() + &Track::IsSelectedOrSyncLockSelected).Visit(
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
         if (n->SupportsBasicEditing())
            n->Clear(selectedRegion.t0(), selectedRegion.t1());
      }
   );

   selectedRegion.collapseToT0();

   ProjectHistory::Get( project )
      .PushState(XO("Cut to the clipboard"), XO("Cut"));

   // Bug 1663
   //mRuler->ClearPlayRegion();
   ruler.DrawOverlays( true );
}

void OnDelete(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   for (auto n : tracks.Any()) {
      if (!n->SupportsBasicEditing())
         continue;
      if (n->GetSelected() || n->IsSyncLockSelected()) {
         n->Clear(selectedRegion.t0(), selectedRegion.t1());
      }
   }

   double seconds = selectedRegion.duration();

   selectedRegion.collapseToT0();

   ProjectHistory::Get( project ).PushState(
      XO("Deleted %.2f seconds at t=%.2f")
         .Format( seconds, selectedRegion.t0()),
      XO("Delete"));
}


void OnCopy(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;

   for (auto lt : tracks.Selected< LabelTrack >()) {
      auto &view = LabelTrackView::Get( *lt );
      if (view.CopySelectedText( context.project )) {
         //trackPanel.Refresh(false);
         return;
      }
   }

   auto &clipboard = Clipboard::Get();
   clipboard.Clear();

   auto pNewClipboard = TrackList::Create( nullptr );
   auto &newClipboard = *pNewClipboard;

   for (auto n : tracks.Selected()) {
      if (n->SupportsBasicEditing()) {
         auto dest = n->Copy(selectedRegion.t0(),
                 selectedRegion.t1());
         FinishCopy(n, dest, newClipboard);
      }
   }

   // Survived possibility of exceptions.  Commit changes to the clipboard now.
   clipboard.Assign( std::move( newClipboard ),
      selectedRegion.t0(), selectedRegion.t1(), project.shared_from_this() );

   //Make sure the menus/toolbar states get updated
   trackPanel.Refresh(false);
}

void OnPaste(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &trackFactory = WaveTrackFactory::Get( project );
   auto &pSampleBlockFactory = trackFactory.GetSampleBlockFactory();
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   const auto &settings = ProjectSettings::Get( project );
   auto &window = ProjectWindow::Get( project );

   auto isSyncLocked = settings.IsSyncLocked();

   // Handle text paste (into active label) first.
   if (DoPasteText(project))
      return;

   // If nothing's selected, we just insert NEW tracks.
   if (DoPasteNothingSelected(project))
      return;

   const auto &clipboard = Clipboard::Get();
   auto clipTrackRange = clipboard.GetTracks().Any< const Track >();
   if (clipTrackRange.empty())
      return;

   // Otherwise, paste into the selected tracks.
   double t0 = selectedRegion.t0();
   double t1 = selectedRegion.t1();

   auto pN = tracks.Any().begin();

   Track *ff = NULL;
   const Track *lastClipBeforeMismatch = NULL;
   const Track *mismatchedClip = NULL;
   const Track *prevClip = NULL;

   bool bAdvanceClipboard = true;
   bool bPastedSomething = false;

   auto pasteWaveTrack = [&](WaveTrack *dst, const Track *src){
      bPastedSomething = true;
      // For correct remapping of preserved split lines:
      PasteTimeWarper warper{ t1, t0 + src->GetEndTime() };
      dst->ClearAndPaste(t0, t1, src, true, true, &warper);
   };

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
                  auto newT1 = t0 + clipboard.Duration();
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
               ExceptionType::BadUserAction,
               XO("Pasting one type of track into another is not allowed."),
               XO("Warning"), 
               "Error:_Copying_or_Pasting"
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
                  ExceptionType::BadUserAction,
                  XO("Copying stereo audio into a mono track is not allowed."),
                  XO("Warning"), 
                  "Error:_Copying_or_Pasting"
               };
            }
         }

         if (!ff)
            ff = n;

         wxASSERT( n && c && n->SameKindAs(*c) );
         n->TypeSwitch(
            [&](WaveTrack *wn){
               pasteWaveTrack(wn, static_cast<const WaveTrack *>(c));
            },
            [&](LabelTrack *ln){
               // Per Bug 293, users expect labels to move on a paste into
               // a label track.
               ln->Clear(t0, t1);

               ln->ShiftLabelsOnInsert( clipboard.Duration(), t0 );

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
                  pasteWaveTrack(wn, c);
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
         auto newT1 = t0 + clipboard.Duration();
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
         *clipboard.GetTracks().Any< const WaveTrack >().rbegin();

      tracks.Any().StartingWith(*pN).Visit(
         [&](WaveTrack *wt, const Track::Fallthrough &fallthrough) {
            if (!wt->GetSelected())
               return fallthrough();

            if (wc) {
               pasteWaveTrack(wt, wc);
            }
            else {
               auto tmp = wt->EmptyCopy( pSampleBlockFactory );
               tmp->InsertSilence( 0.0,
                  // MJS: Is this correct?
                  clipboard.Duration() );
               tmp->Flush();

               pasteWaveTrack(wt, tmp.get());
            }
         },
         [&](LabelTrack *lt, const Track::Fallthrough &fallthrough) {
            if (!lt->GetSelected() && !lt->IsSyncLockSelected())
               return fallthrough();

            lt->Clear(t0, t1);

            // As above, only shift labels if sync-lock is on.
            if (isSyncLocked)
               lt->ShiftLabelsOnInsert(
                  clipboard.Duration(), t0);
         },
         [&](Track *n) {
            if (n->IsSyncLockSelected())
               n->SyncLockAdjust(t1, t0 + clipboard.Duration() );
         }
      );
   }

   // TODO: What if we clicked past the end of the track?

   if (bPastedSomething)
   {
      selectedRegion.setT1( t0 + clipboard.Duration() );

      ProjectHistory::Get( project )
         .PushState(XO("Pasted from the clipboard"), XO("Paste"));

      if (ff) {
         TrackFocus::Get(project).Set(ff);
         ff->EnsureVisible();
      }
   }
}

void OnDuplicate(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   // This iteration is unusual because we add to the list inside the loop
   auto range = tracks.Selected();
   auto last = *range.rbegin();
   for (auto n : range) {
      if (!n->SupportsBasicEditing())
         continue;

      // Make copies not for clipboard but for direct addition to the project
      auto dest = n->Copy(selectedRegion.t0(),
              selectedRegion.t1(), false);
      dest->Init(*n);
      dest->SetOffset(wxMax(selectedRegion.t0(), n->GetOffset()));
      tracks.Add( dest );

      // This break is really needed, else we loop infinitely
      if (n == last)
         break;
   }

   ProjectHistory::Get( project )
      .PushState(XO("Duplicated"), XO("Duplicate"));
}

void OnSplitCut(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   auto &clipboard = Clipboard::Get();
   clipboard.Clear();

   auto pNewClipboard = TrackList::Create( nullptr );
   auto &newClipboard = *pNewClipboard;

   Track::Holder dest;

   tracks.Selected().Visit(
      [&](WaveTrack *n) {
         dest = n->SplitCut(
            selectedRegion.t0(),
            selectedRegion.t1());
         if (dest)
            FinishCopy(n, dest, newClipboard);
      },
      [&](Track *n) {
         if (n->SupportsBasicEditing()) {
            dest = n->Copy(selectedRegion.t0(),
                    selectedRegion.t1());
            n->Silence(selectedRegion.t0(),
                       selectedRegion.t1());
            if (dest)
               FinishCopy(n, dest, newClipboard);
         }
      }
   );

   // Survived possibility of exceptions.  Commit changes to the clipboard now.
   clipboard.Assign( std::move( newClipboard ),
      selectedRegion.t0(), selectedRegion.t1(), project.shared_from_this() );

   ProjectHistory::Get( project )
      .PushState(XO("Split-cut to the clipboard"), XO("Split Cut"));
}

void OnSplitDelete(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   tracks.Selected().Visit(
      [&](WaveTrack *wt) {
         wt->SplitDelete(selectedRegion.t0(),
                         selectedRegion.t1());
      },
      [&](Track *n) {
         if (n->SupportsBasicEditing())
            n->Silence(selectedRegion.t0(),
                       selectedRegion.t1());
      }
   );

   ProjectHistory::Get( project ).PushState(
      XO("Split-deleted %.2f seconds at t=%.2f")
         .Format( selectedRegion.duration(), selectedRegion.t0() ),
      XO("Split Delete"));
}

void OnSilence(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;

   for ( auto n : tracks.Selected< WaveTrack >() )
      n->Silence(selectedRegion.t0(), selectedRegion.t1());

   ProjectHistory::Get( project ).PushState(
      XO("Silenced selected tracks for %.2f seconds at %.2f")
         .Format( selectedRegion.duration(), selectedRegion.t0() ),
      /* i18n-hint: verb */
      XC("Silence", "command"));
}

void OnTrim(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   if (selectedRegion.isPoint())
      return;

   tracks.Selected().Visit(
      [&](WaveTrack *wt) {
         //Delete the section before the left selector
         wt->Trim(selectedRegion.t0(),
            selectedRegion.t1());
      }
   );

   ProjectHistory::Get( project ).PushState(
      XO("Trim selected audio tracks from %.2f seconds to %.2f seconds")
         .Format( selectedRegion.t0(), selectedRegion.t1() ),
      XO("Trim Audio"));
}

void OnSplit(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;

   double sel0 = selectedRegion.t0();
   double sel1 = selectedRegion.t1();

   for (auto wt : tracks.Selected< WaveTrack >())
      wt->Split( sel0, sel1 );

   ProjectHistory::Get( project ).PushState(XO("Split"), XO("Split"));
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

   PushState(XO("Split"), XO("Split"));
   */
#endif
}

void OnSplitNew(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   Track::Holder dest;

   // This iteration is unusual because we add to the list inside the loop
   auto range = tracks.Selected();
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
               FinishCopy(wt, dest, tracks);
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

   ProjectHistory::Get( project )
      .PushState(XO("Split to new track"), XO("Split New"));
}

void OnJoin(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   for (auto wt : tracks.Selected< WaveTrack >())
      wt->Join(selectedRegion.t0(),
               selectedRegion.t1());

   ProjectHistory::Get( project ).PushState(
      XO("Joined %.2f seconds at t=%.2f")
         .Format( selectedRegion.duration(), selectedRegion.t0() ),
      XO("Join"));
}

void OnDisjoin(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   for (auto wt : tracks.Selected< WaveTrack >())
      wt->Disjoin(selectedRegion.t0(),
                  selectedRegion.t1());

   ProjectHistory::Get( project ).PushState(
      XO("Detached %.2f seconds at t=%.2f")
         .Format( selectedRegion.duration(), selectedRegion.t0() ),
      XO("Detach"));
}

void OnEditMetadata(const CommandContext &context)
{
   auto &project = context.project;
   (void)Exporter::DoEditMetadata( project,
      XO("Edit Metadata Tags"), XO("Metadata Tags"), true);
}

void OnPreferences(const CommandContext &context)
{
   auto &project = context.project;

   GlobalPrefsDialog dialog(&GetProjectFrame( project ) /* parent */, &project );

   if( ScreenshotCommand::MayCapture( &dialog ) )
      return;

   if (!dialog.ShowModal()) {
      // Canceled
      return;
   }

   // LL:  Moved from PrefsDialog since wxWidgets on OSX can't deal with
   //      rebuilding the menus while the PrefsDialog is still in the modal
   //      state.
   for (auto p : AllProjects{}) {
      MenuManager::Get(*p).RebuildMenuBar(*p);
// TODO: The comment below suggests this workaround is obsolete.
#if defined(__WXGTK__)
      // Workaround for:
      //
      //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
      //
      // This workaround should be removed when Audacity updates to wxWidgets
      // 3.x which has a fix.
      auto &window = GetProjectFrame( *p );
      wxRect r = window.GetRect();
      window.SetSize(wxSize(1,1));
      window.SetSize(r.GetSize());
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

#define FN(X) (& EditActions::Handler :: X)

static const ReservedCommandFlag
&CutCopyAvailableFlag() { static ReservedCommandFlag flag{
   [](const AudacityProject &project){
      auto range = TrackList::Get( project ).Any<const LabelTrack>()
         + [&](const LabelTrack *pTrack){
            return LabelTrackView::Get( *pTrack ).IsTextSelected(
               // unhappy const_cast because track focus might be set
               const_cast<AudacityProject&>(project)
            );
         };
      if ( !range.empty() )
         return true;

      if (
         TimeSelectedPred( project )
      &&
         EditableTracksSelectedPred( project )
      )
         return true;

      return false;
   },
   cutCopyOptions()
}; return flag; }

namespace {
using namespace MenuTable;
BaseItemSharedPtr EditMenu()
{
   using Options = CommandManager::Options;

   static const auto NotBusyTimeAndTracksFlags =
      AudioIONotBusyFlag() | TimeSelectedFlag() | EditableTracksSelectedFlag();

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

   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Edit"), XXO("&Edit"),
      Section( "UndoRedo",
         Command( wxT("Undo"), XXO("&Undo"), FN(OnUndo),
            AudioIONotBusyFlag() | UndoAvailableFlag(), wxT("Ctrl+Z") ),

         Command( wxT("Redo"), XXO("&Redo"), FN(OnRedo),
            AudioIONotBusyFlag() | RedoAvailableFlag(), redoKey ),
            
         Special( wxT("UndoItemsUpdateStep"),
         [](AudacityProject &project, wxMenu&) {
            // Change names in the CommandManager as a side-effect
            MenuManager::ModifyUndoMenuItems(project);
         })
      ),

      Section( "Basic",
         // Basic Edit commands
         /* i18n-hint: (verb)*/
         Command( wxT("Cut"), XXO("Cu&t"), FN(OnCut),
            AudioIONotBusyFlag() | CutCopyAvailableFlag() | NoAutoSelect(),
            wxT("Ctrl+X") ),
         Command( wxT("Delete"), XXO("&Delete"), FN(OnDelete),
            AudioIONotBusyFlag() | EditableTracksSelectedFlag() | TimeSelectedFlag() | NoAutoSelect(),
            wxT("Ctrl+K") ),
         /* i18n-hint: (verb)*/
         Command( wxT("Copy"), XXO("&Copy"), FN(OnCopy),
            AudioIONotBusyFlag() | CutCopyAvailableFlag(), wxT("Ctrl+C") ),
         /* i18n-hint: (verb)*/
         Command( wxT("Paste"), XXO("&Paste"), FN(OnPaste),
            AudioIONotBusyFlag(), wxT("Ctrl+V") ),
         /* i18n-hint: (verb)*/
         Command( wxT("Duplicate"), XXO("Duplic&ate"), FN(OnDuplicate),
            NotBusyTimeAndTracksFlags, wxT("Ctrl+D") ),

         Section( "",
            Menu( wxT("RemoveSpecial"), XXO("R&emove Special"),
               Section( "",
                  /* i18n-hint: (verb) Do a special kind of cut*/
                  Command( wxT("SplitCut"), XXO("Spl&it Cut"), FN(OnSplitCut),
                     NotBusyTimeAndTracksFlags,
                     Options{ wxT("Ctrl+Alt+X") } ),
                  /* i18n-hint: (verb) Do a special kind of DELETE*/
                  Command( wxT("SplitDelete"), XXO("Split D&elete"), FN(OnSplitDelete),
                     NotBusyTimeAndTracksFlags,
                     Options{ wxT("Ctrl+Alt+K") } )
               ),

               Section( "",
                  /* i18n-hint: (verb)*/
                  Command( wxT("Silence"), XXO("Silence Audi&o"), FN(OnSilence),
                     AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
                     wxT("Ctrl+L") ),
                  /* i18n-hint: (verb)*/
                  Command( wxT("Trim"), XXO("Tri&m Audio"), FN(OnTrim),
                     AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
                     Options{ wxT("Ctrl+T") } )
               )
            )
         )
      ),
        

      Section( "Other",
      //////////////////////////////////////////////////////////////////////////

         Menu( wxT("Clip"), XXO("Clip B&oundaries"),
            Section( "",
               /* i18n-hint: (verb) It's an item on a menu. */
               Command( wxT("Split"), XXO("Sp&lit"), FN(OnSplit),
                  AudioIONotBusyFlag() | WaveTracksSelectedFlag(),
                  Options{ wxT("Ctrl+I") } ),
               Command( wxT("SplitNew"), XXO("Split Ne&w"), FN(OnSplitNew),
                  AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
                  Options{ wxT("Ctrl+Alt+I") } )
            ),

            Section( "",
               /* i18n-hint: (verb)*/
               Command( wxT("Join"), XXO("&Join"), FN(OnJoin),
                  NotBusyTimeAndTracksFlags, wxT("Ctrl+J") ),
               Command( wxT("Disjoin"), XXO("Detac&h at Silences"), FN(OnDisjoin),
                  NotBusyTimeAndTracksFlags, wxT("Ctrl+Alt+J") )
            )
         ),

         //////////////////////////////////////////////////////////////////////////

         Command( wxT("EditMetaData"), XXO("&Metadata..."), FN(OnEditMetadata),
            AudioIONotBusyFlag() )

         //////////////////////////////////////////////////////////////////////////

      ),

      // Note that on Mac, the Preferences menu item is specially handled in
      // CommandManager (assigned a special wxWidgets id) so that it does
      // not appear in the Edit menu but instead under Audacity, consistent with
      // MacOS conventions.
      Section( "Preferences",
         Command( wxT("Preferences"), XXO("Pre&ferences..."), FN(OnPreferences),
            AudioIONotBusyFlag(), prefKey )
      )

   ) ) };
   return menu;
}

AttachedItem sAttachment1{
   wxT(""),
   Shared( EditMenu() )
};

BaseItemSharedPtr ExtraEditMenu()
{
   using Options = CommandManager::Options;
   static const auto flags =
      AudioIONotBusyFlag() | EditableTracksSelectedFlag() | TimeSelectedFlag();
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Edit"), XXO("&Edit"),
      Command( wxT("DeleteKey"), XXO("&Delete Key"), FN(OnDelete),
         (flags | NoAutoSelect()),
         wxT("Backspace") ),
      Command( wxT("DeleteKey2"), XXO("Delete Key&2"), FN(OnDelete),
         (flags | NoAutoSelect()),
         wxT("Delete") )
   ) ) };
   return menu;
}

auto canSelectAll = [](const AudacityProject &project){
   return MenuManager::Get( project ).mWhatIfNoSelection != 0; };
auto selectAll = []( AudacityProject &project, CommandFlag flagsRqd ){
   if ( MenuManager::Get( project ).mWhatIfNoSelection == 1 &&
      (flagsRqd & NoAutoSelect()).none() )
      SelectUtilities::DoSelectAllAudio(project);
};

RegisteredMenuItemEnabler selectTracks{{
   []{ return TracksExistFlag(); },
   []{ return EditableTracksSelectedFlag(); },
   canSelectAll,
   selectAll
}};

// Including time tracks.
RegisteredMenuItemEnabler selectAnyTracks{{
   []{ return TracksExistFlag(); },
   []{ return AnyTracksSelectedFlag(); },
   canSelectAll,
   selectAll
}};

RegisteredMenuItemEnabler selectWaveTracks{{
   []{ return WaveTracksExistFlag(); },
   []{ return TimeSelectedFlag() | WaveTracksSelectedFlag() | CutCopyAvailableFlag(); },
   canSelectAll,
   selectAll
}};

// Also enable select for the noise reduction case.
RegisteredMenuItemEnabler selectWaveTracks2{{
   []{ return WaveTracksExistFlag(); },
   []{ return NoiseReductionTimeSelectedFlag() | WaveTracksSelectedFlag() | CutCopyAvailableFlag(); },
   canSelectAll,
   selectAll
}};

AttachedItem sAttachment2{
   wxT("Optional/Extra/Part1"),
   Shared( ExtraEditMenu() )
};

}
#undef FN
