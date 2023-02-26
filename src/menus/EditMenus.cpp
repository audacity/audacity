
#include "../AdornedRulerPanel.h"
#include "../Clipboard.h"
#include "../CommonCommandFlags.h"
#include "../LabelTrack.h"
#include "../Menus.h"
#include "../NoteTrack.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectRate.h"
#include "../ProjectWindow.h"
#include "../ProjectWindows.h"
#include "../SelectUtilities.h"
#include "SyncLock.h"
#include "../TrackPanel.h"
#include "../TrackPanelAx.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "WaveClip.h"
#include "SampleBlock.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "TimeWarper.h"
#include "../prefs/PrefsDialog.h"
#include "../prefs/TracksBehaviorsPrefs.h"
#include "../tracks/labeltrack/ui/LabelTrackView.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveTrackView.h"
#include "AudacityMessageBox.h"
#include "VetoDialogHook.h"
#include "../AudioPasteDialog.h"
#include "BasicUI.h"
#include "Sequence.h"

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

/*
Track copy helper function. 
When copying tracks we consider two cases when pasting:
1. There is no selection
2. Not empty region is selected
In the first case we copy all tracks from src (used in simplified paste method
DoPasteNothingSelected). When selection isn't empty `N = min(src.size(), dst.size())`
tracks are copied from src, plus the last track from src could be duplicated
`M = dst.size() - N` times more, if `M > 0` (corresponds to a paste logic after
`!tracks.Selected()` condition in `OnPaste`). In both cases `ForEachCopiedWaveTrack`
visits tracks that are to be copied according to behaviour described above.
 */
void ForEachCopiedWaveTrack(const TrackList& src,
                            const TrackList& dst,
                            const std::function<void(const WaveTrack& waveTrack)>& f)
{
   if(dst.Selected().empty())
   {
      for(auto waveTrack : src.Any<const WaveTrack>())
         f(*waveTrack);
   }
   else
   {
      const auto srcTrackRange = src.Any<const WaveTrack>();
      const auto dstTrackRange = dst.Any<const WaveTrack>();
      auto srcTrack = srcTrackRange.begin();
      auto dstTrack = dstTrackRange.begin();
      auto lastCopiedTrack = srcTrack;
      while(dstTrack != dstTrackRange.end() && srcTrack != srcTrackRange.end())
      {
         if(!(*dstTrack)->GetSelected())
         {
            ++dstTrack;
            continue;
         }
         
         auto srcChannelCount = TrackList::Channels(*srcTrack).size();
         auto dstChannelCount = TrackList::Channels(*dstTrack).size();
                  
         while(srcChannelCount > 0 && dstChannelCount > 0)
         {
            f(**srcTrack);
            
            lastCopiedTrack = srcTrack;
            ++srcTrack;
            ++dstTrack;
            --srcChannelCount;
            --dstChannelCount;
         }
         
         while(dstChannelCount > 0)
         {
            f(**lastCopiedTrack);
            ++dstTrack;
            --dstChannelCount;
         }
      }
      while(dstTrack != dstTrackRange.end())
      {
         if((*dstTrack)->GetSelected())
            f(**lastCopiedTrack);
         ++dstTrack;
      }
   }
}

wxULongLong EstimateCopyBytesCount(const TrackList& src, const TrackList& dst)
{
   wxULongLong result{};
   ForEachCopiedWaveTrack(src, dst, [&](const WaveTrack& waveTrack) {
      sampleCount samplesCount = 0;
      for(auto& clip : waveTrack.GetClips())
         samplesCount += clip->GetSequenceSamplesCount();
      result += samplesCount.as_long_long() * SAMPLE_SIZE(waveTrack.GetSampleFormat());
   });
   return result;
}

BlockArray::size_type EstimateCopiedBlocks(const TrackList& src, const TrackList& dst)
{
   BlockArray::size_type result{};
   ForEachCopiedWaveTrack(src, dst, [&](const WaveTrack& waveTrack) {
      for(auto& clip : waveTrack.GetClips())
         result += clip->GetSequenceBlockArray()->size();
   });
   return result;
}

std::shared_ptr<TrackList> DuplicateDiscardTrimmed(const TrackList& src) {
   auto result = TrackList::Create(nullptr);
   for(auto track : src)
   {
      auto trackCopy = track->Copy(track->GetStartTime(), track->GetEndTime(), false);
      trackCopy->Init(*track);
      trackCopy->SetOffset(track->GetOffset());
      
      if(auto waveTrack = dynamic_cast<WaveTrack*>(trackCopy.get()))
      {
         for(auto clip : waveTrack->GetClips())
         {
            if(clip->GetTrimLeft() != 0)
            {
               auto t0 = clip->GetPlayStartTime();
               clip->SetTrimLeft(0);
               clip->ClearLeft(t0);
            }
            if(clip->GetTrimRight() != 0)
            {
               auto t1 = clip->GetPlayEndTime();
               clip->SetTrimRight(0);
               clip->ClearRight(t1);
            }
         }
      }
      result->Add(trackCopy);
   }
   return result;
}

// Create and paste into NEW tracks.
// Simplified version of DoPaste, used when there is no selection
// on tracks
// (This was formerly the second part of overly-long OnPaste.)
void DoPasteNothingSelected(AudacityProject &project, const TrackList& src, double t0, double t1)
{
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &viewInfo = ViewInfo::Get( project );
   auto &window = ProjectWindow::Get( project );
   
   assert(!tracks.Selected());

   Track* pFirstNewTrack = NULL;
   for (auto pClip : src) {
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
   double projRate = ProjectRate::Get( project ).GetRate();
   double quantT0 = QUANTIZED_TIME(t0, projRate);
   double quantT1 = QUANTIZED_TIME(t1, projRate);
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
}

bool HasHiddenData(const TrackList& trackList)
{
   for(auto waveTrack : trackList.Any<const WaveTrack>())
   {
      for(auto& clip : waveTrack->GetClips())
      {
         if(clip->GetTrimLeft() != 0 || clip->GetTrimRight() != 0)
            return true;
      }
   }
   return false;
}

}

namespace {

// Menu handler functions

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
   TrackFocus::Get(project).Set(t);
   if (t) {
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
   TrackFocus::Get(project).Set(t);
   if (t) {
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

   //Presumably, there might be not more than one track
   //that expects text input
   for (auto wt : tracks.Any<WaveTrack>()) {
      auto& view = WaveTrackView::Get(*wt);
      if (view.CutSelectedText(context.project)) {
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

   (tracks.Any() + &SyncLock::IsSelectedOrSyncLockSelected).Visit(
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
      if (SyncLock::IsSelectedOrSyncLockSelected(n)) {
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
   //Presumably, there might be not more than one track
   //that expects text input
   for (auto wt : tracks.Any<WaveTrack>()) {
      auto& view = WaveTrackView::Get(*wt);
      if (view.CopySelectedText(context.project)) {
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

std::pair<double, double> FindSelection(const CommandContext &context)
{
   double sel0 = 0.0, sel1 = 0.0;
   
#if 0
   // Use the overriding selection if any was given in the context
   if (auto *pRegion = context.temporarySelection.pSelectedRegion) {
      auto &selectedRegion = *pRegion;
      sel0 = selectedRegion.t0();
      sel1 = selectedRegion.t1();
   }
   else
#endif
   {
      auto &selectedRegion = ViewInfo::Get(context.project).selectedRegion;
      sel0 = selectedRegion.t0();
      sel1 = selectedRegion.t1();
   }
   
   return { sel0, sel1 };
}

void OnPaste(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto& trackPanel = TrackPanel::Get(project);
   auto &trackFactory = WaveTrackFactory::Get( project );
   auto &pSampleBlockFactory = trackFactory.GetSampleBlockFactory();
   auto &window = ProjectWindow::Get( project );

   auto isSyncLocked = SyncLockState::Get(project).IsSyncLocked();

   // Handle text paste (into active label) first.
   if (DoPasteText(project))
      return;

   //Presumably, there might be not more than one track
   //that expects text input
   for (auto wt : tracks.Any<WaveTrack>()) {
      auto& view = WaveTrackView::Get(*wt);
      if (view.PasteText(context.project)) {
         trackPanel.Refresh(false);
         return;
      }
   }

   const auto &clipboard = Clipboard::Get();
   if (clipboard.GetTracks().empty())
      return;
   
   auto discardTrimmed = false;
   if(&context.project != &*clipboard.Project().lock())
   {
      const auto waveClipCopyPolicy = TracksBehaviorsAudioTrackPastePolicy.Read();
      if(waveClipCopyPolicy == wxT("Ask") && HasHiddenData(clipboard.GetTracks())) {
         AudioPasteDialog audioPasteDialog(
            &window,
            EstimateCopyBytesCount(clipboard.GetTracks(), tracks)
         );
         const auto result = audioPasteDialog.ShowModal();
         if(result == wxID_CANCEL)
            return;
         discardTrimmed =
            result == AudioPasteDialog::DISCARD;
      }
      else if(waveClipCopyPolicy == wxT("Discard"))
         discardTrimmed = true;
   }
   
   std::shared_ptr<const TrackList> srcTracks;
   if(discardTrimmed)
      srcTracks = DuplicateDiscardTrimmed(clipboard.GetTracks());
   else
      srcTracks = clipboard.GetTracks().shared_from_this();
   
   auto scopedSubscription = pSampleBlockFactory->Subscribe([
      toCopy = EstimateCopiedBlocks(*srcTracks, tracks),
      nCopied = 0,
      copyStartTime = std::chrono::system_clock::now(),
      progressDialog = std::shared_ptr<BasicUI::ProgressDialog>()]
      (const SampleBlockCreateMessage&) mutable {
         using namespace std::chrono;
         constexpr auto ProgressDialogShowDelay = milliseconds { 100 };
         ++nCopied;
         if(!progressDialog) {
            if(duration_cast<milliseconds>(system_clock::now() - copyStartTime) >= ProgressDialogShowDelay)
               progressDialog = BasicUI::MakeProgress(XO("Paste clip"), XO("Pasting clip contents, please wait"), 0);
         }
         else {
            progressDialog->Poll(nCopied, toCopy);
         }
   });
   
   // If nothing's selected, we just insert NEW tracks.
   if(!tracks.Selected())
   {
      DoPasteNothingSelected(project, *srcTracks, clipboard.T0(), clipboard.T1());
      return;
   }
   
   // Otherwise, paste into the selected tracks.
   double t0, t1;
   std::tie(t0, t1) = FindSelection(context);

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

   auto clipTrackRange = srcTracks->Any();
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
               if (SyncLock::IsSyncLockSelected(n)) {
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
      else if (SyncLock::IsSyncLockSelected(n))
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
         *srcTracks->Any< const WaveTrack >().rbegin();

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
            if (!SyncLock::IsSelectedOrSyncLockSelected(lt))
               return fallthrough();

            lt->Clear(t0, t1);

            // As above, only shift labels if sync-lock is on.
            if (isSyncLocked)
               lt->ShiftLabelsOnInsert(
                  clipboard.Duration(), t0);
         },
         [&](Track *n) {
            if (SyncLock::IsSyncLockSelected(n))
               n->SyncLockAdjust(t1, t0 + clipboard.Duration() );
         }
      );
   }

   // TODO: What if we clicked past the end of the track?

   if (bPastedSomething)
   {
      ViewInfo::Get(project).selectedRegion
         .setTimes( t0, t0 + clipboard.Duration() );

      ProjectHistory::Get( project )
         .PushState(XO("Pasted from the clipboard"), XO("Paste"));

      if (ff) {
         TrackFocus::Get(project).Set(ff);
         ff->EnsureVisible();
         ff->LinkConsistencyFix();
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
         //Hide the section before the left selector
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

   auto [sel0, sel1] = FindSelection(context);
   
   if (auto *pTrack = context.temporarySelection.pTrack) {
      if (auto pWaveTrack = dynamic_cast<WaveTrack*>(pTrack))
         for (auto pChannel : TrackList::Channels(pWaveTrack))
            pChannel->Split( sel0, sel1 );
      else
         // Did nothing, don't push history
         return;
   }
   else {
      for (auto wt : tracks.Selected< WaveTrack >())
         wt->Split( sel0, sel1 );
   }

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
            double newt0 = wt->LongSamplesToTime(wt->TimeToLongSamples(
               selectedRegion.t0()));
            double newt1 = wt->LongSamplesToTime(wt->TimeToLongSamples(
               selectedRegion.t1()));
            // Fix issue 2846 by calling copy with forClipboard = false.
            // This avoids creating the blank placeholder clips
            dest = wt->Copy(newt0, newt1, false);
            wt->SplitDelete(newt0, newt1);
            if (dest) {
               // The copy function normally puts the clip at time 0
               // This offset lines it up with the original track's timing
               dest->Offset(newt0);
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

void OnPreferences(const CommandContext &context)
{
   auto &project = context.project;

   GlobalPrefsDialog dialog(&GetProjectFrame( project ) /* parent */, &project );

   if( VetoDialogHook::Call( &dialog ) )
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

// Menu definitions

const ReservedCommandFlag
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
   Menu( wxT("Edit"), XXO("&Edit"),
      Section( "UndoRedo",
         Command( wxT("Undo"), XXO("&Undo"), OnUndo,
            AudioIONotBusyFlag() | UndoAvailableFlag(), wxT("Ctrl+Z") ),

         Command( wxT("Redo"), XXO("&Redo"), OnRedo,
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
         Command( wxT("Cut"), XXO("Cu&t"), OnCut,
            AudioIONotBusyFlag() | CutCopyAvailableFlag() | NoAutoSelect(),
            wxT("Ctrl+X") ),
         Command( wxT("Delete"), XXO("&Delete"), OnDelete,
            AudioIONotBusyFlag() | EditableTracksSelectedFlag() | TimeSelectedFlag() | NoAutoSelect(),
            wxT("Ctrl+K") ),
         /* i18n-hint: (verb)*/
         Command( wxT("Copy"), XXO("&Copy"), OnCopy,
            AudioIONotBusyFlag() | CutCopyAvailableFlag(), wxT("Ctrl+C") ),
         /* i18n-hint: (verb)*/
         Command( wxT("Paste"), XXO("&Paste"), OnPaste,
            AudioIONotBusyFlag(), wxT("Ctrl+V") ),
         /* i18n-hint: (verb)*/
         Command( wxT("Duplicate"), XXO("Duplic&ate"), OnDuplicate,
            NotBusyTimeAndTracksFlags, wxT("Ctrl+D") ),

         Section( "",
            Menu( wxT("RemoveSpecial"), XXO("R&emove Special"),
               Section( "",
                  /* i18n-hint: (verb) Do a special kind of cut*/
                  Command( wxT("SplitCut"), XXO("Spl&it Cut"), OnSplitCut,
                     NotBusyTimeAndTracksFlags,
                     Options{ wxT("Ctrl+Alt+X") } ),
                  /* i18n-hint: (verb) Do a special kind of DELETE*/
                  Command( wxT("SplitDelete"), XXO("Split D&elete"), OnSplitDelete,
                     NotBusyTimeAndTracksFlags,
                     Options{ wxT("Ctrl+Alt+K") } )
               ),

               Section( "",
                  /* i18n-hint: (verb)*/
                  Command( wxT("Silence"), XXO("Silence Audi&o"), OnSilence,
                     AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
                     wxT("Ctrl+L") ),
                  /* i18n-hint: (verb)*/
                  Command( wxT("Trim"), XXO("Tri&m Audio"), OnTrim,
                     AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
                     Options{ wxT("Ctrl+T") } )
               )
            )
         )
      ),
        

      Section( "Other",
      //////////////////////////////////////////////////////////////////////////

         Menu( wxT("Clip"), XXO("Audi&o Clips"),
            Section( "",
               /* i18n-hint: (verb) It's an item on a menu. */
               Command( wxT("Split"), XXO("Sp&lit"), OnSplit,
                  AudioIONotBusyFlag() | WaveTracksSelectedFlag(),
                  Options{ wxT("Ctrl+I") } ),
               Command( wxT("SplitNew"), XXO("Split Ne&w"), OnSplitNew,
                  AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
                  Options{ wxT("Ctrl+Alt+I") } )
            ),

            Section( "",
               /* i18n-hint: (verb)*/
               Command( wxT("Join"), XXO("&Join"), OnJoin,
                  NotBusyTimeAndTracksFlags, wxT("Ctrl+J") ),
               Command( wxT("Disjoin"), XXO("Detac&h at Silences"), OnDisjoin,
                  NotBusyTimeAndTracksFlags, wxT("Ctrl+Alt+J") )
            )
         )

      ),

      // Note that on Mac, the Preferences menu item is specially handled in
      // CommandManager (assigned a special wxWidgets id) so that it does
      // not appear in the Edit menu but instead under Audacity, consistent with
      // MacOS conventions.
      Section( "Preferences",
         Command( wxT("Preferences"), XXO("Pre&ferences"), OnPreferences,
            AudioIONotBusyFlag(), prefKey )
      )

   ) };
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
   Menu( wxT("Edit"), XXO("&Edit"),
      Command( wxT("DeleteKey"), XXO("&Delete Key"), OnDelete,
         (flags | NoAutoSelect()),
         wxT("Backspace") ),
      Command( wxT("DeleteKey2"), XXO("Delete Key&2"), OnDelete,
         (flags | NoAutoSelect()),
         wxT("Delete") )
   ) };
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
