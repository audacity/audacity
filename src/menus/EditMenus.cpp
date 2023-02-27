
#include "../AdornedRulerPanel.h"
#include "../Clipboard.h"
#include "../CommonCommandFlags.h"
#include "LabelTrack.h"
#include "../Menus.h"
#include "../NoteTrack.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectRate.h"
#include "ProjectTimeSignature.h"
#include "../ProjectWindow.h"
#include "../ProjectWindows.h"
#include "../SelectUtilities.h"
#include "SyncLock.h"
#include "../TrackPanel.h"
#include "../TrackPanelAx.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "WaveTrackUtilities.h"
#include "WaveClip.h"
#include "SampleBlock.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "TimeWarper.h"
#include "../prefs/PrefsDialog.h"
#include "../prefs/TracksBehaviorsPrefs.h"
#include "../tracks/labeltrack/ui/LabelTrackView.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveChannelView.h"
#include "AudacityMessageBox.h"
#include "../widgets/VetoDialogHook.h"
#include "../AudioPasteDialog.h"
#include "BasicUI.h"
#include "Sequence.h"
#include "UserException.h"

// private helper classes and functions
namespace {

// Handle text paste. Return true if did paste.
// (This was formerly the first part of overly-long OnPaste.)
bool DoPasteText(AudacityProject &project)
{
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   // Paste into the active label (if any)
   for (auto pLabelTrack : tracks.Any<LabelTrack>()) {
      // Does this track have an active label?
      if (LabelTrackView::Get( *pLabelTrack ).GetTextEditIndex(project) != -1) {

         // Yes, so try pasting into it
         auto &view = LabelTrackView::Get( *pLabelTrack );
         if (view.PasteSelectedText( project, selectedRegion.t0(),
                                            selectedRegion.t1() ))
         {
            ProjectHistory::Get( project )
               .PushState(XO("Pasted text from the clipboard"), XO("Paste"));

            return true;
         }
      }
   }

   //Presumably, there might be not more than one track
   //that expects text input
   for (auto wt : tracks.Any<WaveTrack>()) {
      auto& view = WaveChannelView::Get(*wt);
      if (view.PasteText(project)) {
         auto &trackPanel = TrackPanel::Get(project);
         trackPanel.Refresh(false);
         return true;
      }
   }

   return false;
}

wxULongLong EstimateCopyBytesCount(const TrackList& src, const TrackList& dst)
{
   wxULongLong result{};
   for (auto waveTrack : src.Any<const WaveTrack>()) {
      const auto samplesCount = waveTrack->GetSequenceSamplesCount();
      result += samplesCount.as_long_long() *
         SAMPLE_SIZE(waveTrack->GetSampleFormat());
   }
   return result;
}

BlockArray::size_type EstimateCopiedBlocks(const TrackList& src, const TrackList& dst)
{
   BlockArray::size_type result{};
   for (const auto waveTrack : src.Any<const WaveTrack>())
      result += waveTrack->CountBlocks();
   return result;
}

std::shared_ptr<TrackList> DuplicateDiscardTrimmed(const TrackList& src) {
   auto result = TrackList::Create(nullptr);
   for (auto track : src) {
      const auto copies =
         track->Copy(track->GetStartTime(), track->GetEndTime(), false);
      const auto pTrack = *copies->begin();
      pTrack->MoveTo(track->GetStartTime());
      if (const auto waveTrack = dynamic_cast<WaveTrack*>(pTrack))
         waveTrack->DiscardTrimmed();
      result->Append(std::move(*copies));
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

   assert(tracks.Selected().empty());

   Track* pFirstNewTrack = NULL;
   for (auto pClip : src) {
      auto pNewTrack = pClip->PasteInto(project, tracks);
      if (!pFirstNewTrack)
         pFirstNewTrack = pNewTrack.get();
      pNewTrack->SetSelected(true);
   }

   // Select some pasted samples, which is probably impossible to get right
   // with various project and track sample rates.
   // So do it at the sample rate of the project
   const double projRate = ProjectRate::Get( project ).GetRate();
   const double projTempo = ProjectTimeSignature::Get(project).GetTempo();
   const double srcTempo =
      pFirstNewTrack ? pFirstNewTrack->GetProjectTempo().value_or(projTempo) :
                       projTempo;
   // Apply adequate stretching to the selection. A selection of 10 seconds of
   // audio in project A should become 5 seconds in project B if tempo in B is
   // twice as fast.
   const double quantT0 = QUANTIZED_TIME(t0 * srcTempo / projTempo, projRate);
   const double quantT1 = QUANTIZED_TIME(t1 * srcTempo / projTempo, projRate);
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
   const auto range = trackList.Any<const WaveTrack>();
   return std::any_of(range.begin(), range.end(),
      [](const WaveTrack *pTrack){ return pTrack->HasHiddenData(); });
}

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
      t = *tracks.begin();
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
      t = *tracks.begin();
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

   for (auto lt : tracks.Selected<LabelTrack>()) {
      auto &view = LabelTrackView::Get( *lt );
      if (view.CutSelectedText( context.project )) {
         trackPanel.Refresh(false);
         return;
      }
   }

   //Presumably, there might be not more than one track
   //that expects text input
   for (auto wt : tracks.Any<WaveTrack>()) {
      auto& view = WaveChannelView::Get(*wt);
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
      [&](NoteTrack &n) {
         // Since portsmf has a built-in cut operator, we use that instead
         auto dest = n.Cut(selectedRegion.t0(), selectedRegion.t1());
         newClipboard.Append(std::move(*dest));
      },
#endif
      [&](Track &n) {
         if (n.SupportsBasicEditing()) {
            auto dest = n.Copy(selectedRegion.t0(), selectedRegion.t1());
            newClipboard.Append(std::move(*dest));
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
      [](NoteTrack&) {
         //if NoteTrack, it was cut, so do not clear anything

         // PRL:  But what if it was sync lock selected only, not selected?
      },
#endif
      [&](auto &&fallthrough){ return [&](WaveTrack &wt) {
         if (gPrefs->Read(wxT("/GUI/EnableCutLines"), (long)0))
            wt.ClearAndAddCutLine(selectedRegion.t0(), selectedRegion.t1());
         else
            fallthrough();
      }; },
      [&](Track &n) {
         if (n.SupportsBasicEditing())
            n.Clear(selectedRegion.t0(), selectedRegion.t1());
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

   for (auto n : tracks) {
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

   for (auto lt : tracks.Selected<LabelTrack>()) {
      auto &view = LabelTrackView::Get( *lt );
      if (view.CopySelectedText( context.project )) {
         //trackPanel.Refresh(false);
         return;
      }
   }
   //Presumably, there might be not more than one track
   //that expects text input
   for (auto wt : tracks.Any<WaveTrack>()) {
      auto& view = WaveChannelView::Get(*wt);
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
         auto dest = n->Copy(selectedRegion.t0(), selectedRegion.t1());
         newClipboard.Append(std::move(*dest));
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

std::shared_ptr<const TrackList> FindSourceTracks(const CommandContext &context)
{
   auto &project = context.project;
   auto &window = ProjectWindow::Get(project);
   auto &tracks = TrackList::Get(project);
   const auto &clipboard = Clipboard::Get();
   auto discardTrimmed = false;
   if (&context.project != &*clipboard.Project().lock()) {
      const auto waveClipCopyPolicy = TracksBehaviorsAudioTrackPastePolicy.Read();
      if(waveClipCopyPolicy == wxT("Ask") && HasHiddenData(clipboard.GetTracks())) {
         AudioPasteDialog audioPasteDialog(
            &window,
            EstimateCopyBytesCount(clipboard.GetTracks(), tracks)
         );
         const auto result = audioPasteDialog.ShowModal();
         if(result == wxID_CANCEL)
            return {};
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

   return srcTracks;
}

auto NotificationScope(
   const CommandContext &context, const TrackList &srcTracks)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get(project);
   auto &trackFactory = WaveTrackFactory::Get(project);
   auto &pSampleBlockFactory = trackFactory.GetSampleBlockFactory();
   return pSampleBlockFactory->Subscribe([
      toCopy = EstimateCopiedBlocks(srcTracks, tracks),
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
}

//! Whether the source track may be pasted into the destination track
bool FitsInto(const Track &src, const Track &dst)
{
   if (!src.SameKindAs(dst))
      return false;
   // Mono can "fit" into stereo, by duplication of the channel
   // Otherwise non-wave tracks always have just one "channel"
   // Future:  Fit stereo into mono too, using mix-down
   return TrackList::NChannels(src) <= TrackList::NChannels(dst);
}

// First, destination track; second, source
using Correspondence = std::vector<std::pair<Track*, const Track*>>;

Correspondence FindCorrespondence(
   TrackList &dstTracks, const TrackList &srcTracks)
{
   Correspondence result;
   auto dstRange = dstTracks.Selected();
   if (dstRange.size() == 1)
      // Special rule when only one track is selected interprets the user's
      // intent as pasting into that track and following ones
      dstRange = dstTracks.Any().StartingWith(*dstRange.begin());
   auto srcRange = srcTracks.Any();
   while (!(dstRange.empty() || srcRange.empty())) {
      auto &dst = **dstRange.begin();
      auto &src = **srcRange.begin();
      if (!FitsInto(src, dst)) {
         // Skip selected track of inappropriate type and try again
         ++dstRange.first;
         continue;
      }
      result.emplace_back(&dst, &src);
      ++srcRange.first;
      ++dstRange.first;
   }

   if (!srcRange.empty())
      // Could not fit all source tracks into the selected tracks
      return {};
   else
      return result;
}

void OnPaste(const CommandContext &context)
{
   auto &project = context.project;

   // Handle text paste first.
   if (DoPasteText(project))
      return;

   const auto &clipboard = Clipboard::Get();
   if (clipboard.GetTracks().empty())
      return;

   const auto srcTracks = FindSourceTracks(context);
   if (!srcTracks)
      // user cancelled
      return;

   auto notificationScope = NotificationScope(context, *srcTracks);

   auto &tracks = TrackList::Get(project);
   // If nothing's selected, we just insert new tracks.
   if (!tracks.Selected()) {
      DoPasteNothingSelected(
         project, *srcTracks, clipboard.T0(), clipboard.T1());
      return;
   }

   // Otherwise, paste into the selected tracks.
   double t0, t1;
   std::tie(t0, t1) = FindSelection(context);
   auto newT1 = t0 + clipboard.Duration();
   const auto isSyncLocked = SyncLockState::Get(project).IsSyncLocked();

   Track *ff = nullptr;
   bool bPastedSomething = false;

   // Find tracks to paste in
   auto correspondence = FindCorrespondence(tracks, *srcTracks);
   if (correspondence.empty()) {
      if (tracks.Selected().size() == 1)
         AudacityMessageBox(XO(
"The content you are trying to paste will span across more tracks than you "
"currently have available. Add more tracks and try again.")
      );
      else
         AudacityMessageBox(XO(
"There are not enough tracks selected to accommodate your copied content. "
"Select additional tracks and try again.")
      );
      return;
   }
   auto iPair = correspondence.begin();
   const auto endPair = correspondence.cend();

   // Outer loop by sync-lock groups
   auto next = tracks.begin();
   for (auto range = tracks.Any(); !range.empty();
      // Skip to next sync lock group
     range.first = next
   ) {
      if (iPair == endPair)
         // Nothing more to paste
         break;
      auto group = SyncLock::Group(*range.first);
      next = tracks.Find(*group.rbegin());
      ++next;

      if (!group.contains(iPair->first))
         // Nothing to paste into this group
         continue;

      // Inner loop over the group by tracks (not channels)
      for (auto leader : group) {
         if (iPair == endPair || leader != iPair->first) {
            if (isSyncLocked) {
               // Track is not pasted into but must be adjusted
               if (t1 != newT1 && t1 <= leader->GetEndTime()) {
                  leader->SyncLockAdjust(t1, newT1);
                  bPastedSomething = true;
               }
            }
         }
         else {
            // Remember first pasted-into track, to focus it
            if (!ff)
               ff = leader;
            // Do the pasting!
            const auto src = (iPair++)->second;
            leader->TypeSwitch(
               [&](WaveTrack &wn){
                  bPastedSomething = true;
                  // For correct remapping of preserved split lines:
                  PasteTimeWarper warper{ t1, t0 + src->GetEndTime() };
                  // New desired behaviour as of 3.4: pasting should result in a
                  // new clip - don't erase boundaries to surrounding clips ...
                  constexpr auto merge = false;
                  // ... and of course, don't preserve the boundaries strictly
                  // in [t0, t1].
                  constexpr auto preserveExistingBoundaries = false;
                  // Data in `[t0, t1]` must be recoverable though trimming.
                  constexpr auto clearByTrimming = true;
                  wn.ClearAndPaste(
                     t0, t1, *static_cast<const WaveTrack*>(src),
                     preserveExistingBoundaries, merge, &warper,
                     clearByTrimming);
               },
               [&](LabelTrack &ln){
                  // Per Bug 293, users expect labels to move on a paste into
                  // a label track.
                  ln.Clear(t0, t1);

                  ln.ShiftLabelsOnInsert( clipboard.Duration(), t0 );

                  bPastedSomething |= ln.PasteOver(t0, *src);
               },
               [&](Track &t){
                  bPastedSomething = true;
                  t.Clear(t0, t1);
                  t.Paste(t0, *src);
               }
            );
         }
      }
   }

   // TODO: What if we clicked past the end of the track?

   if (bPastedSomething) {
      ViewInfo::Get(project).selectedRegion
         .setTimes( t0, t0 + clipboard.Duration() );

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
   auto &tracks = TrackList::Get(project);
   auto &selectedRegion = ViewInfo::Get(project).selectedRegion;
   auto &window = ProjectWindow::Get(project);

   // This iteration is unusual because we add to the list inside the loop
   auto range = tracks.Selected();
   auto last = *range.rbegin();
   for (auto n : range) {
      if (!n->SupportsBasicEditing())
         continue;

      // Make copies not for clipboard but for direct addition to the project
      auto dest = n->Copy(selectedRegion.t0(), selectedRegion.t1(), false);
      (*dest->begin())
         ->MoveTo(std::max(selectedRegion.t0(), n->GetStartTime()));
      tracks.Append(std::move(*dest));

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
   auto &tracks = TrackList::Get(project);
   auto &selectedRegion = ViewInfo::Get(project).selectedRegion;
   auto &window = ProjectWindow::Get(project);

   auto &clipboard = Clipboard::Get();
   clipboard.Clear();

   auto pNewClipboard = TrackList::Create(nullptr);
   auto &newClipboard = *pNewClipboard;

   tracks.Selected().Visit(
      [&](WaveTrack &n) {
         auto tracks = n.SplitCut(selectedRegion.t0(), selectedRegion.t1());
         newClipboard.Append(std::move(*tracks));
      },
      [&](Track &n) {
         if (n.SupportsBasicEditing()) {
            auto dest = n.Copy(selectedRegion.t0(), selectedRegion.t1());
            n.Silence(selectedRegion.t0(), selectedRegion.t1());
            newClipboard.Append(std::move(*dest));
         }
      }
   );

   // Survived possibility of exceptions.  Commit changes to the clipboard now.
   clipboard.Assign(std::move(newClipboard),
      selectedRegion.t0(), selectedRegion.t1(), project.shared_from_this());

   ProjectHistory::Get(project)
      .PushState(XO("Split-cut to the clipboard"), XO("Split Cut"));
}

void OnSplitDelete(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get(project);
   auto &selectedRegion = ViewInfo::Get(project).selectedRegion;
   auto &window = ProjectWindow::Get(project);

   tracks.Selected().Visit(
      [&](WaveTrack &wt) {
         wt.SplitDelete(selectedRegion.t0(), selectedRegion.t1());
      },
      [&](Track &n) {
         if (n.SupportsBasicEditing())
            n.Silence(selectedRegion.t0(), selectedRegion.t1());
      }
   );

   ProjectHistory::Get(project).PushState(
      XO("Split-deleted %.2f seconds at t=%.2f")
         .Format(selectedRegion.duration(), selectedRegion.t0()),
      XO("Split Delete"));
}

void OnSilence(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get(project);
   auto &selectedRegion = ViewInfo::Get(project).selectedRegion;

   const auto selectedWaveTracks = tracks.Selected<WaveTrack>();
   WaveTrackUtilities::WithStretchRenderingProgress(
      [&](const ProgressReporter& parent) {
         BasicUI::SplitProgress(
            selectedWaveTracks.begin(), selectedWaveTracks.end(),
            [&](WaveTrack* n, const ProgressReporter& child) {
               n->Silence(selectedRegion.t0(), selectedRegion.t1(), child);
            },
            parent);
      });

   ProjectHistory::Get(project).PushState(
      XO("Silenced selected tracks for %.2f seconds at %.2f")
         .Format(selectedRegion.duration(), selectedRegion.t0()),
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

   tracks.Selected().Visit( [&](WaveTrack &wt) {
      //Hide the section before the left selector
      wt.Trim(selectedRegion.t0(), selectedRegion.t1());
   } );

   ProjectHistory::Get( project ).PushState(
      XO("Trim selected audio tracks from %.2f seconds to %.2f seconds")
         .Format( selectedRegion.t0(), selectedRegion.t1() ),
      XO("Trim Audio"));
}

void OnSplit(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get(project);
   auto [sel0, sel1] = FindSelection(context);
   if (auto *pTrack = *tracks.Find(context.temporarySelection.pTrack)) {
      if (auto pWaveTrack = dynamic_cast<WaveTrack*>(pTrack))
         pWaveTrack->Split(sel0, sel1);
      else
         // Did nothing, don't push history
         return;
   }
   else
      for (auto wt : tracks.Selected<WaveTrack>())
         wt->Split(sel0, sel1);

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
         dest->MoveTo(std::max(sel0, n->GetOffset()));

         if (sel1 >= n->GetEndTime())
            n->Clear(sel0, sel1);
         else if (sel0 <= n->GetOffset()) {
            n->Clear(sel0, sel1);
            n->MoveTo(sel1);
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
   auto &tracks = TrackList::Get(project);
   auto &selectedRegion = ViewInfo::Get(project).selectedRegion;
   auto &window = ProjectWindow::Get(project);

   // This iteration is unusual because we add to the list inside the loop
   auto range = tracks.Selected();
   auto last = *range.rbegin();
   for (auto track : range) {
      track->TypeSwitch(
         [&](WaveTrack &wt) {
            // Clips must be aligned to sample positions or the NEW clip will
            // not fit in the gap where it came from
            const double newt0 = wt.SnapToSample(selectedRegion.t0());
            const double newt1 = wt.SnapToSample(selectedRegion.t1());
            // Fix issue 2846 by calling copy with forClipboard = false.
            // This avoids creating the blank placeholder clips
            const auto dest = wt.Copy(newt0, newt1, false);
            if (dest) {
               // The copy function normally puts the clip at time 0
               // This offset lines it up with the original track's timing
               (*dest->begin())->MoveTo(newt0);
               tracks.Append(std::move(*dest));
            }
            wt.SplitDelete(newt0, newt1);
         }
#if 0
         ,
         // LL:  For now, just skip all non-wave tracks since the other do not
         //      yet support proper splitting.
         [&](Track &n) {
            dest = n.Cut(viewInfo.selectedRegion.t0(),
                   viewInfo.selectedRegion.t1());
            if (dest) {
               dest->MoveTo(std::max(0, n.GetOffset()));
               tracks.Add(dest);
            }
         }
#endif
      );
      if (track == last)
         break;
   }

   ProjectHistory::Get(project)
      .PushState(XO("Split to new track"), XO("Split New"));
}

void OnJoin(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get(project);
   auto &selectedRegion = ViewInfo::Get(project).selectedRegion;
   auto &window = ProjectWindow::Get(project);
   const auto selectedTracks = tracks.Selected<WaveTrack>();
   WaveTrackUtilities::WithStretchRenderingProgress(
      [&](const ProgressReporter& reportProgress) {
         using namespace BasicUI;
         SplitProgress(
            selectedTracks.begin(), selectedTracks.end(),
            [&](WaveTrack* wt, const ProgressReporter& childProgress) {
               wt->Join(
                  selectedRegion.t0(), selectedRegion.t1(), childProgress);
            },
            reportProgress);
      });

   ProjectHistory::Get(project).PushState(
      XO("Joined %.2f seconds at t=%.2f")
         .Format(selectedRegion.duration(), selectedRegion.t0()),
      XO("Join"));
}

void OnDisjoin(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get(project);
   auto &selectedRegion = ViewInfo::Get(project).selectedRegion;
   auto &window = ProjectWindow::Get(project);

   for (auto wt : tracks.Selected<WaveTrack>())
      wt->Disjoin(selectedRegion.t0(), selectedRegion.t1());

   ProjectHistory::Get(project).PushState(
      XO("Detached %.2f seconds at t=%.2f")
         .Format(selectedRegion.duration(), selectedRegion.t0()),
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
      auto range = TrackList::Get(project).Any<const LabelTrack>()
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

const ReservedCommandFlag
&JoinClipsAvailableFlag() { static ReservedCommandFlag flag {
   [](const AudacityProject &project)
   {
      if(AudioIOBusyPred(project))
         return false;
      
      const auto &viewInfo = ViewInfo::Get(project);
      if(viewInfo.selectedRegion.isPoint())
         return false;

      const auto selectedTracks = TrackList::Get(project).Selected<const WaveTrack>();
      for(const auto track : selectedTracks)
      {
         const auto selectedClips =
            track->GetClipsIntersecting(viewInfo.selectedRegion.t0(), viewInfo.selectedRegion.t1());
         if(selectedClips.size() > 1)
            return true;
      }
      return false;
   },
   CommandFlagOptions{}.DisableDefaultMessage()
}; return flag; }

using namespace MenuTable;
auto EditMenu()
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

   static auto menu = std::shared_ptr{
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
            AudioIONotBusyFlag() | CutCopyAvailableFlag(),
            wxT("Ctrl+X") ),
         Command( wxT("Delete"), XXO("&Delete"), OnDelete,
            AudioIONotBusyFlag() | EditableTracksSelectedFlag() | TimeSelectedFlag() | NoAutoSelect(),
            wxT("Ctrl+K") ),
         /* i18n-hint: (verb)*/
         Command( wxT("Copy"), XXO("&Copy"), OnCopy,
            AudioIONotBusyFlag() | CutCopyAvailableFlag(), wxT("Ctrl+C") ),
         /* i18n-hint: (verb)*/
         Command( wxT("Paste"), XXO("&Paste"), OnPaste,
            AudioIONotBusyFlag() | ClipboardNotEmptyFlag(), wxT("Ctrl+V") ),
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
                  JoinClipsAvailableFlag(), wxT("Ctrl+J") ),
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

AttachedItem sAttachment1{ Indirect(EditMenu()) };

auto ExtraEditMenu()
{
   using Options = CommandManager::Options;
   static const auto flags =
      AudioIONotBusyFlag() | EditableTracksSelectedFlag() | TimeSelectedFlag();
   static auto menu = std::shared_ptr{
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
   []{ return TimeSelectedFlag() | WaveTracksSelectedFlag(); },
   canSelectAll,
   selectAll
}};

// Also enable select for the noise reduction case.
RegisteredMenuItemEnabler selectWaveTracks2{{
   []{ return WaveTracksExistFlag(); },
   []{ return NoiseReductionTimeSelectedFlag() | WaveTracksSelectedFlag(); },
   canSelectAll,
   selectAll
}};

AttachedItem sAttachment2{ Indirect(ExtraEditMenu()),
   wxT("Optional/Extra/Part1")
};

}
