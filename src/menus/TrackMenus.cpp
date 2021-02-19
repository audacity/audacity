#include "../CommonCommandFlags.h"
#include "../LabelTrack.h"
#include "../Menus.h"
#include "MixAndRender.h"

#include "Prefs.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "ProjectRate.h"
#include "../ProjectSettings.h"
#include "PluginManager.h"
#include "ProjectStatus.h"
#include "../ProjectWindow.h"
#include "../SelectUtilities.h"
#include "ShuttleGui.h"
#include "../SyncLock.h"
#include "../TrackPanelAx.h"
#include "../TrackPanel.h"
#include "../TrackUtilities.h"
#include "UndoManager.h"
#include "WaveClip.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../effects/EffectManager.h"
#include "../effects/EffectUI.h"
#include "QualitySettings.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveTrackControls.h"
#include "../toolbars/ToolManager.h"
#include "../widgets/ASlider.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/ProgressDialog.h"

#include <wx/combobox.h>

#ifdef EXPERIMENTAL_SCOREALIGN
#include "../effects/ScoreAlignDialog.h"
#include "audioreader.h"
#include "scorealign.h"
#include "scorealign-glue.h"
#endif /* EXPERIMENTAL_SCOREALIGN */

// private helper classes and functions
namespace {

void DoMixAndRender
(AudacityProject &project, bool toNewTrack)
{
   auto &tracks = TrackList::Get( project );
   auto &trackFactory = WaveTrackFactory::Get( project );
   auto rate = ProjectRate::Get(project).GetRate();
   auto defaultFormat = QualitySettings::SampleFormatChoice();
   auto &trackPanel = TrackPanel::Get( project );
   auto &window = ProjectWindow::Get( project );

   auto trackRange = tracks.Selected< WaveTrack >();
   WaveTrack::Holder uNewLeft, uNewRight;
   ::MixAndRender(trackRange.Filter<const WaveTrack>(),
      Mixer::WarpOptions{ tracks },
      tracks.MakeUniqueTrackName(_("Mix")),
      &trackFactory, rate, defaultFormat, 0.0, 0.0, uNewLeft, uNewRight);

   if (uNewLeft) {
      // Remove originals, get stats on what tracks were mixed

      // But before removing, determine the first track after the removal
      auto last = *trackRange.rbegin();
      auto insertionPoint = * ++ tracks.Find( last );
      
      auto selectedCount = (trackRange + &Track::IsLeader).size();
      wxString firstName;
      int firstColour = -1;
      if (selectedCount > 0) {
         firstName = (*trackRange.begin())->GetName();
         firstColour = (*trackRange.begin())->GetWaveColorIndex();
      }
      if (!toNewTrack)  {
         // Beware iterator invalidation!
         for (auto &it = trackRange.first, &end = trackRange.second; it != end;)
            tracks.Remove( *it++ );
      }

      // Add NEW tracks

      auto pNewLeft = tracks.Add( uNewLeft );
      decltype(pNewLeft) pNewRight{};
      if (uNewRight)
      {
         pNewRight = tracks.Add(uNewRight);
         tracks.MakeMultiChannelTrack(*pNewLeft, 2, true);
      }

      // If we're just rendering (not mixing), keep the track name the same
      if (selectedCount==1) {
         pNewLeft->SetName(firstName);
         if (pNewRight) {
            pNewRight->SetName(firstName);
         }
      }

      // Bug 2218, remember more things...
      if (selectedCount>=1) {
         pNewLeft->SetWaveColorIndex(firstColour);
         pNewLeft->SetSelected(!toNewTrack);
         if (pNewRight) {
            pNewRight->SetWaveColorIndex(firstColour);
            pNewRight->SetSelected(!toNewTrack);
         }
      }

      // Permute the tracks as needed
      // The new track appears after the old tracks (or where the old tracks
      // had been) so that they are in the same sync-lock group
      if (insertionPoint)
      {
         std::vector<TrackNodePointer> arr;
         arr.reserve( tracks.size() );
         size_t begin = 0, ii = 0;
         for (auto iter = tracks.ListOfTracks::begin(),
              end = tracks.ListOfTracks::end(); iter != end; ++iter) {
            arr.push_back( {iter, &tracks} );
            if ( iter->get() == insertionPoint )
               begin = ii;
            ++ii;
         }
         auto mid = arr.end();
         std::advance( mid, -TrackList::Channels( pNewLeft ).size() );
         std::rotate( arr.begin() + begin, mid, arr.end() );
         tracks.Permute( arr );
      }

      // Smart history/undo message
      if (selectedCount==1) {
         auto msg = XO("Rendered all audio in track '%s'").Format( firstName );
         /* i18n-hint: Convert the audio into a more usable form, so apply
          * panning and amplification and write to some external file.*/
         ProjectHistory::Get( project ).PushState(msg, XO("Render"));
      }
      else {
         auto msg = (pNewRight
            ? XO("Mixed and rendered %d tracks into one new stereo track")
            : XO("Mixed and rendered %d tracks into one new mono track")
         )
            .Format( (int)selectedCount );
         ProjectHistory::Get( project ).PushState(msg, XO("Mix and Render"));
      }

      trackPanel.SetFocus();
      TrackFocus::Get( project ).Set( pNewLeft );
      pNewLeft->EnsureVisible();
   }
}

void DoPanTracks(AudacityProject &project, float PanValue)
{
   auto &tracks = TrackList::Get( project );
   auto &window = ProjectWindow::Get( project );

   // count selected wave tracks
   const auto range = tracks.Any< WaveTrack >();
   const auto selectedRange = range + &Track::IsSelected;
   auto count = selectedRange.size();

   // iter through them, all if none selected.
   for (auto left : count == 0 ? range : selectedRange )
      left->SetPan( PanValue );

   auto flags = UndoPush::NONE;
   ProjectHistory::Get( project )
      /*i18n-hint: One or more audio tracks have been panned*/
      .PushState(XO("Panned audio track(s)"), XO("Pan Track"), flags);
         flags = flags | UndoPush::CONSOLIDATE;
}

enum {
   kAlignStartZero = 0,
   kAlignStartSelStart,
   kAlignStartSelEnd,
   kAlignEndSelStart,
   kAlignEndSelEnd,
   // The next two are only in one subMenu, so more easily handled at the end.
   kAlignEndToEnd,
   kAlignTogether
};

static const std::vector< ComponentInterfaceSymbol >
&alignLabels() { static std::vector< ComponentInterfaceSymbol > symbols{
   { wxT("StartToZero"),     XXO("Start to &Zero") },
   { wxT("StartToSelStart"), XXO("Start to &Cursor/Selection Start") },
   { wxT("StartToSelEnd"),   XXO("Start to Selection &End") },
   { wxT("EndToSelStart"),   XXO("End to Cu&rsor/Selection Start") },
   { wxT("EndToSelEnd"),     XXO("End to Selection En&d") },
}; return symbols; }

const size_t kAlignLabelsCount(){ return alignLabels().size(); }

void DoAlign
(AudacityProject &project, int index, bool moveSel)
{
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &window = ProjectWindow::Get( project );

   TranslatableString action, shortAction;
   double delta = 0.0;
   double newPos = -1.0;

   auto channelRange = tracks.Selected< AudioTrack >();
   auto trackRange = tracks.SelectedLeaders< AudioTrack >();

   auto FindOffset = []( const Track *pTrack ) {
      return TrackList::Channels(pTrack).min( &Track::GetOffset ); };

   auto firstTrackOffset = [&]{ return FindOffset( *trackRange.begin() ); };
   auto minOffset = [&]{ return trackRange.min( FindOffset ); };
   auto avgOffset = [&]{
      return trackRange.sum( FindOffset ) /
                             std::max( size_t(1), trackRange.size() ); };

   auto maxEndOffset = [&]{
      return std::max(0.0, channelRange.max( &Track::GetEndTime ) ); };

   switch(index) {
   case kAlignStartZero:
      delta = -minOffset();
      action = moveSel
         /* i18n-hint: In this and similar messages describing editing actions,
            the starting or ending points of tracks are re-"aligned" to other
            times, and the time selection may be "moved" too.  The first
            noun -- "start" in this example -- is the object of a verb (not of
            an implied preposition "from"). */
         ? XO("Aligned/Moved start to zero")
         : XO("Aligned start to zero");
      shortAction = moveSel
         /* i18n-hint: This and similar messages give shorter descriptions of
            the aligning and moving editing actions */
         ? XO("Align/Move Start")
         : XO("Align Start");
      break;
   case kAlignStartSelStart:
      delta = selectedRegion.t0() - minOffset();
      action = moveSel
         ? XO("Aligned/Moved start to cursor/selection start")
         : XO("Aligned start to cursor/selection start");
      shortAction = moveSel
         ? XO("Align/Move Start")
         : XO("Align Start");
      break;
   case kAlignStartSelEnd:
      delta = selectedRegion.t1() - minOffset();
      action = moveSel
         ? XO("Aligned/Moved start to selection end")
         : XO("Aligned start to selection end");
      shortAction = moveSel
         ? XO("Align/Move Start")
         : XO("Align Start");
      break;
   case kAlignEndSelStart:
      delta = selectedRegion.t0() - maxEndOffset();
      action = moveSel
         ? XO("Aligned/Moved end to cursor/selection start")
         : XO("Aligned end to cursor/selection start");
      shortAction =
         moveSel
         ? XO("Align/Move End")
         : XO("Align End");
      break;
   case kAlignEndSelEnd:
      delta = selectedRegion.t1() - maxEndOffset();
      action = moveSel
         ? XO("Aligned/Moved end to selection end")
         : XO("Aligned end to selection end");
      shortAction =
         moveSel
         ? XO("Align/Move End")
         : XO("Align End");
      break;
   // index set in alignLabelsNoSync
   case kAlignEndToEnd:
      newPos = firstTrackOffset();
      action = moveSel
         ? XO("Aligned/Moved end to end")
         : XO("Aligned end to end");
      shortAction =
         moveSel
         ? XO("Align/Move End to End")
         : XO("Align End to End");
      break;
   case kAlignTogether:
      newPos = avgOffset();
      action = moveSel
         ? XO("Aligned/Moved together")
         : XO("Aligned together");
      shortAction =
         moveSel
         ? XO("Align/Move Together")
         : XO("Align Together");
   }

   if ((unsigned)index >= kAlignLabelsCount()) {
      // This is an alignLabelsNoSync command.
      for (auto t : tracks.SelectedLeaders< AudioTrack >()) {
         // This shifts different tracks in different ways, so no sync-lock
         // move.
         // Only align Wave and Note tracks end to end.
         auto channels = TrackList::Channels(t);

         auto trackStart = channels.min( &Track::GetStartTime );
         auto trackEnd = channels.max( &Track::GetEndTime );

         for (auto channel : channels)
            // Move the track
            channel->SetOffset(newPos + channel->GetStartTime() - trackStart);

         if (index == kAlignEndToEnd)
            newPos += (trackEnd - trackStart);
      }
      if (index == kAlignEndToEnd)
         window.DoZoomFit();
   }

   if (delta != 0.0) {
      // For a fixed-distance shift move sync-lock selected tracks also.
      for (auto t : tracks.Any()
           + &SyncLock::IsSelectedOrSyncLockSelected )
         t->SetOffset(t->GetOffset() + delta);
   }

   if (moveSel)
      selectedRegion.move(delta);

   ProjectHistory::Get( project ).PushState(action, shortAction);
}

#ifdef EXPERIMENTAL_SCOREALIGN

#ifdef USE_MIDI
static const ReservedCommandFlag&
   NoteTracksSelectedFlag() { static ReservedCommandFlag flag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Selected<const NoteTrack>().empty();
      }
   }; return flag; }  //gsw
#endif

// rough relative amount of time to compute one
//    frame of audio or midi, or one cell of matrix, or one iteration
//    of smoothing, measured on a 1.9GHz Core 2 Duo in 32-bit mode
//    (see COLLECT_TIMING_DATA below)
#define AUDIO_WORK_UNIT 0.004F
#define MIDI_WORK_UNIT 0.0001F
#define MATRIX_WORK_UNIT 0.000002F
#define SMOOTHING_WORK_UNIT 0.000001F

// Write timing data to a file; useful for calibrating AUDIO_WORK_UNIT,
// MIDI_WORK_UNIT, MATRIX_WORK_UNIT, and SMOOTHING_WORK_UNIT coefficients
// Data is written to timing-data.txt; look in
//     audacity-src/win/Release/modules/
#define COLLECT_TIMING_DATA

// Audacity Score Align Progress class -- progress reports come here
class ASAProgress final : public SAProgress {
 private:
   float mTotalWork;
   float mFrames[2];
   long mTotalCells; // how many matrix cells?
   long mCellCount; // how many cells so far?
   long mPrevCellCount; // cell_count last reported with Update()
   std::optional<ProgressDialog> mProgress;
   #ifdef COLLECT_TIMING_DATA
      FILE *mTimeFile;
      wxDateTime mStartTime;
      long iterations;
   #endif

 public:
   ASAProgress() {
      smoothing = false;
      #ifdef COLLECT_TIMING_DATA
         mTimeFile = fopen("timing-data.txt", "w");
      #endif
   }
   ~ASAProgress() {
      #ifdef COLLECT_TIMING_DATA
         fclose(mTimeFile);
      #endif
   }
   void set_phase(int i) override {
      float work[2]; // chromagram computation work estimates
      float work2, work3 = 0; // matrix and smoothing work estimates
      SAProgress::set_phase(i);
      #ifdef COLLECT_TIMING_DATA
         long ms = 0;
         wxDateTime now = wxDateTime::UNow();
         wxFprintf(mTimeFile, "Phase %d begins at %s\n",
                 i, now.FormatTime());
         if (i != 0)
            ms = now.Subtract(mStartTime).GetMilliseconds().ToLong();
         mStartTime = now;
      #endif
      if (i == 0) {
         mCellCount = 0;
         for (int j = 0; j < 2; j++) {
            mFrames[j] = durations[j] / frame_period;
         }
         mTotalWork = 0;
         for (int j = 0; j < 2; j++) {
             work[j] =
                (is_audio[j] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[j];
             mTotalWork += work[j];
         }
         mTotalCells = mFrames[0] * mFrames[1];
         work2 = mTotalCells * MATRIX_WORK_UNIT;
         mTotalWork += work2;
         // arbitrarily assume 60 iterations to fit smooth segments and
         // per frame per iteration is SMOOTHING_WORK_UNIT
         if (smoothing) {
            work3 =
               wxMax(mFrames[0], mFrames[1]) * SMOOTHING_WORK_UNIT * 40;
            mTotalWork += work3;
         }
         #ifdef COLLECT_TIMING_DATA
            wxFprintf(mTimeFile,
               " mTotalWork (an estimate) = %g\n", mTotalWork);
            wxFprintf(mTimeFile, " work0 = %g, frames %g, is_audio %d\n",
                    work[0], mFrames[0], is_audio[0]);
            wxFprintf(mTimeFile, " work1 = %g, frames %g, is_audio %d\n",
                    work[1], mFrames[1], is_audio[1]);
            wxFprintf(mTimeFile, "work2 = %g, work3 = %g\n", work2, work3);
         #endif
         mProgress.emplace(XO("Synchronize MIDI with Audio"),
                               XO("Synchronizing MIDI and Audio Tracks"));
      } else if (i < 3) {
         wxFprintf(mTimeFile,
                "Phase %d took %d ms for %g frames, coefficient = %g s/frame\n",
               i - 1, ms, mFrames[i - 1], (ms * 0.001) / mFrames[i - 1]);
      } else if (i == 3) {
        wxFprintf(mTimeFile,
                "Phase 2 took %d ms for %d cells, coefficient = %g s/cell\n",
                ms, mCellCount, (ms * 0.001) / mCellCount);
      } else if (i == 4) {
        wxFprintf(mTimeFile,
                "Phase 3 took %d ms for %d iterations on %g frames, "
                "coefficient = %g s per frame per iteration\n",
                ms, iterations, wxMax(mFrames[0], mFrames[1]),
                (ms * 0.001) / (wxMax(mFrames[0], mFrames[1]) * iterations));
      }
   }
   bool set_feature_progress(float s) override {
      float work;
      if (phase == 0) {
         float f = s / frame_period;
         work = (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * f;
      } else if (phase == 1) {
         float f = s / frame_period;
         work = (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
                (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * f;
      }
      auto updateResult = mProgress->Update((int)(work), (int)(mTotalWork));
      return (updateResult == ProgressResult::Success);
   }
   bool set_matrix_progress(int cells) override {
      mCellCount += cells;
      float work =
             (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
             (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[1];
      work += mCellCount * MATRIX_WORK_UNIT;
      auto updateResult = mProgress->Update((int)(work), (int)(mTotalWork));
      return (updateResult == ProgressResult::Success);
   }
   bool set_smoothing_progress(int i) override {
      iterations = i;
      float work =
             (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
             (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[1] +
             MATRIX_WORK_UNIT * mFrames[0] * mFrames[1];
      work += i * wxMax(mFrames[0], mFrames[1]) * SMOOTHING_WORK_UNIT;
      auto updateResult = mProgress->Update((int)(work), (int)(mTotalWork));
      return (updateResult == ProgressResult::Success);
   }
};


long mixer_process(void *mixer, float **buffer, long n)
{
   Mixer *mix = (Mixer *) mixer;
   long frame_count = mix->Process(std::max(0L, n));
   *buffer = (float *) mix->GetBuffer();
   return frame_count;
}

#endif // EXPERIMENTAL_SCOREALIGN

enum{
   kAudacitySortByTime = (1 << 1),
   kAudacitySortByName = (1 << 2),
};

void DoSortTracks( AudacityProject &project, int flags )
{
   auto GetTime = [](const Track *t) {
      return t->TypeSwitch< double >(
         [&](const WaveTrack* w) {
            auto stime = w->GetEndTime();

            int ndx;
            for (ndx = 0; ndx < w->GetNumClips(); ndx++) {
               const auto c = w->GetClipByIndex(ndx);
               if (c->GetPlaySamplesCount() == 0)
                  continue;
               stime = std::min(stime, c->GetPlayStartTime());
            }
            return stime;
         },
         [&](const LabelTrack* l) {
            return l->GetStartTime();
         }
      );
   };

   size_t ndx = 0;
   // This one place outside of TrackList where we must use undisguised
   // std::list iterators!  Avoid this elsewhere!
   std::vector<TrackNodePointer> arr;
   auto &tracks = TrackList::Get( project );
   arr.reserve(tracks.size());

   // First find the permutation.
   // This routine, very unusually, deals with the underlying stl list
   // iterators, not with TrackIter!  Dangerous!
   for (auto iter = tracks.ListOfTracks::begin(),
        end = tracks.ListOfTracks::end(); iter != end; ++iter) {
      const auto &track = *iter;
      if ( !track->IsLeader() )
         // keep channels contiguous
         ndx++;
      else {
         auto size = arr.size();
         for (ndx = 0; ndx < size;) {
            Track &arrTrack = **arr[ndx].first;
            auto channels = TrackList::Channels(&arrTrack);
            if(flags & kAudacitySortByName) {
               //do case insensitive sort - cmpNoCase returns less than zero if
               // the string is 'less than' its argument
               //also if we have case insensitive equality, then we need to sort
               // by case as well
               //We sort 'b' before 'B' accordingly.  We uncharacteristically
               // use greater than for the case sensitive
               //compare because 'b' is greater than 'B' in ascii.
               auto cmpValue = track->GetName().CmpNoCase(arrTrack.GetName());
               if ( cmpValue < 0 ||
                     ( 0 == cmpValue &&
                        track->GetName().CompareTo(arrTrack.GetName()) > 0 ) )
                  break;
            }
            //sort by time otherwise
            else if(flags & kAudacitySortByTime) {
               auto time1 = TrackList::Channels(track.get()).min( GetTime );

               //get candidate's (from sorted array) time
               auto time2 = channels.min( GetTime );

               if (time1 < time2)
                  break;
            }
            ndx += channels.size();
         }
      }
      arr.insert(arr.begin() + ndx, TrackNodePointer{iter, &tracks});
   }

   // Now apply the permutation
   tracks.Permute(arr);
}

void SetTrackGain(AudacityProject &project, WaveTrack * wt, LWSlider * slider)
{
   wxASSERT(wt);
   float newValue = slider->Get();

   for (auto channel : TrackList::Channels(wt))
      channel->SetGain(newValue);

   ProjectHistory::Get( project )
      .PushState(XO("Adjusted gain"), XO("Gain"), UndoPush::CONSOLIDATE);

   TrackPanel::Get( project ).RefreshTrack(wt);
}

void SetTrackPan(AudacityProject &project, WaveTrack * wt, LWSlider * slider)
{
   wxASSERT(wt);
   float newValue = slider->Get();

   for (auto channel : TrackList::Channels(wt))
      channel->SetPan(newValue);

   ProjectHistory::Get( project )
      .PushState(XO("Adjusted Pan"), XO("Pan"), UndoPush::CONSOLIDATE);

   TrackPanel::Get( project ).RefreshTrack(wt);
}

}

namespace {

// Menu handler functions

void OnStereoToMono(const CommandContext &context)
{
   EffectUI::DoEffect(
      EffectManager::Get().GetEffectByIdentifier(wxT("StereoToMono")),
      context,
      EffectManager::kConfigured);
}

void OnMixAndRender(const CommandContext &context)
{
   auto &project = context.project;
   DoMixAndRender(project, false);
}

void OnMixAndRenderToNewTrack(const CommandContext &context)
{
   auto &project = context.project;
   DoMixAndRender(project, true);
}

void OnResample(const CommandContext &context)
{
   auto &project = context.project;
   auto projectRate = ProjectRate::Get(project).GetRate();
   auto &tracks = TrackList::Get( project );
   auto &undoManager = UndoManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   int newRate;

   while (true)
   {
      wxDialogWrapper dlg(&window, wxID_ANY, XO("Resample"));
      ShuttleGui S(&dlg, eIsCreating);
      wxString rate;
      wxComboBox *cb;

      rate.Printf(wxT("%ld"), lrint(projectRate));

      wxArrayStringEx rates{
         wxT("8000") ,
         wxT("11025") ,
         wxT("16000") ,
         wxT("22050") ,
         wxT("32000") ,
         wxT("44100") ,
         wxT("48000") ,
         wxT("88200") ,
         wxT("96000") ,
         wxT("176400") ,
         wxT("192000") ,
         wxT("352800") ,
         wxT("384000") ,
      };

      S.StartVerticalLay(true);
      {
         S.AddSpace(-1, 15);

         S.StartHorizontalLay(wxCENTER, false);
         {
            cb = S.AddCombo(XXO("New sample rate (Hz):"),
                            rate,
                            rates);
         }
         S.EndHorizontalLay();

         S.AddSpace(-1, 15);

         S.AddStandardButtons();
      }
      S.EndVerticalLay();

      dlg.Layout();
      dlg.Fit();
      dlg.Center();

      if (dlg.ShowModal() != wxID_OK)
      {
         return;  // user cancelled dialog
      }

      long lrate;
      if (cb->GetValue().ToLong(&lrate) && lrate >= 1 && lrate <= 1000000)
      {
         newRate = (int)lrate;
         break;
      }

      AudacityMessageBox(
         XO("The entered value is invalid"),
         XO("Error"),
         wxICON_ERROR,
         &window);
   }

   int ndx = 0;
   auto flags = UndoPush::NONE;
   for (auto wt : tracks.Selected< WaveTrack >())
   {
      auto msg = XO("Resampling track %d").Format( ++ndx );

      using namespace BasicUI;
      auto progress = MakeProgress(XO("Resample"), msg);

      // The resampling of a track may be stopped by the user.  This might
      // leave a track with multiple clips in a partially resampled state.
      // But the thrown exception will cause rollback in the application
      // level handler.

       wt->Resample(newRate, progress.get());

      // Each time a track is successfully, completely resampled,
      // commit that to the undo stack.  The second and later times,
      // consolidate.

      ProjectHistory::Get( project ).PushState(
         XO("Resampled audio track(s)"), XO("Resample Track"), flags);
      flags = flags | UndoPush::CONSOLIDATE;
   }

   undoManager.StopConsolidating();

   // Need to reset
   window.FinishAutoScroll();
}

void OnRemoveTracks(const CommandContext &context)
{
   TrackUtilities::DoRemoveTracks( context.project );
}

static void MuteTracks(const CommandContext &context, bool mute, bool selected)
{
   auto &project = context.project;
   const auto &settings = ProjectSettings::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &window = ProjectWindow::Get( project );

   auto soloSimple = settings.IsSoloSimple();
   auto soloNone = settings.IsSoloNone();

   auto iter = selected ? tracks.Selected<PlayableTrack>() : tracks.Any<PlayableTrack>();
   for (auto pt : iter)
   {
      pt->SetMute(mute);
      if (soloSimple || soloNone)
         pt->SetSolo(false);
   }

   ProjectHistory::Get( project ).ModifyState(true);
}

void OnMuteAllTracks(const CommandContext &context)
{
   MuteTracks(context, true, false);
}

void OnUnmuteAllTracks(const CommandContext &context)
{
   MuteTracks(context, false, false);
}

void OnMuteSelectedTracks(const CommandContext &context)
{
   MuteTracks(context, true, true);
}

void OnUnmuteSelectedTracks(const CommandContext &context)
{
   MuteTracks(context, false, true);
}

void OnPanLeft(const CommandContext &context)
{
   auto &project = context.project;
   DoPanTracks( project, -1.0);
}

void OnPanRight(const CommandContext &context)
{
   auto &project = context.project;
   DoPanTracks( project, 1.0);
}

void OnPanCenter(const CommandContext &context)
{
   auto &project = context.project;
   DoPanTracks( project, 0.0);
}

void OnAlignNoSync(const CommandContext &context)
{
   auto &project = context.project;

   DoAlign(project,
      context.index + kAlignLabelsCount(), false);
}

void OnAlign(const CommandContext &context)
{
   auto &project = context.project;

   bool bMoveWith;
   gPrefs->Read(wxT("/GUI/MoveSelectionWithTracks"), &bMoveWith, false);
   DoAlign(project, context.index, bMoveWith);
}

/*
// Now handled in OnAlign.
void OnAlignMoveSel(int index)
{
   DoAlign(index, true);
}
*/

void OnMoveSelectionWithTracks(const CommandContext &WXUNUSED(context) )
{
   bool bMoveWith;
   gPrefs->Read(wxT("/GUI/MoveSelectionWithTracks"), &bMoveWith, false);
   gPrefs->Write(wxT("/GUI/MoveSelectionWithTracks"), !bMoveWith);
   gPrefs->Flush();

}

#ifdef EXPERIMENTAL_SCOREALIGN
void OnScoreAlign(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   const auto rate = ProjectSettings::Get( project ).GetRate();

   int numWaveTracksSelected = 0;
   int numNoteTracksSelected = 0;
   int numOtherTracksSelected = 0;
   double endTime = 0.0;

   // Iterate through once to make sure that there is exactly
   // one WaveTrack and one NoteTrack selected.
   tracks.Selected().Visit(
      [&](WaveTrack *wt) {
         numWaveTracksSelected++;
         endTime = endTime > wt->GetEndTime() ? endTime : wt->GetEndTime();
      },
      [&](NoteTrack *) {
         numNoteTracksSelected++;
      },
      [&](Track*) {
         numOtherTracksSelected++;
      }
   );

   if(numWaveTracksSelected == 0 ||
      numNoteTracksSelected != 1 ||
      numOtherTracksSelected != 0){
      AudacityMessageBox(
         XO("Please select at least one audio track and one MIDI track.") );
      return;
   }

   // Creating the dialog also stores dialog into gScoreAlignDialog so
   // that it can be deleted by CloseScoreAlignDialog() either here or
   // if the program is quit by the user while the dialog is up.
   ScoreAlignParams params;

   // safe because the class maintains a global resource pointer
   safenew ScoreAlignDialog(params);

   CloseScoreAlignDialog();

   if (params.mStatus != wxID_OK) return;

   // We're going to do it.
   //pushing the state before the change is wrong (I think)
   //PushState(XO("Sync MIDI with Audio"), XO("Sync MIDI with Audio"));
   // Make a copy of the note track in case alignment is canceled or fails
   auto holder = nt->Duplicate();
   auto alignedNoteTrack = static_cast<NoteTrack*>(holder.get());
   // Remove offset from NoteTrack because audio is
   // mixed starting at zero and incorporating clip offsets.
   if (alignedNoteTrack->GetOffset() < 0) {
      // remove the negative offset data before alignment
      nt->Clear(alignedNoteTrack->GetOffset(), 0);
   } else if (alignedNoteTrack->GetOffset() > 0) {
      alignedNoteTrack->Shift(alignedNoteTrack->GetOffset());
   }
   alignedNoteTrack->SetOffset(0);

   WaveTrackConstArray waveTracks =
      tracks->GetWaveTrackConstArray(true /* selectionOnly */);

   int result;
   {
      Mixer mix(
         waveTracks,              // const WaveTrackConstArray &inputTracks
         false, // mayThrow -- is this right?
         Mixer::WarpOptions{ *tracks },
         0.0,                     // double startTime
         endTime,                 // double stopTime
         2,                       // int numOutChannels
         44100u,                   // size_t outBufferSize
         true,                    // bool outInterleaved
         rate,                   // double outRate
         floatSample,             // sampleFormat outFormat
         true,                    // bool highQuality = true
         NULL);                   // MixerSpec *mixerSpec = NULL

      ASAProgress progress;

      // There's a lot of adjusting made to incorporate the note track offset into
      // the note track while preserving the position of notes within beats and
      // measures. For debugging, you can see just the pre-scorealign note track
      // manipulation by setting SKIP_ACTUAL_SCORE_ALIGNMENT. You could then, for
      // example, save the modified note track in ".gro" form to read the details.
      //#define SKIP_ACTUAL_SCORE_ALIGNMENT 1
#ifndef SKIP_ACTUAL_SCORE_ALIGNMENT
      result = scorealign((void *) &mix, &mixer_process,
         2 /* channels */, 44100.0 /* srate */, endTime,
         &alignedNoteTrack->GetSeq(), &progress, params);
#else
      result = SA_SUCCESS;
#endif
   }

   if (result == SA_SUCCESS) {
      tracks->Replace(nt, holder);
      AudacityMessageBox(
         XO("Alignment completed: MIDI from %.2f to %.2f secs, Audio from %.2f to %.2f secs.")
            .Format(
               params.mMidiStart, params.mMidiEnd,
               params.mAudioStart, params.mAudioEnd) );
      ProjectHistory::Get( project )
         .PushState(XO("Sync MIDI with Audio"), XO("Sync MIDI with Audio"));
   } else if (result == SA_TOOSHORT) {
      AudacityMessageBox(
         XO(
"Alignment error: input too short: MIDI from %.2f to %.2f secs, Audio from %.2f to %.2f secs.")
            .Format(
               params.mMidiStart, params.mMidiEnd,
               params.mAudioStart, params.mAudioEnd) );
   } else if (result == SA_CANCEL) {
      // wrong way to recover...
      //project.OnUndo(); // recover any changes to note track
      return; // no message when user cancels alignment
   } else {
      //project.OnUndo(); // recover any changes to note track
      AudacityMessageBox( XO("Internal error reported by alignment process.") );
   }
}
#endif /* EXPERIMENTAL_SCOREALIGN */

void OnSortTime(const CommandContext &context)
{
   auto &project = context.project;
   DoSortTracks(project, kAudacitySortByTime);

   ProjectHistory::Get( project )
      .PushState(XO("Tracks sorted by time"), XO("Sort by Time"));
}

void OnSortName(const CommandContext &context)
{
   auto &project = context.project;
   DoSortTracks(project, kAudacitySortByName);

   ProjectHistory::Get( project )
      .PushState(XO("Tracks sorted by name"), XO("Sort by Name"));
}

void OnSyncLock(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );

   bool bSyncLockTracks;
   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &bSyncLockTracks, false);
   gPrefs->Write(wxT("/GUI/SyncLockTracks"), !bSyncLockTracks);
   gPrefs->Flush();

   // Toolbar, project sync-lock handled within
   ToolManager::ModifyAllProjectToolbarMenus();

   trackPanel.Refresh(false);
}

///The following methods operate controls on specified tracks,
///This will pop up the track panning dialog for specified track
void OnTrackPan(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );

   const auto track = TrackFocus::Get( project ).Get();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = WaveTrackControls::PanSlider( trackPanel, *wt );
      if (slider->ShowDialog())
         SetTrackPan(project, wt, slider);
   });
}

void OnTrackPanLeft(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );

   const auto track = TrackFocus::Get( project ).Get();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = WaveTrackControls::PanSlider( trackPanel, *wt );
      slider->Decrease(1);
      SetTrackPan(project, wt, slider);
   });
}

void OnTrackPanRight(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );

   const auto track = TrackFocus::Get( project ).Get();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = WaveTrackControls::PanSlider( trackPanel, *wt );
      slider->Increase(1);
      SetTrackPan(project, wt, slider);
   });
}

void OnTrackGain(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );

   /// This will pop up the track gain dialog for specified track
   const auto track = TrackFocus::Get( project ).Get();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = WaveTrackControls::GainSlider( trackPanel, *wt );
      if (slider->ShowDialog())
         SetTrackGain(project, wt, slider);
   });
}

void OnTrackGainInc(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );

   const auto track = TrackFocus::Get( project ).Get();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = WaveTrackControls::GainSlider( trackPanel, *wt );
      slider->Increase(1);
      SetTrackGain(project, wt, slider);
   });
}

void OnTrackGainDec(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );

   const auto track = TrackFocus::Get( project ).Get();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = WaveTrackControls::GainSlider( trackPanel, *wt );
      slider->Decrease(1);
      SetTrackGain(project, wt, slider);
   });
}

void OnTrackMenu(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );

   trackPanel.OnTrackMenu();
}

void OnTrackMute(const CommandContext &context)
{
   auto &project = context.project;

   // Use the temporary selection if it is specified, else the track focus
   auto track = context.temporarySelection.pTrack;
   if (!track)
      track = TrackFocus::Get( project ).Get();

   if (track) track->TypeSwitch( [&](PlayableTrack *t) {
      TrackUtilities::DoTrackMute(project, t, false);
   });
}

void OnTrackSolo(const CommandContext &context)
{
   auto &project = context.project;

   const auto track = TrackFocus::Get( project ).Get();
   if (track) track->TypeSwitch( [&](PlayableTrack *t) {
      TrackUtilities::DoTrackSolo(project, t, false);
   });
}

void OnTrackClose(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );

   const auto t = TrackFocus::Get( project ).Get();
   if (!t)
      return;

   auto isAudioActive = ProjectAudioIO::Get( project ).IsAudioActive();

   if (isAudioActive)
   {
      ProjectStatus::Get( project ).Set(
         XO("Can't delete track with active audio"));
      wxBell();
      return;
   }

   TrackUtilities::DoRemoveTrack(project, t);

   trackPanel.UpdateViewIfNoTracks();
   trackPanel.Refresh(false);
}

void OnTrackMoveUp(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );
   auto &tracks = TrackList::Get( project );

   const auto focusedTrack = TrackFocus::Get( project ).Get();
   if (tracks.CanMoveUp(focusedTrack)) {
      DoMoveTrack(project, focusedTrack, TrackUtilities::OnMoveUpID);
      trackPanel.Refresh(false);
   }
}

void OnTrackMoveDown(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );
   auto &tracks = TrackList::Get( project );

   const auto focusedTrack = TrackFocus::Get( project ).Get();
   if (tracks.CanMoveDown(focusedTrack)) {
      DoMoveTrack(project, focusedTrack, TrackUtilities::OnMoveDownID);
      trackPanel.Refresh(false);
   }
}

void OnTrackMoveTop(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );
   auto &tracks = TrackList::Get( project );

   const auto focusedTrack = TrackFocus::Get( project ).Get();
   if (tracks.CanMoveUp(focusedTrack)) {
      DoMoveTrack(project, focusedTrack, TrackUtilities::OnMoveTopID);
      trackPanel.Refresh(false);
   }
}

void OnTrackMoveBottom(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );
   auto &tracks = TrackList::Get( project );

   const auto focusedTrack = TrackFocus::Get( project ).Get();
   if (tracks.CanMoveDown(focusedTrack)) {
      DoMoveTrack(project, focusedTrack, TrackUtilities::OnMoveBottomID);
      trackPanel.Refresh(false);
   }
}

// Menu definitions

// Under /MenuBar
using namespace MenuTable;
BaseItemSharedPtr TracksMenu()
{
   // Tracks Menu (formerly Project Menu)
   using Options = CommandManager::Options;
   
   static BaseItemSharedPtr menu{
   Menu( wxT("Tracks"), XXO("&Tracks"),
      Section( "Add",
         Menu( wxT("Add"), XXO("Add &New") )
      ),

      //////////////////////////////////////////////////////////////////////////

      Section( "",
         Menu( wxT("Mix"), XXO("Mi&x"),
            // Delayed evaluation
            // Stereo to Mono is an oddball command that is also subject to control
            // by the plug-in manager, as if an effect.  Decide whether to show or
            // hide it.
            [](AudacityProject&) -> BaseItemPtr {
               const PluginID ID =
                  EffectManager::Get().GetEffectByIdentifier(wxT("StereoToMono"));
               const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
               if (plug && plug->IsEnabled())
                  return Command( wxT("Stereo to Mono"),
                     XXO("Mix Stereo Down to &Mono"), OnStereoToMono,
                     AudioIONotBusyFlag() | StereoRequiredFlag() |
                        WaveTracksSelectedFlag(), Options{} );
               else
                  return {};
            },
            Command( wxT("MixAndRender"), XXO("Mi&x and Render"),
               OnMixAndRender,
               AudioIONotBusyFlag() | WaveTracksSelectedFlag() ),
            Command( wxT("MixAndRenderToNewTrack"),
               XXO("Mix and Render to Ne&w Track"),
               OnMixAndRenderToNewTrack,
               AudioIONotBusyFlag() | WaveTracksSelectedFlag(), wxT("Ctrl+Shift+M") )
         ),

         Command( wxT("Resample"), XXO("&Resample..."), OnResample,
            AudioIONotBusyFlag() | WaveTracksSelectedFlag() )
      ),

      Section( "",
         Command( wxT("RemoveTracks"), XXO("Remo&ve Tracks"), OnRemoveTracks,
            AudioIONotBusyFlag() | AnyTracksSelectedFlag() )
      ),

      Section( "",
         Menu( wxT("Mute"), XXO("M&ute/Unmute"),
            Command( wxT("MuteAllTracks"), XXO("&Mute All Tracks"),
               OnMuteAllTracks, TracksExistFlag(), wxT("Ctrl+U") ),
            Command( wxT("UnmuteAllTracks"), XXO("&Unmute All Tracks"),
               OnUnmuteAllTracks, TracksExistFlag(), wxT("Ctrl+Shift+U") ),
            Command( wxT("MuteTracks"), XXO("Mut&e Tracks"),
               OnMuteSelectedTracks, EditableTracksSelectedFlag(), wxT("Ctrl+Alt+U") ),
            Command( wxT("UnmuteTracks"), XXO("U&nmute Tracks"),
               OnUnmuteSelectedTracks, EditableTracksSelectedFlag(), wxT("Ctrl+Alt+Shift+U") )
         ),

         Menu( wxT("Pan"), XXO("&Pan"),
            // As Pan changes are not saved on Undo stack,
            // pan settings for all tracks
            // in the project could very easily be lost unless we
            // require the tracks to be selected.
            Command( wxT("PanLeft"), XXO("&Left"), OnPanLeft,
               EditableTracksSelectedFlag(),
               Options{}.LongName( XO("Pan Left") ) ),
            Command( wxT("PanRight"), XXO("&Right"), OnPanRight,
               EditableTracksSelectedFlag(),
               Options{}.LongName( XO("Pan Right") ) ),
            Command( wxT("PanCenter"), XXO("&Center"), OnPanCenter,
               EditableTracksSelectedFlag(),
               Options{}.LongName( XO("Pan Center") ) )
         )
      ),

      Section( "",
         Menu( wxT("Align"), XXO("&Align Tracks"), // XO("Just Move Tracks"),
            Section( "",
               // Mutual alignment of tracks independent of selection or zero
               CommandGroup(wxT("Align"),
                  {
                     { wxT("EndToEnd"),     XXO("&Align End to End") },
                     { wxT("Together"),     XXO("Align &Together") },
                  },
                  OnAlignNoSync, AudioIONotBusyFlag() | EditableTracksSelectedFlag())
            ),

            Section( "",
               // Alignment commands using selection or zero
               CommandGroup(wxT("Align"),
                  alignLabels(),
                  OnAlign, AudioIONotBusyFlag() | EditableTracksSelectedFlag())
            ),

            Section( "",
               Command( wxT("MoveSelectionWithTracks"),
                  XXO("&Move Selection with Tracks (on/off)"),
                  OnMoveSelectionWithTracks,
                  AlwaysEnabledFlag,
                  Options{}.CheckTest( wxT("/GUI/MoveSelectionWithTracks"), false ) )
            )
         ),

   #if 0
         // TODO: Can these labels be made clearer?
         // Do we need this sub-menu at all?
         Menu( wxT("MoveSelectionAndTracks"), XO("Move Sele&ction and Tracks"), {
            CommandGroup(wxT("AlignMove"), alignLabels(),
               OnAlignMoveSel, AudioIONotBusyFlag() | EditableTracksSelectedFlag()),
         } ),
   #endif

         //////////////////////////////////////////////////////////////////////////

   #ifdef EXPERIMENTAL_SCOREALIGN
         Command( wxT("ScoreAlign"), XXO("Synchronize MIDI with Audio"),
            OnScoreAlign,
            AudioIONotBusyFlag() | NoteTracksSelectedFlag() | WaveTracksSelectedFlag() ),
   #endif // EXPERIMENTAL_SCOREALIGN

         //////////////////////////////////////////////////////////////////////////

         Menu( wxT("Sort"), XXO("S&ort Tracks"),
            Command( wxT("SortByTime"), XXO("By &Start Time"), OnSortTime,
               TracksExistFlag(),
               Options{}.LongName( XO("Sort by Time") ) ),
            Command( wxT("SortByName"), XXO("By &Name"), OnSortName,
               TracksExistFlag(),
               Options{}.LongName( XO("Sort by Name") ) )
         )

         //////////////////////////////////////////////////////////////////////////
      )

#ifdef EXPERIMENTAL_SYNC_LOCK
      ,

      Section( "",
         Command( wxT("SyncLock"), XXO("Sync-&Lock Tracks (on/off)"),
            OnSyncLock, AlwaysEnabledFlag,
            Options{}.CheckTest( wxT("/GUI/SyncLockTracks"), false ) )
      )

#endif

   ) };
   return menu;
}

AttachedItem sAttachment1{
   wxT(""),
   Shared( TracksMenu() )
};

BaseItemSharedPtr ExtraTrackMenu()
{
   using Options = CommandManager::Options;
   static BaseItemSharedPtr menu{
   Menu( wxT("Track"), XXO("&Track"),
      Command( wxT("TrackPan"), XXO("Change P&an on Focused Track..."),
         OnTrackPan,
         TrackPanelHasFocus() | TracksExistFlag(), wxT("Shift+P") ),
      Command( wxT("TrackPanLeft"), XXO("Pan &Left on Focused Track"),
         OnTrackPanLeft,
         TrackPanelHasFocus() | TracksExistFlag(), wxT("Alt+Shift+Left") ),
      Command( wxT("TrackPanRight"), XXO("Pan &Right on Focused Track"),
         OnTrackPanRight,
         TrackPanelHasFocus() | TracksExistFlag(), wxT("Alt+Shift+Right") ),
      Command( wxT("TrackGain"), XXO("Change Gai&n on Focused Track..."),
         OnTrackGain,
         TrackPanelHasFocus() | TracksExistFlag(), wxT("Shift+G") ),
      Command( wxT("TrackGainInc"), XXO("&Increase Gain on Focused Track"),
         OnTrackGainInc,
         TrackPanelHasFocus() | TracksExistFlag(), wxT("Alt+Shift+Up") ),
      Command( wxT("TrackGainDec"), XXO("&Decrease Gain on Focused Track"),
         OnTrackGainDec,
         TrackPanelHasFocus() | TracksExistFlag(), wxT("Alt+Shift+Down") ),
      Command( wxT("TrackMenu"), XXO("Op&en Menu on Focused Track..."),
         OnTrackMenu,
         TracksExistFlag() | TrackPanelHasFocus(),
         Options{ wxT("Shift+M") }.SkipKeyDown() ),
      Command( wxT("TrackMute"), XXO("M&ute/Unmute Focused Track"),
         OnTrackMute,
         TracksExistFlag() | TrackPanelHasFocus(), wxT("Shift+U") ),
      Command( wxT("TrackSolo"), XXO("&Solo/Unsolo Focused Track"),
         OnTrackSolo,
         TracksExistFlag() | TrackPanelHasFocus(), wxT("Shift+S") ),
      Command( wxT("TrackClose"), XXO("&Close Focused Track"),
         OnTrackClose,
         AudioIONotBusyFlag() | TrackPanelHasFocus() | TracksExistFlag(),
         wxT("Shift+C") ),
      Command( wxT("TrackMoveUp"), XXO("Move Focused Track U&p"),
         OnTrackMoveUp,
         AudioIONotBusyFlag() | TrackPanelHasFocus() | TracksExistFlag() ),
      Command( wxT("TrackMoveDown"), XXO("Move Focused Track Do&wn"),
         OnTrackMoveDown,
         AudioIONotBusyFlag() | TrackPanelHasFocus() | TracksExistFlag() ),
      Command( wxT("TrackMoveTop"), XXO("Move Focused Track to T&op"),
         OnTrackMoveTop,
         AudioIONotBusyFlag() | TrackPanelHasFocus() | TracksExistFlag() ),
      Command( wxT("TrackMoveBottom"), XXO("Move Focused Track to &Bottom"),
         OnTrackMoveBottom,
         AudioIONotBusyFlag() | TrackPanelHasFocus() | TracksExistFlag() )
   ) };
   return menu;
}

AttachedItem sAttachment2{
   wxT("Optional/Extra/Part2"),
   Shared( ExtraTrackMenu() )
};

}
