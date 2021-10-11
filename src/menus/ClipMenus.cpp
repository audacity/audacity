#include "../CommonCommandFlags.h"
#include "../ProjectHistory.h"
#include "../ProjectSettings.h"
#include "../TrackPanelAx.h"
#include "../ProjectWindow.h"
#include "../UndoManager.h"
#include "../WaveClip.h"
#include "ViewInfo.h"
#include "../WaveTrack.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../tracks/ui/TimeShiftHandle.h"

// private helper classes and functions
namespace {

struct FoundTrack {
   const WaveTrack* waveTrack{};
   int trackNum{};
   bool channel{};

   TranslatableString ComposeTrackName() const
   {
      auto name = waveTrack->GetName();
      auto shortName = name == waveTrack->GetDefaultName()
         /* i18n-hint: compose a name identifying an unnamed track by number */
         ? XO("Track %d").Format( trackNum )
         : Verbatim(name);
      auto longName = shortName;
      if (channel) {
         // TODO: more-than-two-channels-message
         if ( waveTrack->IsLeader() )
         /* i18n-hint: given the name of a track, specify its left channel */
            longName = XO("%s left").Format(shortName);
         else
         /* i18n-hint: given the name of a track, specify its right channel */
            longName = XO("%s right").Format(shortName);
      }
      return longName;
   }
};

struct FoundClip : FoundTrack {
   bool found{};
   double startTime{};
   double endTime{};
   wxString name{};
   int index{};
};

struct FoundClipBoundary : FoundTrack {
   int nFound{};    // 0, 1, or 2
   double time{};
   int index1{};
   wxString name1{};
   bool clipStart1{};
   int index2{};
   wxString name2{};
   bool clipStart2{};
};

bool TwoChannelsHaveSameBoundaries
( const WaveTrack *first, const WaveTrack *second )
{
   bool sameClips = false;

   auto& left = first->GetClips();
   auto& right = second->GetClips();
   
   // PRL:  should that have been? :
   // auto left = first->SortedClipArray();
   // auto right = second->SortedClipArray();

   if (left.size() == right.size()) {
      sameClips = true;
      for (unsigned int i = 0; i < left.size(); i++) {
         if (left[i]->GetPlayStartTime() != right[i]->GetPlayStartTime() ||
            left[i]->GetPlayEndTime() != right[i]->GetPlayEndTime()) {
            sameClips = false;
            break;
         }
      }
   }
   return sameClips;
}

bool ChannelsHaveDifferentClipBoundaries(
   const WaveTrack* wt)
{
   // This is quadratic in the number of channels
   auto channels = TrackList::Channels(wt);
   while (!channels.empty()) {
      auto channel = *channels.first++;
      for (auto other : channels) {
         if (!TwoChannelsHaveSameBoundaries(channel, other))
            return true;
      }
   }

   return false;
}

// When two clips are immediately next to each other, the GetPlayEndTime() of the 
// first clip and the GetPlayStartTime() of the second clip may not be exactly equal
// due to rounding errors. When searching for the next/prev start time from a
// given time, the following function adjusts that given time if necessary to
// take this into account. If the given time is the end time of the first of two
// clips which are next to each other, then the given time is changed to the
// start time of the second clip. This ensures that the correct next/prev start
// time is found.
double AdjustForFindingStartTimes(
   const std::vector<const WaveClip*> & clips, double time)
{
   auto q = std::find_if(clips.begin(), clips.end(),
      [&] (const WaveClip* const& clip) {
         return clip->GetPlayEndTime() == time; });
   if (q != clips.end() && q + 1 != clips.end() &&
      (*q)->SharesBoundaryWithNextClip(*(q+1))) {
      time = (*(q+1))->GetPlayStartTime();
   }

   return time;
}

// When two clips are immediately next to each other, the GetPlayEndTime() of the
// first clip and the GetPlayStartTime() of the second clip may not be exactly equal
// due to rounding errors. When searching for the next/prev end time from a
// given time, the following function adjusts that given time if necessary to
// take this into account. If the given time is the start time of the second of
// two clips which are next to each other, then the given time is changed to the
// end time of the first clip. This ensures that the correct next/prev end time
// is found.
double AdjustForFindingEndTimes(
   const std::vector<const WaveClip*>& clips, double time)
{
   auto q = std::find_if(clips.begin(), clips.end(),
      [&] (const WaveClip* const& clip) {
         return clip->GetPlayStartTime() == time; });
   if (q != clips.end() && q != clips.begin() &&
      (*(q - 1))->SharesBoundaryWithNextClip(*q)) {
      time = (*(q-1))->GetPlayEndTime();
   }

   return time;
}

FoundClipBoundary FindNextClipBoundary
(const WaveTrack* wt, double time)
{
   FoundClipBoundary result{};
   result.waveTrack = wt;
   const auto clips = wt->SortedClipArray();
   double timeStart = AdjustForFindingStartTimes(clips, time);
   double timeEnd = AdjustForFindingEndTimes(clips, time);

   auto pStart = std::find_if(clips.begin(), clips.end(),
      [&] (const WaveClip* const& clip) {
         return clip->GetPlayStartTime() > timeStart; });
   auto pEnd = std::find_if(clips.begin(), clips.end(),
      [&] (const WaveClip* const& clip) {
         return clip->GetPlayEndTime() > timeEnd; });

   if (pStart != clips.end() && pEnd != clips.end()) {
      if ((*pEnd)->SharesBoundaryWithNextClip(*pStart)) {
         // boundary between two clips which are immediately next to each other.
         result.nFound = 2;
         result.time = (*pEnd)->GetPlayEndTime();
         result.index1 = std::distance(clips.begin(), pEnd);
         result.name1 = (*pEnd)->GetName();
         result.clipStart1 = false;
         result.index2 = std::distance(clips.begin(), pStart);
         result.name2 = (*pStart)->GetName();
         result.clipStart2 = true;
      }
      else if ((*pStart)->GetPlayStartTime() < (*pEnd)->GetPlayEndTime()) {
         result.nFound = 1;
         result.time = (*pStart)->GetPlayStartTime();
         result.index1 = std::distance(clips.begin(), pStart);
         result.name1 = (*pStart)->GetName();
         result.clipStart1 = true;
      }
      else  {
         result.nFound = 1;
         result.time = (*pEnd)->GetPlayEndTime();
         result.index1 = std::distance(clips.begin(), pEnd);
         result.name1 = (*pEnd)->GetName();
         result.clipStart1 = false;
      }
   }
   else if (pEnd != clips.end()) {
      result.nFound = 1;
      result.time = (*pEnd)->GetPlayEndTime();
      result.index1 = std::distance(clips.begin(), pEnd);
      result.name1 = (*pEnd)->GetName();
      result.clipStart1 = false;
   }

   return result;
}

FoundClipBoundary FindPrevClipBoundary(const WaveTrack* wt, double time)
{
   FoundClipBoundary result{};
   result.waveTrack = wt;
   const auto clips = wt->SortedClipArray();
   double timeStart = AdjustForFindingStartTimes(clips, time);
   double timeEnd = AdjustForFindingEndTimes(clips, time);

   auto pStart = std::find_if(clips.rbegin(), clips.rend(),
      [&] (const WaveClip* const& clip) {
         return clip->GetPlayStartTime() < timeStart; });
   auto pEnd = std::find_if(clips.rbegin(), clips.rend(),
      [&] (const WaveClip* const& clip) {
         return clip->GetPlayEndTime() < timeEnd; });

   if (pStart != clips.rend() && pEnd != clips.rend()) {
      if ((*pEnd)->SharesBoundaryWithNextClip(*pStart)) {
         // boundary between two clips which are immediately next to each other.
         result.nFound = 2;
         result.time = (*pStart)->GetPlayStartTime();
         result.index1 =
            static_cast<int>(clips.size()) - 1 -
               std::distance(clips.rbegin(), pStart);
         result.name1 = (*pStart)->GetName();
         result.clipStart1 = true;
         result.index2 =
            static_cast<int>(clips.size()) - 1 -
               std::distance(clips.rbegin(), pEnd);
         result.name2 = (*pEnd)->GetName();
         result.clipStart2 = false;
      }
      else if ((*pStart)->GetPlayStartTime() > (*pEnd)->GetPlayEndTime()) {
         result.nFound = 1;
         result.time = (*pStart)->GetPlayStartTime();
         result.index1 =
            static_cast<int>(clips.size()) - 1 -
               std::distance(clips.rbegin(), pStart);
         result.name1 = (*pStart)->GetName();
         result.clipStart1 = true;
      }
      else {
         result.nFound = 1;
         result.time = (*pEnd)->GetPlayEndTime();
         result.index1 =
            static_cast<int>(clips.size()) - 1 -
               std::distance(clips.rbegin(), pEnd);
         result.name1 = (*pEnd)->GetName();
         result.clipStart1 = false;
      }
   }
   else if (pStart != clips.rend()) {
      result.nFound = 1;
      result.time = (*pStart)->GetPlayStartTime();
      result.index1 =
         static_cast<int>(clips.size()) - 1 -
            std::distance(clips.rbegin(), pStart);
      result.name1 = (*pStart)->GetName();
      result.clipStart1 = true;
   }

   return result;
}

int FindClipBoundaries
(AudacityProject &project,
 double time, bool next, std::vector<FoundClipBoundary>& finalResults)
{
   auto &tracks = TrackList::Get( project );
   finalResults.clear();

   bool anyWaveTracksSelected{ tracks.Selected< const WaveTrack >() };


   // first search the tracks individually

   std::vector<FoundClipBoundary> results;

   int nTracksSearched = 0;
   auto leaders = tracks.Leaders();
   auto rangeLeaders = leaders.Filter<const WaveTrack>();
   if (anyWaveTracksSelected)
      rangeLeaders = rangeLeaders + &Track::GetSelected;
   for (auto waveTrack : rangeLeaders) {
      bool stereoAndDiff = ChannelsHaveDifferentClipBoundaries(waveTrack);

      auto rangeChan = stereoAndDiff
         ? TrackList::Channels( waveTrack )
         : TrackList::SingletonRange(waveTrack);

      for (auto wt : rangeChan) {
         auto result = next ? FindNextClipBoundary(wt, time) :
         FindPrevClipBoundary(wt, time);
         if (result.nFound > 0) {
            result.trackNum =
               1 + std::distance( leaders.begin(), leaders.find( waveTrack ) );
            result.channel = stereoAndDiff;
            results.push_back(result);
         }
      }

      nTracksSearched++;
   }


   if (results.size() > 0) {
      // If any clip boundaries were found
      // find the clip boundary or boundaries with the min/max time
      auto compare = [] (const FoundClipBoundary& a, const FoundClipBoundary&b)
         { return a.time < b.time; };

      auto p = next ? min_element(results.begin(), results.end(), compare ) :
         max_element(results.begin(), results.end(), compare);

      for ( auto &r : results )
         if ( r.time == (*p).time )
            finalResults.push_back( r );
   }

   return nTracksSearched; // can be used for screen reader messages if required
}

// for clip boundary commands, create a message for screen readers
TranslatableString ClipBoundaryMessage(
   const std::vector<FoundClipBoundary>& results)
{
   TranslatableString message;
   for (auto& result : results) {

      auto longName = result.ComposeTrackName();

      TranslatableString str;
      auto nClips = result.waveTrack->GetNumClips();
      if (result.nFound < 2) {
         str = XP(
            /* i18n-hint:
               First %s is replaced with the noun "start" or "end"
               identifying one end of a clip,
               second string is the name of that clip,
               first number gives the position of that clip in a sequence
               of clips,
               last number counts all clips,
               and the last string is the name of the track containing the
               clips.
             */
            "%s %s, %d of %d clip %s",
            "%s %s, %d of %d clips %s",
            3
         )(
            result.clipStart1 ? XO("start") : XO("end"),
            result.name1,
            result.index1 + 1,
            nClips,
            longName
         );
      }
      else {
         str = XP(
            /* i18n-hint:
               First and third %s are each replaced with the noun "start"
               or with "end", identifying and end of a clip,
               second and fourth strings are the names of those clips,
               first and second numbers give the position of those clips in
               a sequence of clips,
               last number counts all clips,
               and the last string is the name of the track containing the
               clips.
             */
            "%s %s and %s %s, %d and %d of %d clip %s",
            "%s %s and %s %s, %d and %d of %d clips %s",
            6
         )(
            result.clipStart1 ? XO("start") : XO("end"),
            result.name1,
            result.clipStart2 ? XO("start") : XO("end"),
            result.name2,
            result.index1 + 1,
            result.index2 + 1,
            nClips,
            longName
         );
      }

      if (message.empty())
         message = str;
      else
         message = XO("%s, %s").Format( message, str );
   }

   return message;
}

void DoSelectClipBoundary(AudacityProject &project, bool next)
{
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &trackFocus = TrackFocus::Get( project );

   std::vector<FoundClipBoundary> results;
   FindClipBoundaries(project, next ? selectedRegion.t1() :
      selectedRegion.t0(), next, results);

   if (results.size() > 0) {
      // note that if there is more than one result, each has the same time
      // value.
      if (next)
         selectedRegion.setT1(results[0].time);
      else
         selectedRegion.setT0(results[0].time);

      ProjectHistory::Get( project ).ModifyState(false);

      auto message = ClipBoundaryMessage(results);
      trackFocus.MessageForScreenReader(message);
   }
}

FoundClip FindNextClip
(AudacityProject &project, const WaveTrack* wt, double t0, double t1)
{
   (void)project;//Compiler food.

   FoundClip result{};
   result.waveTrack = wt;
   const auto clips = wt->SortedClipArray();

   t0 = AdjustForFindingStartTimes(clips, t0);

   {
      auto p = std::find_if(clips.begin(), clips.end(),
         [&] (const WaveClip* const& clip) {
            return clip->GetPlayStartTime() == t0; });
      if (p != clips.end() && (*p)->GetPlayEndTime() > t1) {
         result.found = true;
         result.startTime = (*p)->GetPlayStartTime();
         result.endTime = (*p)->GetPlayEndTime();
         result.name = (*p)->GetName();
         result.index = std::distance(clips.begin(), p);
         return result;
      }
   }

   {
      auto p = std::find_if(clips.begin(), clips.end(),
         [&] (const WaveClip* const& clip) {
            return clip->GetPlayStartTime() > t0; });
      if (p != clips.end()) {
         result.found = true;
         result.startTime = (*p)->GetPlayStartTime();
         result.endTime = (*p)->GetPlayEndTime();
         result.name = (*p)->GetName();
         result.index = std::distance(clips.begin(), p);
         return result;
      }
   }

   return result;
}

FoundClip FindPrevClip
(AudacityProject &project, const WaveTrack* wt, double t0, double t1)
{
   (void)project;//Compiler food.

   FoundClip result{};
   result.waveTrack = wt;
   const auto clips = wt->SortedClipArray();

   t0 = AdjustForFindingStartTimes(clips, t0);

   {
      auto p = std::find_if(clips.begin(), clips.end(),
         [&] (const WaveClip* const& clip) {
            return clip->GetPlayStartTime() == t0; });
      if (p != clips.end() && (*p)->GetPlayEndTime() < t1) {
         result.found = true;
         result.startTime = (*p)->GetPlayStartTime();
         result.endTime = (*p)->GetPlayEndTime();
         result.name = (*p)->GetName();
         result.index = std::distance(clips.begin(), p);
         return result;
      }
   }
   
   {
      auto p = std::find_if(clips.rbegin(), clips.rend(),
         [&] (const WaveClip* const& clip) {
            return clip->GetPlayStartTime() < t0; });
      if (p != clips.rend()) {
         result.found = true;
         result.startTime = (*p)->GetPlayStartTime();
         result.endTime = (*p)->GetPlayEndTime();
         result.name = (*p)->GetName();
         result.index =
            static_cast<int>(clips.size()) - 1 -
               std::distance(clips.rbegin(), p);
         return result;
      }
   }

   return result;
}

int FindClips
(AudacityProject &project,
 double t0, double t1, bool next, std::vector<FoundClip>& finalResults)
{
   auto &tracks = TrackList::Get( project );
   finalResults.clear();

   bool anyWaveTracksSelected{ tracks.Selected< const WaveTrack >() };

   // first search the tracks individually

   std::vector<FoundClip> results;

   int nTracksSearched = 0;
   auto leaders = tracks.Leaders();
   auto rangeLeaders = leaders.Filter<const WaveTrack>();
   if (anyWaveTracksSelected)
      rangeLeaders = rangeLeaders + &Track::GetSelected;
   for (auto waveTrack : rangeLeaders) {
      bool stereoAndDiff = ChannelsHaveDifferentClipBoundaries(waveTrack);

      auto rangeChans = stereoAndDiff
         ? TrackList::Channels( waveTrack )
         : TrackList::SingletonRange( waveTrack );

      for ( auto wt : rangeChans ) {
         auto result = next ? FindNextClip(project, wt, t0, t1) :
            FindPrevClip(project, wt, t0, t1);
         if (result.found) {
            result.trackNum =
               1 + std::distance( leaders.begin(), leaders.find( waveTrack ) );
            result.channel = stereoAndDiff;
            results.push_back(result);
         }
      }

      nTracksSearched++;
   }


   if (results.size() > 0) {
      // if any clips were found,
      // find the clip or clips with the min/max start time
      auto compareStart = [] (const FoundClip& a, const FoundClip& b)
         { return a.startTime < b.startTime; };

      auto pStart = next
         ? std::min_element(results.begin(), results.end(), compareStart)
         : std::max_element(results.begin(), results.end(), compareStart);

      std::vector<FoundClip> resultsStartTime;
      for ( auto &r : results )
         if ( r.startTime == (*pStart).startTime )
            resultsStartTime.push_back( r );

      if (resultsStartTime.size() > 1) {
         // more than one clip with same start time so
         // find the clip or clips with the min/max end time
         auto compareEnd = [] (const FoundClip& a, const FoundClip& b)
            { return a.endTime < b.endTime; };

         auto pEnd = next ? std::min_element(resultsStartTime.begin(),
            resultsStartTime.end(), compareEnd) :
            std::max_element(resultsStartTime.begin(),
            resultsStartTime.end(), compareEnd);

         for ( auto &r : resultsStartTime )
            if ( r.endTime == (*pEnd).endTime )
               finalResults.push_back( r );
      }
      else {
         finalResults = resultsStartTime;
      }
   }

   return nTracksSearched; // can be used for screen reader messages if required
}

void DoSelectClip(AudacityProject &project, bool next)
{
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &trackFocus = TrackFocus::Get( project );
   auto &window = ProjectWindow::Get( project );

   std::vector<FoundClip> results;
   FindClips(project, selectedRegion.t0(),
      selectedRegion.t1(), next, results);

   if (results.size() > 0) {
      // note that if there is more than one result, each has the same start
      // and end time
      double t0 = results[0].startTime;
      double t1 = results[0].endTime;
      selectedRegion.setTimes(t0, t1);
      ProjectHistory::Get( project ).ModifyState(false);
      window.ScrollIntoView(selectedRegion.t0());

      // create and send message to screen reader
      TranslatableString message;
      for (auto& result : results) {
         auto longName = result.ComposeTrackName();
         auto nClips = result.waveTrack->GetNumClips();
         auto str = XP(
            /* i18n-hint:
               first string is the name of a clip,
               first number gives the position of that clip
               in a sequence of clips,
               last number counts all clips,
               last string names a track */
            "%s, %d of %d clip %s",
            "%s, %d of %d clips %s",
            2
         )(
            result.name,
            result.index + 1,
            nClips,
            longName
         );

         if (message.empty())
            message = str;
         else
            message = XO("%s, %s").Format( message, str );
      }
      trackFocus.MessageForScreenReader(message);
   }
}

void DoCursorClipBoundary
(AudacityProject &project, bool next)
{
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &trackFocus = TrackFocus::Get( project );
   auto &window = ProjectWindow::Get( project );

   std::vector<FoundClipBoundary> results;
   FindClipBoundaries(project, next ? selectedRegion.t1() :
      selectedRegion.t0(), next, results);

   if (results.size() > 0) {
      // note that if there is more than one result, each has the same time
      // value.
      double time = results[0].time;
      selectedRegion.setTimes(time, time);
      ProjectHistory::Get( project ).ModifyState(false);
      window.ScrollIntoView(selectedRegion.t0());

      auto message = ClipBoundaryMessage(results);
      trackFocus.MessageForScreenReader(message);
   }
}

// This function returns the amount moved.  Possibly 0.0.
double DoClipMove( AudacityProject &project, Track *track,
     TrackList &trackList, bool syncLocked, bool right )
{
   auto &viewInfo = ViewInfo::Get(project);
   auto &selectedRegion = viewInfo.selectedRegion;

   if (track) {
      ClipMoveState state;

      auto t0 = selectedRegion.t0();

      std::unique_ptr<TrackShifter> uShifter;

      // Find the first channel that has a clip at time t0
      auto hitTestResult = TrackShifter::HitTestResult::Track;
      for (auto channel : TrackList::Channels(track) ) {
         uShifter = MakeTrackShifter::Call( *channel, project );
         if ( (hitTestResult = uShifter->HitTest( t0, viewInfo )) ==
             TrackShifter::HitTestResult::Miss )
            uShifter.reset();
         else
            break;
      }

      if (!uShifter)
         return 0.0;
      auto pShifter = uShifter.get();
      auto desiredT0 = viewInfo.OffsetTimeByPixels( t0, ( right ? 1 : -1 ) );
      auto desiredSlideAmount = pShifter->HintOffsetLarger( desiredT0 - t0 );

      state.Init( project, pShifter->GetTrack(), hitTestResult, std::move( uShifter ),
         t0, viewInfo, trackList, syncLocked );

      auto hSlideAmount = state.DoSlideHorizontal( desiredSlideAmount );

      double newT0 = t0 + hSlideAmount;
      if (hitTestResult != TrackShifter::HitTestResult::Track) {
         // If necessary, correct for rounding errors. For example,
         // for a wavetrack, ensure that t0 is still in the clip
         // which it was within before the move.
         // (pShifter is still undestroyed in the ClipMoveState.)
         newT0 = pShifter->AdjustT0(newT0);
      }

      double diff = selectedRegion.duration();
      selectedRegion.setTimes(newT0, newT0 + diff);

      return hSlideAmount;
   };
   return 0.0;
}

void DoClipLeftOrRight
(AudacityProject &project, bool right, bool keyUp )
{
   auto &undoManager = UndoManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   if (keyUp) {
      undoManager.StopConsolidating();
      return;
   }

   auto &trackFocus = TrackFocus::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &selectedRegion = viewInfo.selectedRegion;
   const auto &settings = ProjectSettings::Get( project );
   auto &tracks = TrackList::Get( project );
   auto isSyncLocked = settings.IsSyncLocked();

   auto amount = DoClipMove( project, trackFocus.Get(),
        tracks, isSyncLocked, right );

   window.ScrollIntoView(selectedRegion.t0());

   if (amount != 0.0) {
      auto message = right? XO("Time shifted clips to the right") :
         XO("Time shifted clips to the left");

      // The following use of the UndoPush flags is so that both a single
      // keypress (keydown, then keyup), and holding down a key
      // (multiple keydowns followed by a keyup) result in a single
      // entry in Audacity's history dialog.
      ProjectHistory::Get( project )
         .PushState(message, XO("Time-Shift"), UndoPush::CONSOLIDATE);
   }

   if ( amount == 0.0 )
      trackFocus.MessageForScreenReader( XO("clip not moved"));
}

}

/// Namespace for functions for Clip menu
namespace ClipActions {

// exported helper functions
// none

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnSelectPrevClipBoundaryToCursor
(const CommandContext &context)
{
   auto &project = context.project;
   DoSelectClipBoundary(project, false);
}

void OnSelectCursorToNextClipBoundary
(const CommandContext &context)
{
   auto &project = context.project;
   DoSelectClipBoundary(project, true);
}

void OnSelectPrevClip(const CommandContext &context)
{
   auto &project = context.project;
   DoSelectClip(project, false);
}

void OnSelectNextClip(const CommandContext &context)
{
   auto &project = context.project;
   DoSelectClip(project, true);
}

void OnCursorPrevClipBoundary(const CommandContext &context)
{
   AudacityProject &project = context.project;

   DoCursorClipBoundary(project, false);
}

void OnCursorNextClipBoundary(const CommandContext &context)
{
   AudacityProject &project = context.project;
   
   DoCursorClipBoundary(project, true);
}

// PRL: Clip moving functions -- more than just selection adjustment.  Do they
// really belong in these navigation menus?
void OnClipLeft(const CommandContext &context)
{
   auto &project = context.project;
   auto evt = context.pEvt;
   if (evt)
      DoClipLeftOrRight( project, false, evt->GetEventType() == wxEVT_KEY_UP );
   else  {              // called from menu, so simulate keydown and keyup
      DoClipLeftOrRight( project, false, false );
      DoClipLeftOrRight( project, false, true );
   }
}

void OnClipRight(const CommandContext &context)
{
   auto &project = context.project;
   auto evt = context.pEvt;
   if (evt)
      DoClipLeftOrRight( project, true, evt->GetEventType() == wxEVT_KEY_UP );
   else  {              // called from menu, so simulate keydown and keyup
      DoClipLeftOrRight( project, true, false );
      DoClipLeftOrRight( project, true, true );
   }
}

}; // struct Handler

} // namespace

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static ClipActions::Handler instance;
   return instance;
};

// Menu definitions

#define FN(X) (& ClipActions::Handler :: X)

namespace {
using namespace MenuTable;

// Register menu items

BaseItemSharedPtr ClipSelectMenu()
{
   using Options = CommandManager::Options;

   static BaseItemSharedPtr menu {
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Clip"), XXO("Clip B&oundaries"),
      Command( wxT("SelPrevClipBoundaryToCursor"),
         XXO("Pre&vious Clip Boundary to Cursor"),
         FN(OnSelectPrevClipBoundaryToCursor),
         WaveTracksExistFlag() ),
      Command( wxT("SelCursorToNextClipBoundary"),
         XXO("Cursor to Ne&xt Clip Boundary"),
         FN(OnSelectCursorToNextClipBoundary),
         WaveTracksExistFlag() ),
      Command( wxT("SelPrevClip"), XXO("Previo&us Clip"),
         FN(OnSelectPrevClip), WaveTracksExistFlag(),
         Options{ wxT("Alt+,"), XO("Select Previous Clip") } ),
      Command( wxT("SelNextClip"), XXO("N&ext Clip"), FN(OnSelectNextClip),
         WaveTracksExistFlag(),
         Options{ wxT("Alt+."), XO("Select Next Clip") } )
   ) ) };
   return menu;
}

AttachedItem sAttachment1{
   wxT("Select/Basic"),
   Shared( ClipSelectMenu() )
};

BaseItemSharedPtr ClipCursorItems()
{
   using Options = CommandManager::Options;

   static BaseItemSharedPtr items{
   ( FinderScope{ findCommandHandler },
   Items( wxT("Clip"),
      Command( wxT("CursPrevClipBoundary"), XXO("Pre&vious Clip Boundary"),
         FN(OnCursorPrevClipBoundary),
         WaveTracksExistFlag(),
         Options{}.LongName( XO("Cursor to Prev Clip Boundary") ) ),
      Command( wxT("CursNextClipBoundary"), XXO("Ne&xt Clip Boundary"),
         FN(OnCursorNextClipBoundary),
         WaveTracksExistFlag(),
         Options{}.LongName( XO("Cursor to Next Clip Boundary") ) )
   ) ) };
   return items;
}

AttachedItem sAttachment2{
   { wxT("Transport/Basic/Cursor"),
     { OrderingHint::Before, wxT("CursProjectStart") } },
   Shared( ClipCursorItems() )
};

BaseItemSharedPtr ExtraTimeShiftItems()
{
   using Options = CommandManager::Options;
   static BaseItemSharedPtr items{
   ( FinderScope{ findCommandHandler },
   Items( wxT("TimeShift"),
      Command( wxT("ClipLeft"), XXO("Time Shift &Left"), FN(OnClipLeft),
         TracksExistFlag() | TrackPanelHasFocus(), Options{}.WantKeyUp() ),
      Command( wxT("ClipRight"), XXO("Time Shift &Right"), FN(OnClipRight),
         TracksExistFlag() | TrackPanelHasFocus(), Options{}.WantKeyUp() )
   ) ) };
   return items;
}

AttachedItem sAttachment3{
  { wxT("Optional/Extra/Part1/Edit"), { OrderingHint::End, {} } },
  Shared( ExtraTimeShiftItems() )
};

}

#undef FN
