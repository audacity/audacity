/**********************************************************************

  Audacity: A Digital Audio Editor

  Snap.cpp

  Dominic Mazzoni

**********************************************************************/


#include "Snap.h"

#include <algorithm>
#include <cstdlib>

#include "Project.h"
#include "ProjectNumericFormats.h"
#include "ProjectRate.h"
#include "ProjectSettings.h"
#include "Track.h"
#include "ViewInfo.h"

inline bool operator < (SnapPoint s1, SnapPoint s2)
{
   return s1.t < s2.t;
}

SnapManager::SnapManager(const AudacityProject &project,
                         SnapPointArray candidates,
                         const ZoomInfo &zoomInfo,
                         bool noTimeSnap,
                         int pixelTolerance)
: mProject{ &project }
, mZoomInfo{ &zoomInfo }
, mPixelTolerance{ pixelTolerance }
, mNoTimeSnap{ noTimeSnap }
, mCandidates{ move( candidates ) }
, mSnapPoints{}
, mConverter{ NumericConverter::TIME }
{
   Reinit();
}

namespace {
SnapPointArray FindCandidates(
   SnapPointArray candidates, const TrackList &tracks )
{
   for ( const auto track : tracks.Any() ) {
      auto intervals = track->GetIntervals();
      for (const auto &interval : intervals) {
         candidates.emplace_back( interval.Start(), track );
         if ( interval.Start() != interval.End() )
            candidates.emplace_back( interval.End(), track );
      }
   }
   return move(candidates);
}
}

SnapManager::SnapManager(const AudacityProject &project,
            const TrackList &tracks,
            const ZoomInfo &zoomInfo,
            SnapPointArray candidates,
            bool noTimeSnap,
            int pixelTolerance)
   : SnapManager{ project,
      // Add candidates to given ones by default rules,
      // then delegate to other ctor
      FindCandidates( move(candidates), tracks ),
      zoomInfo, noTimeSnap, pixelTolerance }
{
}

SnapManager::~SnapManager()
{
}

void SnapManager::Reinit()
{
   const auto &formats = ProjectNumericFormats::Get(*mProject);
   const auto &settings = ProjectSettings::Get( *mProject );
   int snapTo = settings.GetSnapTo();
   auto rate = ProjectRate::Get(*mProject).GetRate();
   auto format = formats.GetSelectionFormat();

   // No need to reinit if these are still the same
   if (snapTo == mSnapTo && rate == mRate && format == mFormat)
   {
      return;
   }

   // Save NEW settings
   mSnapTo = snapTo;
   mRate = rate;
   mFormat = format;

   mSnapPoints.clear();

   // Grab time-snapping prefs (unless otherwise requested)
   mSnapToTime = false;

   // Look up the format string
   if (mSnapTo != SNAP_OFF && !mNoTimeSnap)
   {
      mSnapToTime = true;
      mConverter.SetSampleRate(mRate);
      mConverter.SetFormatName(mFormat);
   }

   // Add a SnapPoint at t=0
   mSnapPoints.push_back(SnapPoint{});

   // Adjust and filter the candidate points
   for (const auto &candidate : mCandidates)
      CondListAdd( candidate.t, candidate.track );

   // Sort all by time
   std::sort(mSnapPoints.begin(), mSnapPoints.end());
}

// Adds to mSnapPoints, filtering by TimeConverter
void SnapManager::CondListAdd(double t, const Track *track)
{
   if (mSnapToTime)
   {
      mConverter.SetValue(t);
   }

   if (!mSnapToTime || mConverter.GetValue() == t)
   {
      mSnapPoints.push_back(SnapPoint{ t, track });
   }
}

// Return the time of the SnapPoint at a given index
double SnapManager::Get(size_t index)
{
   return mSnapPoints[index].t;
}

// Returns the difference in time between t and the point at a given index
wxInt64 SnapManager::PixelDiff(double t, size_t index)
{
   return std::abs(mZoomInfo->TimeToPosition(t, 0) -
                   mZoomInfo->TimeToPosition(Get(index), 0));
}

// Find the index where this SnapPoint should go in
// sorted order, between i0 (inclusive) and i1 (exclusive).
size_t SnapManager::Find(double t, size_t i0, size_t i1)
{
   if (i1 <= i0 + 1)
   {
      return i0;
   }

   size_t half = (i0 + i1) / 2;

   if (t < Get(half))
   {
      return Find(t, i0, half);
   }

   return Find(t, half, i1);
}

// Find the SnapPoint nearest to time t
size_t SnapManager::Find(double t)
{
   size_t cnt = mSnapPoints.size();
   size_t index = Find(t, 0, cnt);

   // At this point, either index is the closest, or the next one
   // to the right is.  Keep moving to the right until we get a
   // different value
   size_t next = index + 1;
   while (next + 1 < cnt && Get(next) == Get(index))
   {
      next++;
   }

   // Now return whichever one is closer to time t
   if (next < cnt && PixelDiff(t, next) < PixelDiff(t, index))
   {
      return next;
   }

   return index;
}

// Helper: performs snap-to-points for Snap(). Returns true if a snap happened.
bool SnapManager::SnapToPoints(Track *currentTrack,
                               double t,
                               bool rightEdge,
                               double *outT)
{
   *outT = t;

   size_t cnt = mSnapPoints.size();
   if (cnt == 0)
   {
      return false;
   }

   // Find the nearest SnapPoint
   size_t index = Find(t);

   // If it's too far away, just give up now
   if (PixelDiff(t, index) >= mPixelTolerance)
   {
      return false;
   }

   // Otherwise, search left and right for all of the points
   // within the allowed range.
   size_t left = index;
   size_t right = index;
   size_t i;

   while (left > 0 && PixelDiff(t, left - 1) < mPixelTolerance)
   {
      left--;
   }

   while (right < cnt - 1 && PixelDiff(t, right + 1) < mPixelTolerance)
   {
      right++;
   }

   if (left == index && right == index)
   {
      // Awesome, there's only one point that matches!
      *outT = Get(index);
      return true;
   }

   size_t indexInThisTrack = 0;
   size_t countInThisTrack = 0;
   for (i = left; i <= right; ++i)
   {
      if (mSnapPoints[i].track == currentTrack)
      {
         indexInThisTrack = i;
         countInThisTrack++;
      }
   }

   if (countInThisTrack == 1)
   {
      // Cool, only one of the points is in the same track, so
      // we'll use that one.
      *outT = Get(indexInThisTrack);
      return true;
   }

   if (Get(right) - Get(left) < mEpsilon)
   {
      // OK, they're basically the same point
      if (rightEdge)
      {
         *outT = Get(right);  // Return rightmost
      }
      else {
         *outT = Get(left);   // Return leftmost
      }
      return true;
   }

   // None of the points matched, bummer.
   return false;
}

SnapResults SnapManager::Snap
(Track *currentTrack, double t, bool rightEdge)
{

   SnapResults results;
   // Check to see if we need to reinitialize
   Reinit();

   results.timeSnappedTime = results.outTime = t;
   results.outCoord = mZoomInfo->TimeToPosition(t);

   // First snap to points in mSnapPoints
   results.snappedPoint =
      SnapToPoints(currentTrack, t, rightEdge, &results.outTime);

   if (mSnapToTime) {
      // Find where it would snap time to the grid
      mConverter.ValueToControls(
         t,
         ProjectSettings::Get( *mProject ).GetSnapTo() == SNAP_NEAREST
      );
      mConverter.ControlsToValue();
      results.timeSnappedTime = mConverter.GetValue();
   }

   results.snappedTime = false;
   if (mSnapToTime)
   {
      if (results.snappedPoint)
      {
         // Since mSnapPoints only contains points on the grid, we're done
         results.snappedTime = true;
      }
      else
      {
         results.outTime = results.timeSnappedTime;
         results.snappedTime = true;
      }
   }

   if (results.Snapped())
      results.outCoord = mZoomInfo->TimeToPosition(results.outTime);

   return results;
}

/* static */ const TranslatableStrings &SnapManager::GetSnapLabels()
{
   static const TranslatableStrings result{
      XO("Off") ,
      XO("Nearest") ,
      XO("Prior") ,
   };
   return result;
}

#include "AColor.h"

void SnapManager::Draw( wxDC *dc, wxInt64 snap0, wxInt64 snap1 )
{
   AColor::SnapGuidePen(dc);
   if ( snap0 >= 0 ) {
      AColor::Line(*dc, (int)snap0, 0, (int)snap0, 30000);
   }
   if ( snap1 >= 0 ) {
      AColor::Line(*dc, (int)snap1, 0, (int)snap1, 30000);
   }
}
