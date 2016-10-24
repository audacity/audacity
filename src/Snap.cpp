/**********************************************************************

  Audacity: A Digital Audio Editor

  Snap.cpp

  Dominic Mazzoni

**********************************************************************/

#include "Audacity.h"
#include "Snap.h"

#include <algorithm>
#include <cstdlib>

#include "Project.h"
#include "LabelTrack.h"
#include "TrackPanel.h"
#include "WaveTrack.h"

#include <wx/arrimpl.cpp>

inline bool operator < (SnapPoint s1, SnapPoint s2)
{
   return s1.t < s2.t;
}

TrackClip::TrackClip(Track *t, WaveClip *c)
{
   track = origTrack = t;
   dstTrack = NULL;
   clip = c;
}

#ifndef __AUDACITY_OLD_STD__
TrackClip::TrackClip(TrackClip&& tc)
   : track{tc.track}, origTrack{tc.origTrack}, dstTrack{tc.dstTrack},
   clip{tc.clip}, holder{std::move(tc.holder)} {}
#endif

TrackClip::~TrackClip()
{

}

SnapManager::SnapManager(TrackList *tracks,
                         const ZoomInfo *zoomInfo,
                         const TrackClipArray *clipExclusions,
                         const TrackArray *trackExclusions,
                         bool noTimeSnap,
                         int pixelTolerance)
:  mConverter(NumericConverter::TIME)
{
   mTracks = tracks;
   mZoomInfo = zoomInfo;
   mClipExclusions = clipExclusions;
   mTrackExclusions = trackExclusions;
   mPixelTolerance = pixelTolerance;
   mNoTimeSnap = noTimeSnap;

   mProject = GetActiveProject();
   wxASSERT(mProject);

   mSnapTo = 0;
   mRate = 0.0;
   mFormat.Empty();

   // Two time points closer than this are considered the same
   mEpsilon = 1 / 44100.0;

   Reinit();
}

SnapManager::~SnapManager()
{
}

void SnapManager::Reinit()
{
   int snapTo = mProject->GetSnapTo();
   double rate = mProject->GetRate();
   wxString format = mProject->GetSelectionFormat();

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

   TrackListIterator iter(mTracks);
   for (Track *track = iter.First();  track; track = iter.Next())
   {
      if (mTrackExclusions &&
          mTrackExclusions->end() !=
          std::find(mTrackExclusions->begin(), mTrackExclusions->end(), track))
      {
         continue;
      }

      if (track->GetKind() == Track::Label)
      {
         LabelTrack *labelTrack = (LabelTrack *)track;
         for (int i = 0, cnt = labelTrack->GetNumLabels(); i < cnt; ++i)
         {
            const LabelStruct *label = labelTrack->GetLabel(i);
            const double t0 = label->getT0();
            const double t1 = label->getT1();
            CondListAdd(t0, labelTrack);
            if (t1 != t0)
            {
               CondListAdd(t1, labelTrack);
            }
         }
      }
      else if (track->GetKind() == Track::Wave)
      {
         auto waveTrack = static_cast<const WaveTrack *>(track);
         for (const auto &clip: waveTrack->GetClips())
         {
            if (mClipExclusions)
            {
               bool skip = false;
               for (size_t j = 0, cnt = mClipExclusions->size(); j < cnt; ++j)
               {
                  if ((*mClipExclusions)[j].track == waveTrack &&
                      (*mClipExclusions)[j].clip == clip.get())
                  {
                     skip = true;
                     break;
                  }
               }

               if (skip)
               {
                  continue;
               }
            }

            CondListAdd(clip->GetStartTime(), waveTrack);
            CondListAdd(clip->GetEndTime(), waveTrack);
         }
      }
#ifdef USE_MIDI
      else if (track->GetKind() == Track::Note)
      {
         CondListAdd(track->GetStartTime(), track);
         CondListAdd(track->GetEndTime(), track);
      }
#endif
   }

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

   // indexInThisTrack is ONLY used if count > 0
   // and is initialised then, so we can 'initialise' it
   // to anything to keep compiler quiet.
   size_t indexInThisTrack = left;
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

bool SnapManager::Snap(Track *currentTrack,
                       double t,
                       bool rightEdge,
                       double *outT,
                       bool *snappedPoint,
                       bool *snappedTime)
{
   // Check to see if we need to reinitialize
   Reinit();

   // First snap to points in mSnapPoints
   *outT = t;
   *snappedPoint = SnapToPoints(currentTrack, t, rightEdge, outT);

   // Now snap to the time grid
   *snappedTime = false;
   if (mSnapToTime)
   {
      if (*snappedPoint)
      {
         // Since mSnapPoints only contains points on the grid, we're done
         *snappedTime = true;
      }
      else
      {
         // Snap time to the grid
         mConverter.ValueToControls(t, GetActiveProject()->GetSnapTo() == SNAP_NEAREST);
         mConverter.ControlsToValue();
         *outT = mConverter.GetValue();
         *snappedTime = true;
      }
   }

   return *snappedPoint || *snappedTime;
}

/* static */ wxArrayString SnapManager::GetSnapLabels()
{
   wxArrayString labels;

   labels.Add(_("Off"));
   labels.Add(_("Nearest"));
   labels.Add(_("Prior"));

   return labels;
}

/* static */ wxArrayString SnapManager::GetSnapValues()
{
   wxArrayString values;

   values.Add(wxT("Off"));
   values.Add(wxT("Nearest"));
   values.Add(wxT("Prior"));

   return values;
}

/* static */ const wxString & SnapManager::GetSnapValue(int index)
{
   wxArrayString values = SnapManager::GetSnapValues();

   if (index >= 0 && index < (int) values.GetCount())
   {
      return values[index];
   }

   return values[SNAP_OFF];
}

/* static */ int SnapManager::GetSnapIndex(const wxString & value)
{
   wxArrayString values = SnapManager::GetSnapValues();
   int index = values.Index(value);

   if (index != wxNOT_FOUND)
   {
      return index;
   }

   return SNAP_OFF;
}
