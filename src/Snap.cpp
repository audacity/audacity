/**********************************************************************

  Audacity: A Digital Audio Editor
  
  Snap.cpp
 
  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include "LabelTrack.h"
#include "Prefs.h"
#include "Project.h"
#include "Snap.h"
#include "TrackPanel.h"
#include "WaveTrack.h"
#include "widgets/TimeTextCtrl.h"

// Change this to "true" to snap to nearest and "false" to snap to previous
// As of 2013/10/23, defaulting to "true" until a decision is made on
// which method is prefered.
#define SNAP_TO_NEAREST false

static int CompareSnapPoints(SnapPoint *s1, SnapPoint *s2)
{
   return (s1->t - s2->t > 0? 1 : -1);
}

SnapManager::SnapManager(TrackList *tracks, TrackClipArray *exclusions,
                         double zoom, int pixelTolerance, bool noTimeSnap)
{
   int i;

   // Grab time-snapping prefs (unless otherwise requested)
   mSnapToTime = false;

   AudacityProject *p = GetActiveProject();
   wxASSERT(p);
   if (p)
   {
      // Look up the format string
      if (p->GetSnapTo() && !noTimeSnap) {
         mSnapToTime = true;
         mConverter.SetSampleRate(p->GetRate());
         mConverter.SetFormatName(p->GetSelectionFormat());
      }
   }

   mSnapPoints = new SnapPointArray(CompareSnapPoints);
   if (zoom > 0 && pixelTolerance > 0)
      mTolerance = pixelTolerance / zoom;
   else {
      // This shouldn't happen, but we don't want to crash if we get
      // illegal values.  The net effect of this is to never snap.
      mTolerance = 0.0;
   }
   // Two time points closer than this are considered the same
   mEpsilon = 1 / 44100.0;

   // Add a SnapPoint at t=0
   mSnapPoints->Add(new SnapPoint(0.0, NULL));

   TrackListIterator iter(tracks);
   Track *track = iter.First();
   while (track) {
      if (track->GetKind() == Track::Label) {
         LabelTrack *labelTrack = (LabelTrack *)track;
         for(i = 0; i < labelTrack->GetNumLabels(); i++) {
            const LabelStruct *label = labelTrack->GetLabel(i);
            CondListAdd(label->t, labelTrack);
            if (label->t1 != label->t) {
               CondListAdd(label->t1, labelTrack);
            }
         }
      }
      else if (track->GetKind() == Track::Wave) {
         WaveTrack *waveTrack = (WaveTrack *)track;
         WaveClipList::compatibility_iterator it;
         for (it=waveTrack->GetClipIterator(); it; it=it->GetNext()) {
            WaveClip *clip = it->GetData();
            if (exclusions) {
               bool skip = false;
               for(int j=0; j<(int)exclusions->GetCount(); j++) {
                  if ((*exclusions)[j].track == waveTrack &&
                      (*exclusions)[j].clip == clip)
                     skip = true;
               }
               if (skip)
                  continue;
            }
            CondListAdd(clip->GetStartTime(), waveTrack);
            CondListAdd(clip->GetEndTime(), waveTrack);
         }
      }
#ifdef USE_MIDI
      else if (track->GetKind() == Track::Note) {
         CondListAdd(track->GetStartTime(), track);
         CondListAdd(track->GetEndTime(), track);
      }
#endif
      track = iter.Next();
   }
}

// Adds to mSnapPoints, filtering by TimeConverter
void SnapManager::CondListAdd(double t, Track *tr)
{
   if (mSnapToTime) {
      mConverter.SetTimeValue(t);
   }

   if (!mSnapToTime || mConverter.GetTimeValue() == t) {
      mSnapPoints->Add(new SnapPoint(t, tr));
   }
}

SnapManager::~SnapManager()
{
   int len = (int)mSnapPoints->GetCount();
   int i;
   for(i = 0; i < len; i++)
      delete (*mSnapPoints)[i];
   delete mSnapPoints;
}

// Return the time of the SnapPoint at a given index
double SnapManager::Get(int index)
{
   return (*mSnapPoints)[index]->t;
}

// Returns the difference in time between t and the point at a given index
double SnapManager::Diff(double t, int index)
{
   return fabs(t - Get(index));
}

// Find the index where this SnapPoint should go in
// sorted order, between i0 (inclusive) and i1 (exclusive).
int SnapManager::Find(double t, int i0, int i1)
{
   if (i1 <= i0 + 1)
      return i0;

   int half = (i0 + i1) / 2;
   if (t < Get(half))
      return Find(t, i0, half);
   else
      return Find(t, half, i1);
}

// Find the SnapPoint nearest to time t
int SnapManager::Find(double t)
{
   int len = (int)mSnapPoints->GetCount();
   int index = Find(t, 0, len);

   // At this point, either index is the closest, or the next one
   // to the right is.  Keep moving to the right until we get a
   // different value
   int next = index + 1;
   while(next+1 < len && Get(next) == Get(index))
      next++;

   // Now return whichever one is closer to time t
   if (next < len && Diff(t, next) < Diff(t, index))
      return next;
   else
      return index;
}

// Helper: performs snap-to-points for Snap(). Returns true if a snap happened.
bool SnapManager::SnapToPoints(Track *currentTrack,
                               double t,
                               bool rightEdge,
                               double *out_t)
{
   int len = (int)mSnapPoints->GetCount();
   *out_t = t;
   if (len == 0)
      return false;

   // Find the nearest SnapPoint
   int index = Find(t);

   // If it's too far away, just give up now
   if (Diff(t, index) >= mTolerance)
      return false;

   // Otherwise, search left and right for all of the points
   // within the allowed range.
   int left = index;
   int right = index;
   int i;

   while(left > 0 && Diff(t, left-1) < mTolerance)
      left--;

   while(right < len-1 && Diff(t, right+1) < mTolerance)
      right++;

   if (left == index && right == index) {
      // Awesome, there's only one point that matches!
      *out_t = Get(index);
      return true;
   }

   int indexInThisTrack = -1;
   int countInThisTrack = 0;
   for(i=left; i<=right; i++) {
      if ((*mSnapPoints)[i]->track == currentTrack) {
         indexInThisTrack = i;
         countInThisTrack++;
      }
   }
   if (countInThisTrack == 1) {
      // Cool, only one of the points is in the same track, so
      // we'll use that one.
      *out_t = Get(indexInThisTrack);
      return true;
   }

   if (Get(right) - Get(left) < mEpsilon) {
      // OK, they're basically the same point
      if (rightEdge)
         *out_t = Get(right);  // Return rightmost
      else
         *out_t = Get(left);   // Return leftmost
      return true;
   }

   // None of the points matched, bummer.
   return false;
}

bool SnapManager::Snap(Track *currentTrack,
                       double t,
                       bool rightEdge,
                       double *out_t,
                       bool *snappedPoint,
                       bool *snappedTime)
{
   // First snap to points in mSnapPoints
   *out_t = t;
   *snappedPoint = SnapToPoints(currentTrack, t, rightEdge, out_t);

   // Now snap to the time grid
   *snappedTime = false;
   if (mSnapToTime) {
      if (*snappedPoint) {
         // Since mSnapPoints only contains points on the grid, we're done
         *snappedTime = true;
      }
      else {
         // Snap time to the grid
         mConverter.ValueToControls(t, GetActiveProject()->GetSnapTo() == SNAP_NEAREST);
         mConverter.ControlsToValue();
         *out_t = mConverter.GetTimeValue();
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
