/**********************************************************************

  Audacity: A Digital Audio Editor
  
  Snap.h
 
  Dominic Mazzoni

  Create one of these objects at the beginning of a click/drag.
  Then, given a time corresponding to the current mouse cursor
  position, it will tell you the closest place to snap to.

**********************************************************************/

#ifndef __AUDACITY_SNAP__
#define __AUDACITY_SNAP__

#include <wx/defs.h>
#include <wx/dynarray.h>

#include "Track.h"
#include "ViewInfo.h"

class TrackClipArray;
class TimeTextCtrl;

class SnapPoint {
 public:
   SnapPoint(double t, Track *track) {
      this->t = t;
      this->track = track;
   }
   double t;
   Track *track;
};

WX_DEFINE_SORTED_ARRAY(SnapPoint *, SnapPointArray);

class SnapManager {
 public:
   // Set gridCtrl to a TimeTextCtrl to use for snap-to-time; if NULL we won't
   // snap to time
   SnapManager(TrackList *tracks, TrackClipArray *exclusions,
               double zoom, int pixelTolerance, bool noTimeSnap = false);

   ~SnapManager();

   // The track may be NULL.
   // Returns true if the output time is not the same as the input.
   // Pass rightEdge=true if this is the right edge of a selection,
   // and false if it's the left edge.
   bool Snap(Track *currentTrack,
             double t,
             bool rightEdge,
             double *out_t,
             bool *snappedPoint,
             bool *snappedTime);

 private:
   void CondListAdd(double t, Track *tr, TimeTextCtrl *ttc);
   double Get(int index);
   double Diff(double t, int index);
   int Find(double t, int i0, int i1);
   int Find(double t);
   bool SnapToPoints(Track *currentTrack, double t, bool rightEdge,
                     double *out_t);

   double           mEpsilon;
   double           mTolerance;
   double           mZoom;
   SnapPointArray  *mSnapPoints;

   // Info for snap-to-time
   bool             mSnapToTime;
   wxString         mFormat;
};

#endif

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
