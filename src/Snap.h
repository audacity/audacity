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
   SnapManager(TrackList *tracks, TrackClipArray *exclusions,
               double zoom, int pixelTolerance);

   ~SnapManager();

   // The track may be NULL.
   // Returns true if the output time is not the same as the input.
   // Pass rightEdge=true if this is the right edge of a selection,
   // and false if it's the left edge.
   bool Snap(Track *currentTrack,
             double t,
             bool rightEdge,
             double *out_t);

 private:
   double Get(int index);
   double Diff(double t, int index);
   int Find(double t, int i0, int i1);
   int Find(double t);

   double           mEpsilon;
   double           mTolerance;
   double           mZoom;
   SnapPointArray  *mSnapPoints;
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
