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

#include <vector>

#include <wx/defs.h>

#include "widgets/NumericTextCtrl.h"

class Track;
class WaveClip;
class TrackList;
class ZoomInfo;

class TrackClip
{
public:
   TrackClip(Track *t, WaveClip *c) { track = t; clip = c; }
   Track *track;
   WaveClip *clip;
};

typedef std::vector<TrackClip> TrackClipArray;

enum
{
   SNAP_OFF,
   SNAP_NEAREST,
   SNAP_PRIOR
};

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
      const ZoomInfo &zoomInfo, int pixelTolerance, bool noTimeSnap = false);

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

   static wxArrayString GetSnapLabels();
   static wxArrayString GetSnapValues();
   static const wxString & GetSnapValue(int index);
   static int GetSnapIndex(const wxString & value);

private:
   void CondListAdd(double t, Track *tr);
   double Get(int index);
   wxInt64 PixelDiff(double t, int index);
   int Find(double t, int i0, int i1);
   int Find(double t);
   bool SnapToPoints(Track *currentTrack, double t, bool rightEdge,
      double *out_t);

   double           mEpsilon;
   SnapPointArray  *mSnapPoints;

   // Info for snap-to-time
   NumericConverter    mConverter;
   bool             mSnapToTime;

   const wxInt64    mPixelTolerance;
   const ZoomInfo  &mZoomInfo;
};

#endif
