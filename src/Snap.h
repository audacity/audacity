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
#include <wx/string.h>
#include "widgets/NumericTextCtrl.h"

class AudacityProject;
class Track;
class TrackArray;
class TrackClipArray;
class WaveClip;
class WaveTrack;
class TrackList;
class ZoomInfo;

class TrackClip
{
public:
   TrackClip(Track *t, WaveClip *c);

#ifndef __AUDACITY_OLD_STD__
   // TrackClip(TrackClip&&) = default; is not supported by vs2013/5 so explicit version needed
   TrackClip(TrackClip&&);
#endif

   ~TrackClip();

   Track *track;
   Track *origTrack;
   WaveTrack *dstTrack;
   WaveClip *clip;
   movable_ptr<WaveClip> holder;
};

class TrackClipArray : public std::vector < TrackClip > {};

enum
{
   SNAP_OFF,
   SNAP_NEAREST,
   SNAP_PRIOR
};

const int kPixelTolerance = 4;

class SnapPoint
{
public:
   explicit
   SnapPoint(double t_ = 0.0, const Track *track_ = nullptr)
      : t(t_), track(track_)
   {
   }

   double t;
   const Track *track;
};

using SnapPointArray = std::vector < SnapPoint > ;

class SnapManager
{
public:
   SnapManager(TrackList *tracks,
               const ZoomInfo *zoomInfo,
               const TrackClipArray *clipExclusions = NULL,
               const TrackArray *trackExclusions = NULL,
               bool noTimeSnap = false,
               int pixelTolerance = kPixelTolerance);
   ~SnapManager();

   // The track may be NULL.
   // Returns true if the output time is not the same as the input.
   // Pass rightEdge=true if this is the right edge of a selection,
   // and false if it's the left edge.
   bool Snap(Track *currentTrack,
             double t,
             bool rightEdge,
             double *outT,
             bool *snappedPoint,
             bool *snappedTime);

   static wxArrayString GetSnapLabels();
   static wxArrayString GetSnapValues();
   static const wxString & GetSnapValue(int index);
   static int GetSnapIndex(const wxString & value);

private:

   void Reinit();
   void CondListAdd(double t, const Track *track);
   double Get(size_t index);
   wxInt64 PixelDiff(double t, size_t index);
   size_t Find(double t, size_t i0, size_t i1);
   size_t Find(double t);
   bool SnapToPoints(Track *currentTrack, double t, bool rightEdge, double *outT);

private:

   const AudacityProject *mProject;
   TrackList *mTracks;
   const TrackClipArray *mClipExclusions;
   const TrackArray *mTrackExclusions;
   const ZoomInfo *mZoomInfo;
   int mPixelTolerance;
   bool mNoTimeSnap;
   
   double mEpsilon;
   SnapPointArray mSnapPoints;

   // Info for snap-to-time
   NumericConverter mConverter;
   bool mSnapToTime;

   int mSnapTo;
   double mRate;
   wxString mFormat;
};

#endif
