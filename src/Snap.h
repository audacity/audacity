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
#include <wx/string.h>
#include "widgets/NumericTextCtrl.h"

class AudacityProject;
class Track;
class TrackArray;
class TrackClipArray;
class WaveClip;
class TrackList;
class ZoomInfo;

class TrackClip
{
public:
   TrackClip(Track *t, WaveClip *c)
   {
      track = origTrack = t;
      clip = c;
   }
   Track *track;
   Track *origTrack;
   WaveClip *clip;
};

WX_DECLARE_USER_EXPORTED_OBJARRAY(TrackClip, TrackClipArray, AUDACITY_DLL_API);

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
   SnapPoint(double t, Track *track)
   {
      this->t = t;
      this->track = track;
   }
   double t;
   Track *track;
};

WX_DEFINE_SORTED_ARRAY(SnapPoint *, SnapPointArray);

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
   void CondListAdd(double t, Track *track);
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
