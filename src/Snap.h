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
#include "widgets/NumericTextCtrl.h" // member variable

class AudacityProject;
class Track;
using TrackArray = std::vector< Track* >;
class TrackClipArray;
class WaveClip;
class WaveTrack;
class TrackList;
class ZoomInfo;
class wxDC;

class TrackClip
{
public:
   TrackClip(Track *t, WaveClip *c);

   ~TrackClip();

   Track *track;
   Track *origTrack;
   WaveClip *clip;

   // These fields are used only during time-shift dragging
   WaveTrack *dstTrack;
   std::shared_ptr<WaveClip> holder;
};

class TrackClipArray : public std::vector < TrackClip > {};

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

struct SnapResults {
   double timeSnappedTime{ 0.0 };
   double outTime{ 0.0 };
   wxInt64 outCoord{ -1 };
   bool snappedPoint{ false };
   bool snappedTime{ false };

   bool Snapped() const { return snappedPoint || snappedTime; }
};

class SnapManager
{
public:
   SnapManager(const TrackList *tracks,
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
   SnapResults Snap(Track *currentTrack,
             double t,
             bool rightEdge);

   static const TranslatableStrings &GetSnapLabels();

   // The two coordinates need not be ordered:
   static void Draw( wxDC *dc, wxInt64 snap0, wxInt64 snap1 );

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
   const TrackList *mTracks;
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
   NumericFormatSymbol mFormat;
};

#endif
