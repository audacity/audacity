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
#include "ComponentInterfaceSymbol.h"

class AudacityProject;
class Track;
class TrackList;
class ZoomInfo;
class wxDC;

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

class SNAPPING_API SnapManager
{
public:
   //! Construct only for specified candidate points
   SnapManager(const AudacityProject &project,
               SnapPointArray candidates,
               const ZoomInfo &zoomInfo,
               bool noTimeSnap = false,
               int pixelTolerance = kPixelTolerance);

   //! Construct for (optionally) specified points, plus significant points
   //! on the tracks in the given list
   SnapManager(const AudacityProject &project,
               const TrackList &tracks,
               const ZoomInfo &zoomInfo,
               SnapPointArray candidates = {},
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
   const ZoomInfo *mZoomInfo;
   int mPixelTolerance;
   bool mNoTimeSnap;
   
   //! Two time points closer than this are considered the same
   double mEpsilon{ 1 / 44100.0 };
   SnapPointArray mCandidates;
   SnapPointArray mSnapPoints;

   // Info for snap-to-time
   bool mSnapToTime{ false };

   Identifier mSnapTo {};
   double mRate{ 0.0 };
   NumericFormatSymbol mFormat{};
};

#endif
