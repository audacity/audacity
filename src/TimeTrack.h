/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeTrack.h

  Dr William Bland

**********************************************************************/

#ifndef __AUDACITY_TIMETRACK__
#define __AUDACITY_TIMETRACK__

#include "Track.h"
#include <wx/brush.h>
#include <wx/pen.h>

#include <algorithm>

class wxRect;
class wxDC;
class Envelope;
class Ruler;
class ZoomInfo;

class TimeTrack final : public Track {

 public:

   TimeTrack(const std::shared_ptr<DirManager> &projDirManager, const ZoomInfo *zoomInfo);
   /** @brief Copy-Constructor - create a NEW TimeTrack:: which is an independent copy of the original
    *
    * Calls TimeTrack::Init() to copy the track metadata, then does a bunch of manipulations on the
    * Envelope:: and Ruler:: members in order to copy one to the other - unfortunately both lack a
    * copy-constructor to encapsulate this.
    * @param orig The original track to copy from
    */
   TimeTrack(const TimeTrack &orig);

   virtual ~TimeTrack();

   // Identifying the type of track
   int GetKind() const override { return Time; }

   // TimeTrack parameters

   double GetOffset() const override { return 0.0; }
   void SetOffset(double /* t */) override {}

   double GetStartTime() const override { return 0.0; }
   double GetEndTime() const override { return 0.0; }

   void Draw(wxDC & dc, const wxRect & r, const ZoomInfo &zoomInfo) const;

   // XMLTagHandler callback methods for loading and saving

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   void HandleXMLEndTag(const wxChar *tag) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) override;

   // Lock and unlock the track: you must lock the track before
   // doing a copy and paste between projects.

   // bool Lock();
   // bool Unlock();

   // Access the track's speed envelope

   Envelope *GetEnvelope() { return mEnvelope.get(); }
   const Envelope *GetEnvelope() const { return mEnvelope.get(); }

   //Note: The meaning of this function has changed (December 2012)
   //Previously this function did something that was close to the opposite (but not entirely accurate).
   /** @brief Compute the integral warp factor between two non-warped time points
    *
    * Calculate the relative length increase of the chosen segment from the original sound.
    * So if this time track has a low value (i.e. makes the sound slower), the NEW warped
    * sound will be *longer* than the original sound, so the return value of this function
    * is larger.
    * @param t0 The starting time to calculate from
    * @param t1 The ending time to calculate to
    * @return The relative length increase of the chosen segment from the original sound.
    */
   double ComputeWarpFactor(double t0, double t1) const;
   /** @brief Compute the duration (in seconds at playback) of the specified region of the track.
    *
    * Takes a region of the time track (specified by the unwarped time points in the project), and
    * calculates how long it will actually take to play this region back, taking the time track's
    * warping effects into account.
    * @param t0 unwarped time to start calculation from
    * @param t1 unwarped time to stop calculation at
    * @return the warped duration in seconds
    */
   double ComputeWarpedLength(double t0, double t1) const;
   /** @brief Compute how much unwarped time must have elapsed if length seconds of warped time has
    * elapsed
    *
    * @param t0 The unwarped time (seconds from project start) at which to start
    * @param length How many seconds of warped time went past.
    * @return The end point (in seconds from project start) as unwarped time
    */
   double SolveWarpedLength(double t0, double length) const;

   // Get/Set the speed-warping range, as percentage of original speed (e.g. 90%-110%)

   double GetRangeLower() const { return mRangeLower; }
   double GetRangeUpper() const { return mRangeUpper; }

   void SetRangeLower(double lower) { mRangeLower = lower; }
   void SetRangeUpper(double upper) { mRangeUpper = upper; }

   bool GetDisplayLog() const { return mDisplayLog; }
   void SetDisplayLog(bool displayLog) { mDisplayLog = displayLog; }
   bool GetInterpolateLog() const;
   void SetInterpolateLog(bool interpolateLog);

   void testMe();

 private:
   const ZoomInfo  *const mZoomInfo;
   std::unique_ptr<Envelope> mEnvelope;
   std::unique_ptr<Ruler> mRuler;
   double           mRangeLower;
   double           mRangeUpper;
   bool             mDisplayLog;
   bool             mRescaleXMLValues; // needed for backward-compatibility with older project files

   /** @brief Copy the metadata from another track but not the points
    *
    * Copies the Name, DefaultName, Range and Display data from the source track
    * @param orig the TimeTrack to copy from
    */
   void Init(const TimeTrack &orig);

   using Holder = std::unique_ptr<TimeTrack>;
   Track::Holder Duplicate() const override;

   friend class TrackFactory;

   wxBrush blankBrush;
   wxPen blankPen;
};


#endif // __AUDACITY_TIMETRACK__

