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

class TimeTrack: public Track {

 public:

   TimeTrack(DirManager * projDirManager);
   /** @brief Copy-Constructor - create a new TimeTrack:: which is an independent copy of the original
    *
    * Calls TimeTrack::Init() to copy the track metadata, then does a bunch of manipulations on the
    * Envelope:: and Ruler:: members in order to copy one to the other - unfortunately both lack a
    * copy-constructor to encapsulate this.
    * @param orig The original track to copy from
    */
   TimeTrack(TimeTrack &orig);

   virtual ~TimeTrack();

   // Identifying the type of track
   virtual int GetKind() const { return Time; }

   // TimeTrack parameters

   virtual double GetOffset() { return 0.0; };
   virtual void SetOffset(double /* t */) {};

   virtual double GetStartTime() { return 0.0; };
   virtual double GetEndTime() { return 0.0; };

   void Draw(wxDC & dc, const wxRect & r, double h, double pps);

   // XMLTagHandler callback methods for loading and saving

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual void HandleXMLEndTag(const wxChar *tag);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   virtual void WriteXML(XMLWriter &xmlFile);

   // Lock and unlock the track: you must lock the track before
   // doing a copy and paste between projects.

   bool Lock();
   bool Unlock();

   // Access the track's speed envelope

   Envelope *GetEnvelope() { return mEnvelope; }

   //Note: The meaning of this function has changed (December 2012)
   //Previously this function did something that was close to the opposite (but not entirely accurate).
   /** @brief Compute the integral warp factor between two non-warped time points
    *
    * Calculate the relative length increase of the chosen segment from the original sound.
    * So if this time track has a low value (i.e. makes the sound slower), the new warped
    * sound will be *longer* than the original sound, so the return value of this function
    * is larger.
    * @param t0 The starting time to calculate from
    * @param t1 The ending time to calculate to
    * @return The relative length increase of the chosen segment from the original sound.
    */
   double ComputeWarpFactor(double t0, double t1);
   /** @brief Compute the duration (in seconds at playback) of the specified region of the track.
    *
    * Takes a region of the time track (specified by the unwarped time points in the project), and
    * calculates how long it will actually take to play this region back, taking the time track's
    * warping effects into account.
    * @param t0 unwarped time to start calculation from
    * @param t1 unwarped time to stop calculation at
    * @return the warped duration in seconds
    */
   double ComputeWarpedLength(double t0, double t1);
   /** @brief Compute how much unwarped time must have elapsed if length seconds of warped time has
    * elapsed
    *
    * @param t0 The unwarped time (seconds from project start) at which to start
    * @param length How many seconds of warped time went past.
    * @return The end point (in seconds from project start) as unwarped time
    */
   double SolveWarpedLength(double t0, double length);

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
   Envelope        *mEnvelope;
   Ruler           *mRuler;
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
   virtual Track *Duplicate();

   friend class TrackFactory;

   wxBrush blankBrush;
   wxPen blankPen;
};


#endif // __AUDACITY_TIMETRACK__

