/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeTrack.h

  Dr William Bland

**********************************************************************/

#ifndef __AUDACITY_TIMETRACK__
#define __AUDACITY_TIMETRACK__

#include "Track.h"

#include <algorithm>

class wxRect;
class BoundedEnvelope;
class Ruler;
class ZoomInfo;
struct TrackPanelDrawingContext;

class AUDACITY_DLL_API TimeTrack final : public Track {

 public:

   explicit TimeTrack(const ZoomInfo *zoomInfo);
   /** @brief Copy-Constructor - create a NEW TimeTrack:: which is an independent copy of the original
    *
    * Calls TimeTrack::Init() to copy the track metadata, then does a bunch of manipulations on the
    * Envelope:: and Ruler:: members in order to copy one to the other - unfortunately both lack a
    * copy-constructor to encapsulate this.
    * @param orig The original track to copy from
    * @param pT0 if not null, then the start of the sub-range to copy
    * @param pT1 if not null, then the end of the sub-range to copy
    */
   TimeTrack(const TimeTrack &orig, double *pT0 = nullptr, double *pT1 = nullptr);

   virtual ~TimeTrack();


   bool SupportsBasicEditing() const override;

   Holder PasteInto( AudacityProject & ) const override;

   Holder Cut( double t0, double t1 ) override;
   Holder Copy( double t0, double t1, bool forClipboard ) const override;
   void Clear(double t0, double t1) override;
   void Paste(double t, const Track * src) override;
   void Silence(double t0, double t1) override;
   void InsertSilence(double t, double len) override;

   // TimeTrack parameters

   double GetOffset() const override { return 0.0; }
   void SetOffset(double /* t */) override {}

   double GetStartTime() const override { return 0.0; }
   double GetEndTime() const override { return 0.0; }

   // XMLTagHandler callback methods for loading and saving

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   void HandleXMLEndTag(const wxChar *tag) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) const override;

   // Lock and unlock the track: you must lock the track before
   // doing a copy and paste between projects.

   // bool Lock();
   // bool Unlock();

   // Access the track's speed envelope

   BoundedEnvelope *GetEnvelope() { return mEnvelope.get(); }
   const BoundedEnvelope *GetEnvelope() const { return mEnvelope.get(); }

   // Get/Set the speed-warping range, as percentage of original speed (e.g. 90%-110%)

   double GetRangeLower() const;
   double GetRangeUpper() const;

   void SetRangeLower(double lower);
   void SetRangeUpper(double upper);

   bool GetDisplayLog() const { return mDisplayLog; }
   void SetDisplayLog(bool displayLog) { mDisplayLog = displayLog; }
   bool GetInterpolateLog() const;
   void SetInterpolateLog(bool interpolateLog);

   void testMe();

   Ruler &GetRuler() const { return *mRuler; }

 private:
   void CleanState();

   // Identifying the type of track
   TrackKind GetKind() const override { return TrackKind::Time; }

   const ZoomInfo  *const mZoomInfo;
   std::unique_ptr<BoundedEnvelope> mEnvelope;
   std::unique_ptr<Ruler> mRuler;
   bool             mDisplayLog;
   bool             mRescaleXMLValues; // needed for backward-compatibility with older project files

   /** @brief Copy the metadata from another track but not the points
    *
    * Copies the Name, DefaultName, Range and Display data from the source track
    * @param orig the TimeTrack to copy from
    */
   void Init(const TimeTrack &orig);

   using Holder = std::unique_ptr<TimeTrack>;

private:
   Track::Holder Clone() const override;
};


#endif // __AUDACITY_TIMETRACK__

