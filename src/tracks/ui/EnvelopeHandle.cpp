/**********************************************************************

Audacity: A Digital Audio Editor

EnvelopeHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "EnvelopeHandle.h"

#include "TrackView.h"

#include "Envelope.h"
#include "Decibels.h"
#include "../../EnvelopeEditor.h"
#include "../../HitTestResult.h"
#include "../../prefs/WaveformSettings.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "../../RefreshCode.h"
#include "TimeTrack.h"
#include "../../TrackArt.h"
#include "../../TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "../../../images/Cursors.h"

#include <wx/event.h>

EnvelopeHandle::EnvelopeHandle( Envelope *pEnvelope )
   : mEnvelope{ pEnvelope }
{
}

void EnvelopeHandle::Enter(bool, AudacityProject *)
{
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   mChangeHighlight = RefreshCode::RefreshCell;
#endif
}

EnvelopeHandle::~EnvelopeHandle()
{}

UIHandlePtr EnvelopeHandle::HitAnywhere
(std::weak_ptr<EnvelopeHandle> &holder, Envelope *envelope, bool timeTrack)
{
   auto result = AssignUIHandlePtr(holder, std::make_shared<EnvelopeHandle>(envelope));
   result->mTimeTrack = timeTrack;
   return result;
}

namespace {
   void GetTimeTrackData
      (const AudacityProject &project, const TimeTrack &tt,
       double &dBRange, bool &dB, float &zoomMin, float &zoomMax)
   {
      const auto &viewInfo = ViewInfo::Get( project );
      dBRange = DecibelScaleCutoff.Read();
      dB = tt.GetDisplayLog();
      zoomMin = tt.GetRangeLower(), zoomMax = tt.GetRangeUpper();
      if (dB) {
         // MB: silly way to undo the work of GetWaveYPos while still getting a logarithmic scale
         zoomMin = LINEAR_TO_DB(std::max(1.0e-7, double(zoomMin))) / dBRange + 1.0;
         zoomMax = LINEAR_TO_DB(std::max(1.0e-7, double(zoomMax))) / dBRange + 1.0;
      }
   }
}

UIHandlePtr EnvelopeHandle::TimeTrackHitTest
(std::weak_ptr<EnvelopeHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const AudacityProject *pProject, const std::shared_ptr<TimeTrack> &tt)
{
   auto envelope = tt->GetEnvelope();
   if (!envelope)
      return {};
   bool dB;
   double dBRange;
   float zoomMin, zoomMax;
   GetTimeTrackData( *pProject, *tt, dBRange, dB, zoomMin, zoomMax);
   return EnvelopeHandle::HitEnvelope
      (holder, state, rect, pProject, envelope, zoomMin, zoomMax, dB, dBRange,
       true);
}

UIHandlePtr EnvelopeHandle::WaveTrackHitTest
(std::weak_ptr<EnvelopeHandle> &holder,
 const wxMouseState &state, const wxRect &rect,
 const AudacityProject *pProject, const std::shared_ptr<WaveTrack> &wt)
{
   /// method that tells us if the mouse event landed on an
   /// envelope boundary.
   auto &viewInfo = ViewInfo::Get(*pProject);
   auto time = viewInfo.PositionToTime(state.m_x, rect.GetX());
   Envelope *const envelope = wt->GetEnvelopeAtTime(time);

   if (!envelope)
      return {};

   // Get envelope point, range 0.0 to 1.0
   const bool dB = !WaveformSettings::Get(*wt).isLinear();

   float zoomMin, zoomMax;
   auto &cache = WaveformScale::Get(*wt);
   cache.GetDisplayBounds(zoomMin, zoomMax);

   const float dBRange = WaveformSettings::Get(*wt).dBRange;

   return EnvelopeHandle::HitEnvelope
       (holder, state, rect, pProject, envelope, zoomMin, zoomMax, dB, dBRange, false);
}

UIHandlePtr EnvelopeHandle::HitEnvelope
(std::weak_ptr<EnvelopeHandle> &holder,
 const wxMouseState &state, const wxRect &rect, const AudacityProject *pProject,
 Envelope *envelope, float zoomMin, float zoomMax,
 bool dB, float dBRange, bool timeTrack)
{
   const auto &viewInfo = ViewInfo::Get( *pProject );

   const double envValue =
      envelope->GetValue(viewInfo.PositionToTime(state.m_x, rect.x));

   // Get y position of envelope point.
   int yValue = GetWaveYPos(envValue,
      zoomMin, zoomMax,
      rect.height, dB, true, dBRange, false) + rect.y;

   // Get y position of center line
   int ctr = GetWaveYPos(0.0,
      zoomMin, zoomMax,
      rect.height, dB, true, dBRange, false) + rect.y;

   // Get y distance of mouse from center line (in pixels).
   int yMouse = abs(ctr - state.m_y);
   // Get y distance of envelope from center line (in pixels)
   yValue = abs(ctr - yValue);

   // JKC: It happens that the envelope is actually drawn offset from its
   // 'true' position (it is 3 pixels wide).  yMisalign is really a fudge
   // factor to allow us to hit it exactly, but I wouldn't dream of
   // calling it yFudgeFactor :)
   const int yMisalign = 2;
   // Perhaps yTolerance should be put into preferences?
   const int yTolerance = 5; // how far from envelope we may be and count as a hit.
   int distance;

   // For amplification using the envelope we introduced the idea of contours.
   // The contours have the same shape as the envelope, which may be partially off-screen.
   // The contours are closer in to the center line.
   // Beware very short rectangles!  Make this at least 1
   int ContourSpacing = std::max(1,
      static_cast<int>(rect.height / (2 * (zoomMax - zoomMin))));
   const int MaxContours = 2;

   // Adding ContourSpacing/2 selects a region either side of the contour.
   int yDisplace = yValue - yMisalign - yMouse + ContourSpacing / 2;
   if (yDisplace > (MaxContours * ContourSpacing))
      return {};
   // Subtracting the ContourSpacing/2 we added earlier ensures distance is centred on the contour.
   distance = abs((yDisplace % ContourSpacing) - ContourSpacing / 2);
   if (distance >= yTolerance)
      return {};

   return HitAnywhere(holder, envelope, timeTrack);
}

UIHandle::Result EnvelopeHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   const bool unsafe = ProjectAudioIO::Get( *pProject ).IsAudioActive();
   if ( unsafe )
      return Cancelled;

   const wxMouseEvent &event = evt.event;
   const auto &viewInfo = ViewInfo::Get( *pProject );
   const auto pView = std::static_pointer_cast<TrackView>(evt.pCell);
   const auto pTrack = pView ? pView->FindTrack().get() : nullptr;

   mEnvelopeEditors.clear();

   unsigned result = Cancelled;
   if (pTrack)
      result = pTrack->TypeSwitch< decltype(RefreshNone) >(
      [&](WaveTrack *wt) {
         if (!mEnvelope)
            return Cancelled;

         mLog = !WaveformSettings::Get(*wt).isLinear();
         auto &cache = WaveformScale::Get(*wt);
         cache.GetDisplayBounds(mLower, mUpper);
         mdBRange = WaveformSettings::Get(*wt).dBRange;
         auto channels = TrackList::Channels( wt );
         for ( auto channel : channels ) {
            if (channel == wt)
               mEnvelopeEditors.push_back(
                  std::make_unique< EnvelopeEditor >( *mEnvelope, true ) );
            else {
               auto time =
                  viewInfo.PositionToTime(event.GetX(), evt.rect.GetX());
               auto e2 = channel->GetEnvelopeAtTime(time);
               if (e2)
                  mEnvelopeEditors.push_back(
                     std::make_unique< EnvelopeEditor >( *e2, true ) );
               else {
                   // There isn't necessarily an envelope there; no guarantee a
                   // linked track has the same WaveClip structure...
                }
            }
         }

         return RefreshNone;
      },
      [&](TimeTrack *tt) {
         if (!mEnvelope)
            return Cancelled;
         GetTimeTrackData( *pProject, *tt, mdBRange, mLog, mLower, mUpper);
         mEnvelopeEditors.push_back(
            std::make_unique< EnvelopeEditor >( *mEnvelope, false )
         );

         return RefreshNone;
      },
      [](Track *) {
         return Cancelled;
      }
   );

   if (result & Cancelled)
      return result;

   mRect = evt.rect;

   const bool needUpdate = ForwardEventToEnvelopes(event, viewInfo);
   return needUpdate ? RefreshCell : RefreshNone;
}

UIHandle::Result EnvelopeHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   const wxMouseEvent &event = evt.event;
   const auto &viewInfo = ViewInfo::Get( *pProject );
   const bool unsafe = ProjectAudioIO::Get( *pProject ).IsAudioActive();
   if (unsafe) {
      this->Cancel(pProject);
      return RefreshCell | Cancelled;
   }

   const bool needUpdate = ForwardEventToEnvelopes(event, viewInfo);
   return needUpdate ? RefreshCell : RefreshNone;
}

HitTestPreview EnvelopeHandle::Preview
(const TrackPanelMouseState &, AudacityProject *pProject)
{
   const bool unsafe = ProjectAudioIO::Get( *pProject ).IsAudioActive();
   static auto disabledCursor =
      ::MakeCursor(wxCURSOR_NO_ENTRY, DisabledCursorXpm, 16, 16);
   static auto envelopeCursor =
      ::MakeCursor(wxCURSOR_ARROW, EnvCursorXpm, 16, 16);

   auto message = mTimeTrack
      ? XO("Click and drag to warp playback time")
      : XO("Click and drag to edit the amplitude envelope");

   return {
      message,
      (unsafe
       ? &*disabledCursor
       : &*envelopeCursor)
   };
}

UIHandle::Result EnvelopeHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *)
{
   const wxMouseEvent &event = evt.event;
   const auto &viewInfo = ViewInfo::Get( *pProject );
   const bool unsafe = ProjectAudioIO::Get( *pProject ).IsAudioActive();
   if (unsafe)
      return this->Cancel(pProject);

   const bool needUpdate = ForwardEventToEnvelopes(event, viewInfo);

   ProjectHistory::Get( *pProject ).PushState(
      /* i18n-hint: (verb) Audacity has just adjusted the envelope .*/
      XO("Adjusted envelope."),
      /* i18n-hint: The envelope is a curve that controls the audio loudness.*/
      XO("Envelope")
   );

   mEnvelopeEditors.clear();

   using namespace RefreshCode;
   return needUpdate ? RefreshCell : RefreshNone;
}

UIHandle::Result EnvelopeHandle::Cancel(AudacityProject *pProject)
{
   ProjectHistory::Get( *pProject ).RollbackState();
   mEnvelopeEditors.clear();
   return RefreshCode::RefreshCell;
}

bool EnvelopeHandle::ForwardEventToEnvelopes
   (const wxMouseEvent &event, const ViewInfo &viewInfo)
{
   /// The Envelope class actually handles things at the mouse
   /// event level, so we have to forward the events over.  Envelope
   /// will then tell us whether or not we need to redraw.

   // AS: I'm not sure why we can't let the Envelope take care of
   //  redrawing itself.  ?
   bool needUpdate = false;
   for (const auto &pEditor : mEnvelopeEditors) {
      needUpdate =
         pEditor->MouseEvent(
            event, mRect, viewInfo, mLog, mdBRange, mLower, mUpper)
         || needUpdate;
   }

   return needUpdate;
}
