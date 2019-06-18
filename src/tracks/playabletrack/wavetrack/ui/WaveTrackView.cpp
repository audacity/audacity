/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "WaveTrackView.h"
#include "../../../../WaveTrack.h"

#include "WaveTrackControls.h"
#include "WaveTrackVRulerControls.h"

#include "WaveTrackVRulerControls.h"
#include "../../../../HitTestResult.h"
#include "../../../../TrackPanelMouseEvent.h"

#include "CutlineHandle.h"
#include "../../../ui/SelectHandle.h"
#include "../../../ui/EnvelopeHandle.h"
#include "SampleHandle.h"
#include "../../../ui/TimeShiftHandle.h"
#include "../../../../ProjectSettings.h"

WaveTrackView::~WaveTrackView()
{
}

std::vector<UIHandlePtr> WaveTrackView::DetailedHitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject, int currentTool, bool bMultiTool)
{
   // This is the only override of Track::DetailedHitTest that still
   // depends on the state of the Tools toolbar.
   // If that toolbar were eliminated, this could simplify to a sequence of
   // hit test routines describable by a table.

   UIHandlePtr result;
   std::vector<UIHandlePtr> results;
   const auto pTrack = std::static_pointer_cast< WaveTrack >( FindTrack() );
   bool isWaveform = (pTrack->GetDisplay() == WaveTrack::Waveform);

   if (bMultiTool && st.state.CmdDown()) {
      // Ctrl modifier key in multi-tool overrides everything else
      // (But this does not do the time shift constrained to the vertical only,
      //  which is what happens when you hold Ctrl in the Time Shift tool mode)
      result = TimeShiftHandle::HitAnywhere(
         mTimeShiftHandle, pTrack, false);
      if (result)
         results.push_back(result);
      return results;
   }

   // Some special targets are not drawn in spectrogram,
   // so don't hit them in such views.
   else if (isWaveform) {

      if (NULL != (result = CutlineHandle::HitTest(
         mCutlineHandle, st.state, st.rect,
         pProject, pTrack )))
         // This overriding test applies in all tools
         results.push_back(result);
      if (bMultiTool) {
         // Conditional hit tests
         // If Tools toolbar were eliminated, we would keep these
         // The priority of these, in case more than one might apply at one
         // point, seems arbitrary
         if (NULL != (result = EnvelopeHandle::WaveTrackHitTest(
            mEnvelopeHandle, st.state, st.rect,
            pProject, pTrack )))
            results.push_back(result);
         if (NULL != (result = TimeShiftHandle::HitTest(
            mTimeShiftHandle, st.state, st.rect, pTrack )))
            // This is the hit test on the "grips" drawn left and
            // right in Multi only
            results.push_back(result);
         if (NULL != (result = SampleHandle::HitTest(
            mSampleHandle, st.state, st.rect,
            pProject, pTrack )))
            results.push_back(result);
      }
      else {
         switch ( currentTool ) {
               // Unconditional hits appropriate to the tool
               // If tools toolbar were eliminated, we would eliminate these
            case ToolCodes::envelopeTool: {
               auto envelope = pTrack->GetEnvelopeAtX( st.state.m_x );
               result = EnvelopeHandle::HitAnywhere(
                  mEnvelopeHandle, envelope, false);
               break;
            }
            case ToolCodes::drawTool:
               result = SampleHandle::HitAnywhere(
                  mSampleHandle, st.state, pTrack );
               break;
            default:
               result = {};
               break;
         }
         if (result)
            results.push_back(result);
      }
   }

   return results;
}

std::shared_ptr<TrackView> WaveTrack::DoGetView()
{
   return std::make_shared<WaveTrackView>( SharedPointer() );
}

std::shared_ptr<TrackControls> WaveTrack::DoGetControls()
{
   return std::make_shared<WaveTrackControls>( SharedPointer() );
}

std::shared_ptr<TrackVRulerControls> WaveTrackView::DoGetVRulerControls()
{
   return
      std::make_shared<WaveTrackVRulerControls>( shared_from_this() );
}
