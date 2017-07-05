/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackUI.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../WaveTrack.h"
#include "WaveTrackControls.h"
#include "WaveTrackVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "../../../../Project.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../toolbars/ToolsToolBar.h"

#include "CutlineHandle.h"
#include "../../../ui/SelectHandle.h"
#include "../../../ui/EnvelopeHandle.h"
#include "SampleHandle.h"
#include "../../../ui/TimeShiftHandle.h"

HitTestResult WaveTrack::DetailedHitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject, int currentTool, bool bMultiTool)
{
   // This is the only override of Track::DetailedHitTest that still
   // depends on the state of the Tools toolbar.
   // If that toolbar were eliminated, this could simplify to a sequence of
   // hit test routines describable by a table.

   const auto wavetrack = static_cast<WaveTrack*>(st.pCell.get());
   bool isWaveform = (wavetrack->GetDisplay() == WaveTrack::Waveform);

   if (bMultiTool && st.state.CmdDown())
      // Ctrl modifier key in multi-tool overrides everything else
      // (But this does not do the time shift constrained to the vertical only,
      //  which is what happens when you hold Ctrl in the Time Shift tool mode)
      return TimeShiftHandle::HitAnywhere(
         mTimeShiftHandle, pProject, Pointer(this), false);

   // Some special targets are not drawn in spectrogram,
   // so don't hit them in such views.
   else if (isWaveform) {
      HitTestResult result;
      if (NULL != (result = CutlineHandle::HitTest(
         mCutlineHandle, st.state, st.rect,
         pProject, Pointer<WaveTrack>(this))).preview.cursor)
         // This overriding test applies in all tools
         return result;
      else if (bMultiTool) {
         // Conditional hit tests
         // If Tools toolbar were eliminated, we would keep these
         // The priority of these, in case more than one might apply at one
         // point, seems arbitrary
         if (NULL != (result = EnvelopeHandle::WaveTrackHitTest(
            mEnvelopeHandle, st.state, st.rect,
            pProject, Pointer<WaveTrack>(this)))
            .preview.cursor)
            ;
         else if (NULL != (result = TimeShiftHandle::HitTest(
            mTimeShiftHandle, st.state, st.rect,
            pProject, Pointer(this))).preview.cursor)
            // This is the hit test on the "grips" drawn left and
            // right in Multi only
            ;
         else if (NULL != (result = SampleHandle::HitTest(
            mSampleHandle, st.state, st.rect,
            pProject, Pointer<WaveTrack>(this))).preview.cursor)
            ;
         return result;
      }
      else switch ( currentTool ) {
         // Unconditional hits appropriate to the tool
         // If tools toolbar were eliminated, we would eliminate these
         case envelopeTool: {
            auto envelope = GetEnvelopeAtX( st.state.m_x );
            return EnvelopeHandle::HitAnywhere(
               mEnvelopeHandle, pProject, envelope);
         }
         case drawTool:
            return SampleHandle::HitAnywhere(
               mSampleHandle, st.state, pProject, Pointer<WaveTrack>(this));
         default:
            break;
      }
   }

   return {};
}

std::shared_ptr<TrackControls> WaveTrack::GetControls()
{
   return std::make_shared<WaveTrackControls>( Pointer( this ) );
}

std::shared_ptr<TrackVRulerControls> WaveTrack::GetVRulerControls()
{
   return std::make_shared<WaveTrackVRulerControls>( Pointer( this ) );
}
