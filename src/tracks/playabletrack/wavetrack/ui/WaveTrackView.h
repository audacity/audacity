/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackView.h

Paul Licameli split from class WaveTrack

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VIEW__
#define __AUDACITY_WAVE_TRACK_VIEW__

#include "../../../ui/CommonTrackView.h"

class CutlineHandle;
class SampleHandle;
class EnvelopeHandle;

class WaveTrackView final : public CommonTrackView
{
   WaveTrackView( const WaveTrackView& ) = delete;
   WaveTrackView &operator=( const WaveTrackView& ) = delete;

public:
   explicit
   WaveTrackView( const std::shared_ptr<Track> &pTrack );
   ~WaveTrackView() override;

   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;


private:
   std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject, int currentTool, bool bMultiTool)
      override;

   std::weak_ptr<CutlineHandle> mCutlineHandle;
   std::weak_ptr<SampleHandle> mSampleHandle;
   std::weak_ptr<EnvelopeHandle> mEnvelopeHandle;

protected:
   void DoSetMinimized( bool minimized ) override;
};

#endif
