/**********************************************************************

Audacity: A Digital Audio Editor

WavelTrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_CONTROLS__
#define __AUDACITY_WAVE_TRACK_CONTROLS__

#include "../../../ui/CommonTrackControls.h" // to inherit

class MuteButtonHandle;
class SoloButtonHandle;
class GainSliderHandle;
class PanSliderHandle;

class WaveTrackControls final : public CommonTrackControls
{
   WaveTrackControls(const WaveTrackControls&) = delete;
   WaveTrackControls &operator=(const WaveTrackControls&) = delete;

public:
   explicit
   WaveTrackControls( std::shared_ptr<Track> pTrack )
      : CommonTrackControls( pTrack ) {}
   ~WaveTrackControls();

   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;

   PopupMenuTable *GetMenuExtension(Track *pTrack) override;

   const TCPLines& GetTCPLines() const override;

private:
   std::weak_ptr<MuteButtonHandle> mMuteHandle;
   std::weak_ptr<SoloButtonHandle> mSoloHandle;
   std::weak_ptr<GainSliderHandle> mGainHandle;
   std::weak_ptr<PanSliderHandle> mPanHandle;
};

#endif
