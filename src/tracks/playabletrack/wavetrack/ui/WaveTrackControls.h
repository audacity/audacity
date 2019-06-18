/**********************************************************************

Audacity: A Digital Audio Editor

WavelTrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_CONTROLS__
#define __AUDACITY_WAVE_TRACK_CONTROLS__

#include "../../../ui/CommonTrackControls.h" // to inherit

class CellularPanel;
class LWSlider;
class MuteButtonHandle;
class SoloButtonHandle;
class GainSliderHandle;
class PanSliderHandle;
class WaveTrack;
class wxEvent;
class wxWindow;

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

   static unsigned DefaultWaveTrackHeight();
   static void GetGainRect(const wxPoint & topLeft, wxRect &dest);
   static void GetPanRect(const wxPoint & topLeft, wxRect &dest);

   static LWSlider *GainSlider( CellularPanel &panel, const WaveTrack &wt );
   static LWSlider * GainSlider
      (const wxRect &sliderRect, const WaveTrack *t, bool captured,
       wxWindow *pParent);

   static LWSlider *PanSlider( CellularPanel &panel, const WaveTrack &wt );
   static LWSlider * PanSlider
      (const wxRect &sliderRect, const WaveTrack *t, bool captured,
       wxWindow *pParent);

   static void ReCreateSliders();

private:
   static void ReCreatePanSlider( wxEvent& );
   static void ReCreateGainSlider( wxEvent& );

   std::weak_ptr<MuteButtonHandle> mMuteHandle;
   std::weak_ptr<SoloButtonHandle> mSoloHandle;
   std::weak_ptr<GainSliderHandle> mGainHandle;
   std::weak_ptr<PanSliderHandle> mPanHandle;
};

#endif
