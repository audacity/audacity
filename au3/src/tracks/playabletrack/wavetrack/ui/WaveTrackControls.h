/**********************************************************************

Audacity: A Digital Audio Editor

WavelTrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_CONTROLS__
#define __AUDACITY_WAVE_TRACK_CONTROLS__

#include "../../ui/PlayableTrackControls.h" // to inherit
#include "Observer.h"

class CellularPanel;
class LWSlider;
class MuteButtonHandle;
class SoloButtonHandle;
class EffectsButtonHandle;
class VolumeSliderHandle;
class PanSliderHandle;
class WaveTrack;
class wxEvent;
class wxWindow;

class AUDACITY_DLL_API WaveTrackControls final : public PlayableTrackControls
{
    WaveTrackControls(const WaveTrackControls&) = delete;
    WaveTrackControls& operator=(const WaveTrackControls&) = delete;

public:
    explicit
    WaveTrackControls(std::shared_ptr<Track> pTrack)
        : PlayableTrackControls(pTrack) {}
    ~WaveTrackControls() override;

    std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject) override;

    PopupMenuTable* GetMenuExtension(Track* pTrack) override;

    const TCPLines& GetTCPLines() const override;

    static unsigned DefaultWaveTrackHeight();
    static void GetVolumeRect(const wxRect& rect, wxRect& dest);
    static void GetPanRect(const wxRect& rect, wxRect& dest);

    static LWSlider* VolumeSlider(CellularPanel& panel, const WaveTrack& wt);
    static LWSlider* VolumeSlider(const wxRect& sliderRect, const WaveTrack* t, bool captured, wxWindow* pParent);

    static LWSlider* PanSlider(CellularPanel& panel, const WaveTrack& wt);
    static LWSlider* PanSlider(const wxRect& sliderRect, const WaveTrack* t, bool captured, wxWindow* pParent);

    static void ReCreateSliders();

private:
    static void ReCreatePanSlider(struct ThemeChangeMessage);
    static void ReCreateVolumeSlider(struct ThemeChangeMessage);

    std::weak_ptr<MuteButtonHandle> mMuteHandle;
    std::weak_ptr<SoloButtonHandle> mSoloHandle;
    std::weak_ptr<EffectsButtonHandle> mEffectsHandle;
    std::weak_ptr<VolumeSliderHandle> mVolumeHandle;
    std::weak_ptr<PanSliderHandle> mPanHandle;
};

#include "../../../../widgets/PopupMenuTable.h"

struct AUDACITY_DLL_API WaveTrackPopupMenuTable : public PopupMenuTable
{
    using PopupMenuTable::PopupMenuTable;
    PlayableTrackControls::InitMenuData* mpData{};
    WaveTrack& FindWaveTrack() const;
    int ReserveId() { return mNextId++; }
protected:
    int mNextId = 0;
};

// Expose the wave track menu table to registration of menu items
AUDACITY_DLL_API
WaveTrackPopupMenuTable& GetWaveTrackMenuTable();

#endif
