/**********************************************************************

Audacity: A Digital Audio Editor

SampleHandle.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_HANDLE__
#define __AUDACITY_SAMPLE_HANDLE__

#include "../../../../UIHandle.h"
#include "SampleCount.h"

class wxMouseEvent;
class wxMouseState;

class Track;
class ViewInfo;
class WaveChannel;
class WaveClipChannel;

class SampleHandle final : public UIHandle
{
    SampleHandle(const SampleHandle&) = delete;
    static HitTestPreview HitPreview(const wxMouseState& state, const AudacityProject* pProject, bool unsafe);

public:
    explicit SampleHandle(const std::shared_ptr<WaveChannel>& pTrack);

    SampleHandle& operator=(const SampleHandle&) = default;

    static UIHandlePtr HitAnywhere(
        std::weak_ptr<SampleHandle>& holder, const wxMouseState& state, const std::shared_ptr<WaveChannel>& pChannel);
    static UIHandlePtr HitTest(
        std::weak_ptr<SampleHandle>& holder, const wxMouseState& state, const wxRect& rect, const AudacityProject* pProject,
        const std::shared_ptr<WaveChannel>& pChannel);

    virtual ~SampleHandle();

    std::shared_ptr<const Track> FindTrack() const override;

    void Enter(bool forward, AudacityProject*) override;

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    HitTestPreview Preview(const TrackPanelMouseState& state, AudacityProject* pProject)
    override;

    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    Result Cancel(AudacityProject* pProject) override;

    bool StopsOnKeystroke() override { return true; }

private:
    float FindSampleEditingLevel(const wxMouseEvent& event, const ViewInfo& viewInfo, double t0);

    std::shared_ptr<WaveChannel> mClickedTrack;
    std::shared_ptr<WaveClipChannel> mClickedClip {};
    wxRect mRect{};

    int mClickedStartPixel {};
    int mLastDragPixel {};
    float mLastDragSampleValue{};
    bool mAltKey{};
};

#endif
