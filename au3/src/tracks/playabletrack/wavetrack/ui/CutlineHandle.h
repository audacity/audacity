/**********************************************************************

Audacity: A Digital Audio Editor

CutlineHandle.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_CUTLINE_HANDLE__
#define __AUDACITY_CUTLINE_HANDLE__

#include "../../../../UIHandle.h"
#include "../../../../WaveTrackLocation.h"

class wxMouseEvent;
class wxMouseState;
class WaveTrack;

class CutlineHandle final : public UIHandle
{
    CutlineHandle(const CutlineHandle&) = delete;
    static HitTestPreview HitPreview(bool unsafe);

public:
    explicit CutlineHandle(const std::shared_ptr<WaveTrack>& pTrack, WaveTrackLocations locations, WaveTrackLocation location);

    CutlineHandle& operator=(const CutlineHandle&) = default;

    static UIHandlePtr HitAnywhere(
        const AudacityProject* pProject, bool cutline, UIHandlePtr ptr);
    static UIHandlePtr HitTest(
        std::weak_ptr<CutlineHandle>& holder, const wxMouseState& state, const wxRect& rect, const AudacityProject* pProject,
        std::shared_ptr<WaveTrack> pTrack);

    virtual ~CutlineHandle();

    std::shared_ptr<const Track> FindTrack() const override;

    const WaveTrackLocation& GetLocation() { return mLocation; }
    std::shared_ptr<WaveTrack> GetTrack() { return mpTrack; }

    void Enter(bool forward, AudacityProject*) override;

    bool HandlesRightClick() override;

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    HitTestPreview Preview(const TrackPanelMouseState& state, AudacityProject* pProject)
    override;

    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    Result Cancel(AudacityProject* pProject) override;

    bool StopsOnKeystroke() override { return true; }

private:
    std::shared_ptr<WaveTrack> mpTrack{};
    enum Operation {
        Expand, Remove
    };
    Operation mOperation{ Expand };
    double mStartTime{}, mEndTime{};
    WaveTrackLocations mLocations;
    WaveTrackLocation mLocation{};
};

#endif
