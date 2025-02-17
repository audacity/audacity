/**********************************************************************

Audacity: A Digital Audio Editor

EnvelopeHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_ENVELOPE_HANDLE__
#define __AUDACITY_ENVELOPE_HANDLE__

#include "../../UIHandle.h"

#include <vector>

class wxMouseEvent;
class wxMouseState;

class Channel;
class Envelope;
class EnvelopeEditor;
class ViewInfo;
class TimeTrack;
class WaveChannel;

class AUDACITY_DLL_API EnvelopeHandle final : public UIHandle
{
    EnvelopeHandle(const EnvelopeHandle&) = delete;
    EnvelopeHandle& operator=(const EnvelopeHandle&) = delete;

    static UIHandlePtr HitEnvelope(std::weak_ptr<EnvelopeHandle>& holder, const wxMouseState& state, const wxRect& rect,
                                   const AudacityProject* pProject, Envelope* envelope, std::weak_ptr<const Channel> wChannel,
                                   float zoomMin, float zoomMax, bool dB, float dBRange, bool timeTrack);

public:
    EnvelopeHandle(Envelope* pEnvelope, std::weak_ptr<const Channel> wChannel);

    EnvelopeHandle(EnvelopeHandle&&) = default;
    EnvelopeHandle& operator=(EnvelopeHandle&&) = default;

    virtual ~EnvelopeHandle();

    std::shared_ptr<const Track> FindTrack() const override;

    static UIHandlePtr HitAnywhere(std::weak_ptr<EnvelopeHandle>& holder, Envelope* envelope, std::weak_ptr<const Channel> wChannel,
                                   bool timeTrack);
    static UIHandlePtr TimeTrackHitTest(std::weak_ptr<EnvelopeHandle>& holder, const wxMouseState& state, const wxRect& rect,
                                        const AudacityProject* pProject, const std::shared_ptr<TimeTrack>& tt);
    static UIHandlePtr WaveChannelHitTest(std::weak_ptr<EnvelopeHandle>& holder, const wxMouseState& state, const wxRect& rect,
                                          const AudacityProject* pProject, const std::shared_ptr<WaveChannel>& wt);

    Envelope* GetEnvelope() const { return mEnvelope; }

    void Enter(bool forward, AudacityProject*) override;

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    HitTestPreview Preview(const TrackPanelMouseState& state, AudacityProject* pProject)
    override;

    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    Result Cancel(AudacityProject* pProject) override;

    bool StopsOnKeystroke() override { return true; }

private:
    bool ForwardEventToEnvelopes(const wxMouseEvent& event, const ViewInfo& viewInfo);

    wxRect mRect{};
    bool mLog{};
    float mLower{}, mUpper{};
    double mdBRange{};

    Envelope* mEnvelope{};
    std::weak_ptr<const Channel> mwChannel;
    std::unique_ptr<EnvelopeEditor> mpEnvelopeEditor;

    bool mTimeTrack{};
};

#endif
