/**********************************************************************

Audacity: A Digital Audio Editor

StretchHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_STRETCH_HANDLE__
#define __AUDACITY_STRETCH_HANDLE__

#include "../../../../UIHandle.h"

class Alg_seq;
class Channel;
class NoteTrack;
class Track;
class ViewInfo;

class StretchHandle : public UIHandle
{
public:
    enum StretchEnum {
        stretchNone = 0, // false value!
        stretchLeft,
        stretchCenter,
        stretchRight
    };

    // Stretching applies to a selected region after quantizing the
    // region to beat boundaries (subbeat stretching is not supported,
    // but maybe it should be enabled with shift or ctrl or something)
    // Stretching can drag the left boundary (the right stays fixed),
    // the right boundary (the left stays fixed), or the center (splits
    // the selection into two parts: when left part grows, the right
    // part shrinks, keeping the leftmost and rightmost boundaries
    // fixed.
    struct StretchState {
        StretchEnum mMode { stretchCenter }; // remembers what to drag

        using QuantizedTimeAndBeat = std::pair< double, double >;

        QuantizedTimeAndBeat mBeatCenter { 0, 0 };
        QuantizedTimeAndBeat mBeat0 { 0, 0 };
        QuantizedTimeAndBeat mBeat1 { 0, 0 };
        double mLeftBeats {}; // how many beats from left to cursor
        double mRightBeats {}; // how many beats from cursor to right

        double mOrigSel0Quantized { -1 }, mOrigSel1Quantized { -1 };
    };

private:
    StretchHandle(const StretchHandle&);
    static HitTestPreview HitPreview(StretchEnum stretchMode, bool unsafe);

public:
    explicit StretchHandle(const std::shared_ptr<NoteTrack>& pTrack, const StretchState& stretchState);

    StretchHandle& operator=(const StretchHandle&) = default;

    static UIHandlePtr HitTest(std::weak_ptr<StretchHandle>& holder, const TrackPanelMouseState& state, const AudacityProject* pProject,
                               const std::shared_ptr<NoteTrack>& pTrack);

    virtual ~StretchHandle();

    std::shared_ptr<const Track> FindTrack() const override;

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    HitTestPreview Preview(const TrackPanelMouseState& state, AudacityProject* pProject)
    override;

    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    Result Cancel(AudacityProject* pProject) override;

    bool StopsOnKeystroke() override { return true; }

private:
    static double GetT0(const Track& track, const ViewInfo& viewInfo);
    static double GetT1(const Track& track, const ViewInfo& viewInfo);

    void Stretch(AudacityProject* pProject, int mouseXCoordinate, int trackLeftEdge, Channel* pChannel);

    std::shared_ptr<NoteTrack> mpTrack{};
    int mLeftEdge{ -1 };

    StretchState mStretchState{};
};

#endif
