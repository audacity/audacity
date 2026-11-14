/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackButtonHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_BUTTON_HANDLE__
#define __AUDACITY_NOTE_TRACK_BUTTON_HANDLE__

class wxMouseState;
class NoteTrack;

#include "../../../../UIHandle.h"

///////////////////////////////////////////////////////////////////////////////
// TODO: do appearance changes as in ButtonHandle, or even, inherit from that
class NoteTrackButtonHandle : public UIHandle
{
    NoteTrackButtonHandle(const NoteTrackButtonHandle&);
    NoteTrackButtonHandle();
    static NoteTrackButtonHandle& Instance();

public:
    explicit NoteTrackButtonHandle(const std::shared_ptr<NoteTrack>& pTrack, int channel, const wxRect& rect);

    NoteTrackButtonHandle& operator=(const NoteTrackButtonHandle&) = default;

    virtual ~NoteTrackButtonHandle();

    static UIHandlePtr HitTest(std::weak_ptr<NoteTrackButtonHandle>& holder, const wxMouseState& state, const wxRect& rect,
                               const std::shared_ptr<NoteTrack>& pTrack);

    std::shared_ptr<NoteTrack> GetTrack() const { return mpTrack.lock(); }
    std::shared_ptr<const Track> FindTrack() const override;
    int GetChannel() const { return mChannel; }

    static UIHandle::Result NeedChangeHighlight(const NoteTrackButtonHandle& oldState, const NoteTrackButtonHandle& newState);

protected:
    void Enter(bool forward, AudacityProject*) override;

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    HitTestPreview Preview(const TrackPanelMouseState& state, AudacityProject* pProject)
    override;

    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    Result Cancel(AudacityProject* pProject) override;

    std::weak_ptr<NoteTrack> mpTrack;
    int mChannel{ -1 };
    wxRect mRect{};
};

#endif
