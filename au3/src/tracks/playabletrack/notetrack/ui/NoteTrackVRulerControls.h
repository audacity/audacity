/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_VRULER_CONTROLS__
#define __AUDACITY_NOTE_TRACK_VRULER_CONTROLS__

#include "../../../ui/ChannelVRulerControls.h"

class NoteTrack;
class NoteTrackVZoomHandle;

class NoteTrackVRulerControls final : public ChannelVRulerControls
{
    NoteTrackVRulerControls(const NoteTrackVRulerControls&) = delete;
    NoteTrackVRulerControls& operator=(const NoteTrackVRulerControls&) = delete;

public:
    explicit
    NoteTrackVRulerControls(const std::shared_ptr<ChannelView>& pChannelView)
        : ChannelVRulerControls{pChannelView} {}
    ~NoteTrackVRulerControls();

    std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject) override;

    unsigned HandleWheelRotation(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

private:
    std::shared_ptr<NoteTrack> FindNoteTrack();

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    // ChannelVRulerControls implementation
    void UpdateRuler(const wxRect& rect) override;

    std::weak_ptr<NoteTrackVZoomHandle> mVZoomHandle;
};

#endif
