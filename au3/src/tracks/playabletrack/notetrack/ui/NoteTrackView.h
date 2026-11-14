/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackView.h

Paul Licameli split from class NoteTrack

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_VIEW__
#define __AUDACITY_NOTE_TRACK_VIEW__

#include "../../../ui/CommonChannelView.h"

class NoteTrackView final : public CommonChannelView
{
    NoteTrackView(const NoteTrackView&) = delete;
    NoteTrackView& operator=(const NoteTrackView&) = delete;

public:
    explicit
    NoteTrackView(const std::shared_ptr<Channel>& pChannel);
    ~NoteTrackView() override;

private:
    std::shared_ptr<ChannelVRulerControls> DoGetVRulerControls() override;
    std::shared_ptr<CommonTrackCell> GetAffordanceControls() override;

    std::vector<UIHandlePtr> DetailedHitTest(const TrackPanelMouseState& state, const AudacityProject* pProject, int currentTool,
                                             bool bMultiTool)
    override;

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    std::shared_ptr<CommonTrackCell> mpAffordanceCellControl;

#ifdef EXPERIMENTAL_MIDI_STRETCHING
    std::weak_ptr<class StretchHandle> mStretchHandle;
#endif
};
#endif
