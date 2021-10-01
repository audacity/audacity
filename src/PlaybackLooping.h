/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 PlaybackLooping.h

 Anton Gerasimov

 **********************************************************************/

#pragma once

#include <wx/dc.h>

#include "ViewInfo.h"
#include "TrackPanelCell.h"
#include "UIHandle.h"

//! Playback looping service (processor).
class AUDACITY_DLL_API PlaybackLooping
{
public:
    PlaybackLooping();

    static PlaybackLooping& GetInstance();

    bool mLoopingEnabled{ false };
    bool IsLoopingEnabled() const;
    void EnableLooping(bool enable);

    enum class ProcessingStates
    {
        kIdle,
        kEnterLooping,
        kLooping,
        kDragging
    };
    ProcessingStates mProcessingState;

    enum class ProcessingEvents
    {
        kNone,
        kClick,
        kDrag,
        kRelease,
        kPreview,
        kCancel
    };

    bool LoopingProcessor(AudacityProject* pProject, wxCoord& mousePosX, ProcessingEvents processingEvents);

    bool IsProcessing() const;

    PlayRegion mOldPlayRegion;
    PlayRegion mLoopedPlayRegion;
    //PlayRegion GetLoopedPlayRegion();

    void UpdateLoopingRange();
};

//! Handling user events like mouse clicking, etc.
class AUDACITY_DLL_API PlaybackLoopingHandle : public UIHandle
{
public:
    PlaybackLoopingHandle();

    PlaybackLoopingHandle(const PlaybackLoopingHandle& other) = default;
    PlaybackLoopingHandle(PlaybackLoopingHandle&& other) = default;

    PlaybackLoopingHandle& operator = (const PlaybackLoopingHandle& other) = default;
    PlaybackLoopingHandle& operator = (PlaybackLoopingHandle&& other) = default;

    Result Click
    (const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Drag
    (const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    HitTestPreview Preview
    (const TrackPanelMouseState& state, AudacityProject* pProject) override;

    Result Release
    (const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    Result Cancel(AudacityProject* pProject) override;
};

//! Describe GUI cell.
class AUDACITY_DLL_API PlaybackLoopingCell : public TrackPanelCell
{
public:
    PlaybackLoopingCell();

    void DrawLoopingRange(wxDC* dc, ViewInfo& viewInfo);

    wxRect mTimeBarBoundaries;

    std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject) override;

    std::weak_ptr<PlaybackLoopingHandle> mUiHandlerPointer;
};
