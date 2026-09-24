/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <gmock/gmock.h>

#include "projectscene/iprojectviewstate.h"

namespace au::projectscene {
class ProjectViewStateMock : public IProjectViewState
{
public:
    MOCK_METHOD(muse::ValCh<int>, totalTrackHeight, (), (const, override));
    MOCK_METHOD(muse::ValCh<int>, trackHeight, (const trackedit::TrackId&), (const, override));
    MOCK_METHOD(muse::ValCh<bool>, isTrackCollapsed, (const trackedit::TrackId&), (const, override));
    MOCK_METHOD(muse::ValCh<double>, channelHeightRatio, (const trackedit::TrackId&), (const, override));

    MOCK_METHOD(int, trackVerticalPosition, (const trackedit::TrackId&), (const, override));
    MOCK_METHOD(void, changeTrackHeight, (const trackedit::TrackId&, int), (override));
    MOCK_METHOD(void, setTrackHeight, (const trackedit::TrackId&, int), (override));
    MOCK_METHOD(void, setChannelHeightRatio, (const trackedit::TrackId&, double), (override));
    MOCK_METHOD(trackedit::TrackId, trackAtPosition, (double), (const, override));
    MOCK_METHOD(trackedit::TrackIdList, tracksInRange, (double, double), (const, override));

    MOCK_METHOD(bool, isSnapEnabled, (), (const, override));
    MOCK_METHOD(void, setIsSnapEnabled, (bool), (override));

    MOCK_METHOD(SnapType, snapType, (), (const, override));
    MOCK_METHOD(void, setSnapType, (SnapType), (override));

    MOCK_METHOD(bool, isSnapTripletsEnabled, (), (const, override));
    MOCK_METHOD(void, setIsSnapTripletsEnabled, (bool), (override));

    MOCK_METHOD(void, setSnap, (const Snap&), (override));
    MOCK_METHOD(Snap, getSnap, (), (const, override));
    MOCK_METHOD(muse::ValCh<Snap>, snap, (), (const, override));

    MOCK_METHOD(void, setClipGainAutomationEnabled, (bool), (override));
    MOCK_METHOD(muse::ValCh<bool>, clipGainAutomationEnabled, (), (const, override));

    MOCK_METHOD(void, setSplitToolEnabled, (bool), (override));
    MOCK_METHOD(muse::ValCh<bool>, splitToolEnabled, (), (const, override));

    MOCK_METHOD((muse::ValCh<std::pair<float, float> >), verticalDisplayBounds, (const trackedit::TrackId&), (const, override));
    MOCK_METHOD(void, zoomInVertically, (const trackedit::TrackId&), (override));
    MOCK_METHOD(void, zoomOutVertically, (const trackedit::TrackId&), (override));
    MOCK_METHOD(void, resetVerticalZoom, (const trackedit::TrackId&), (override));
    MOCK_METHOD(bool, isDefaultVerticalZoom, (const trackedit::TrackId&), (const, override));
    MOCK_METHOD(bool, isMaxVerticalZoom, (const trackedit::TrackId&), (const, override));
    MOCK_METHOD(bool, isMinVerticalZoom, (const trackedit::TrackId&), (const, override));

    MOCK_METHOD(muse::ValCh<int>, verticalRulerWidth, (), (const, override));

    MOCK_METHOD(muse::ValCh<bool>, isHalfWave, (const trackedit::TrackId&), (const, override));
    MOCK_METHOD(void, toggleHalfWave, (const trackedit::TrackId&), (override));

    MOCK_METHOD(muse::ValCh<trackedit::TrackViewType>, trackViewType, (const trackedit::TrackId&), (const, override));
    MOCK_METHOD(void, setTrackViewType, (const trackedit::TrackId&, trackedit::TrackViewType), (override));
    MOCK_METHOD(void, toggleGlobalSpectrogramView, (), (override));
    MOCK_METHOD(bool, globalSpectrogramToggleIsOn, (), (const, override));
    MOCK_METHOD(muse::async::Notification, globalSpectrogramToggleIsOnChanged, (), (const, override));

    MOCK_METHOD(muse::ValCh<int>, trackRulerType, (const trackedit::TrackId&), (const, override));
    MOCK_METHOD(void, setTrackRulerType, (const trackedit::TrackId&, int), (override));

    MOCK_METHOD(double, mousePositionY, (), (const, override));
    MOCK_METHOD(void, setMousePositionY, (double), (override));

    MOCK_METHOD(muse::ValCh<int>, tracksVerticalOffset, (), (const, override));
    MOCK_METHOD(void, changeTracksVerticalOffset, (int), (override));
    MOCK_METHOD(muse::ValCh<bool>, tracksVerticalScrollLocked, (), (const, override));
    MOCK_METHOD(void, setTracksVerticalScrollLocked, (bool), (override));

    MOCK_METHOD(void, setItemEditStartTimeOffset, (double), (override));
    MOCK_METHOD(double, itemEditStartTimeOffset, (), (const, override));

    MOCK_METHOD(void, setItemEditEndTimeOffset, (double), (override));
    MOCK_METHOD(double, itemEditEndTimeOffset, (), (const, override));

    MOCK_METHOD(void, setMoveInitiated, (bool), (override));
    MOCK_METHOD(bool, moveInitiated, (), (const, override));

    MOCK_METHOD(void, setMovePreviewEndTime, (double), (override));
    MOCK_METHOD(double, movePreviewEndTime, (), (const, override));

    MOCK_METHOD(void, setLastEditedClip, (const trackedit::ClipKey&), (override));
    MOCK_METHOD(trackedit::ClipKey, lastEditedClip, (), (const, override));

    MOCK_METHOD(void, setEditedItem, (const trackedit::TrackItemKey&), (override));

    MOCK_METHOD(void, setItemsBoundaries, (const std::set<muse::secs_t>&), (override));
    MOCK_METHOD(std::set<muse::secs_t>, itemsBoundaries, (), (const, override));
    MOCK_METHOD(void, updateItemsBoundaries, (bool, const trackedit::TrackItemKey&), (override));

    MOCK_METHOD(void, setZoomState, (const ZoomState&), (override));
    MOCK_METHOD(ZoomState, zoomState, (), (const, override));

    MOCK_METHOD(muse::async::Notification, rolledBack, (), (const, override));

    MOCK_METHOD(muse::ValCh<bool>, altPressed, (), (const, override));
    MOCK_METHOD(muse::ValCh<bool>, ctrlPressed, (), (const, override));

    MOCK_METHOD(muse::ValCh<bool>, keyboardMoveActive, (), (const, override));
    MOCK_METHOD(void, setKeyboardMoveActive, (bool), (override));

    MOCK_METHOD(muse::async::Notification, modifiersReleased, (), (const, override));

    MOCK_METHOD(int, trackDefaultHeight, (), (const, override));
};
}
