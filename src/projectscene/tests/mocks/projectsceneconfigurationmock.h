/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <gmock/gmock.h>

#include "projectscene/iprojectsceneconfiguration.h"

namespace au::projectscene {
class ProjectSceneConfigurationMock : public projectscene::IProjectSceneConfiguration
{
public:
    MOCK_METHOD(bool, isVerticalRulersVisible, (), (const, override));
    MOCK_METHOD(void, setVerticalRulersVisible, (bool visible), (override));
    MOCK_METHOD(muse::async::Channel<bool>, isVerticalRulersVisibleChanged, (), (const, override));

    MOCK_METHOD(bool, isRMSInWaveformVisible, (), (const, override));
    MOCK_METHOD(void, setRMSInWaveformVisible, (bool visible), (override));
    MOCK_METHOD(muse::async::Channel<bool>, isRMSInWaveformVisibleChanged, (), (const, override));

    MOCK_METHOD(bool, isClippingInWaveformVisible, (), (const, override));
    MOCK_METHOD(void, setClippingInWaveformVisible, (bool visible), (override));
    MOCK_METHOD(muse::async::Channel<bool>, isClippingInWaveformVisibleChanged, (), (const, override));

    MOCK_METHOD(double, zoom, (), (const, override));

    MOCK_METHOD(int, mouseZoomPrecision, (), (const, override));
    MOCK_METHOD(void, setMouseZoomPrecision, (int precision), (override));

    MOCK_METHOD(TimelineRulerMode, timelineRulerMode, (), (const, override));
    MOCK_METHOD(void, setTimelineRulerMode, (const TimelineRulerMode mode), (override));
    MOCK_METHOD(muse::async::Notification, timelineRulerModeChanged, (), (const, override));

    MOCK_METHOD(bool, isEffectsPanelVisible, (), (const, override));
    MOCK_METHOD(void, setIsEffectsPanelVisible, (bool visible), (override));
    MOCK_METHOD(muse::async::Notification, isEffectsPanelVisibleChanged, (), (const, override));

    MOCK_METHOD((const std::vector<std::pair<std::string, std::string> >&), clipColors, (), (const, override));

    MOCK_METHOD(ClipStyles::Style, clipStyle, (), (const, override));
    MOCK_METHOD(void, setClipStyle, (ClipStyles::Style style), (override));
    MOCK_METHOD(muse::async::Channel<ClipStyles::Style>, clipStyleChanged, (), (const, override));

    MOCK_METHOD(StereoHeightsPref::AsymmetricStereoHeights,  stereoHeightsPref, (), (const, override));
    MOCK_METHOD(void, setStereoHeightsPref, (StereoHeightsPref::AsymmetricStereoHeights pref), (override));
    MOCK_METHOD(muse::async::Notification, stereoHeightsPrefChanged, (), (const, override));

    MOCK_METHOD(std::vector<std::string>, asymmetricStereoHeightsWorkspaces, (), (const, override));
    MOCK_METHOD(void, setAsymmetricStereoHeightsWorkspaces, (std::vector<std::string>& workspaces), (override));
    MOCK_METHOD(muse::async::Notification, asymmetricStereoHeightsWorkspacesChanged, (), (const, override));
};
}
