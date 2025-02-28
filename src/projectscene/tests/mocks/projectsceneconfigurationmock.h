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

    MOCK_METHOD(double, zoom, (), (const, override));

    MOCK_METHOD(trackedit::secs_t, insertSilenceDuration, (), (const, override));
    MOCK_METHOD(void, setInsertSilenceDuration, (const trackedit::secs_t duration), (override));

    MOCK_METHOD(std::string, insertSilenceDurationFormat, (), (const, override));
    MOCK_METHOD(void, setInsertSilenceDurationFormat, (const std::string& format), (override));

    MOCK_METHOD(int, mouseZoomPrecision, (), (const, override));
    MOCK_METHOD(void, setMouseZoomPrecision, (int precision), (override));

    MOCK_METHOD(TimelineRulerMode, timelineRulerMode, (), (const, override));
    MOCK_METHOD(void, setTimelineRulerMode, (const TimelineRulerMode mode), (override));
    MOCK_METHOD(muse::async::Channel<TimelineRulerMode>, timelineRulerModeChanged, (), (const, override));

    MOCK_METHOD(muse::ValCh<bool>, isEffectsPanelVisible, (), (const, override));
    MOCK_METHOD(void, setIsEffectsPanelVisible, (bool visible), (override));

    MOCK_METHOD((const std::vector<std::pair<std::string, std::string> >&), clipColors, (), (const, override));
};
}
