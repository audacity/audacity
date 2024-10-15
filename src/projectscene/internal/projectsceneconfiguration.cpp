/*
* Audacity: A Digital Audio Editor
*/
#include "projectsceneconfiguration.h"
#include "types/projectscenetypes.h"

#include "settings.h"

using namespace au::projectscene;

static const std::string moduleName("projectscene");

static const muse::Settings::Key IS_VERTICAL_RULERS_VISIBLE(moduleName, "projectscene/verticalRulersVisible");
static const muse::Settings::Key TIMELINE_RULER_MODE(moduleName, "projectscene/timelineRulerMode");
static const muse::Settings::Key MOUSE_ZOOM_PRECISION(moduleName, "projectscene/zoomPrecisionMouse");

void ProjectSceneConfiguration::init()
{
    muse::settings()->setDefaultValue(IS_VERTICAL_RULERS_VISIBLE, muse::Val(false));
    muse::settings()->valueChanged(IS_VERTICAL_RULERS_VISIBLE).onReceive(nullptr, [this](const muse::Val& val) {
        m_isVerticalRulersVisibleChanged.send(val.toBool());
    });

    muse::settings()->setDefaultValue(MOUSE_ZOOM_PRECISION, muse::Val(6));

    muse::settings()->setDefaultValue(TIMELINE_RULER_MODE, muse::Val(TimelineRulerMode::MINUTES_AND_SECONDS));
    muse::settings()->valueChanged(TIMELINE_RULER_MODE).onReceive(nullptr, [this](const muse::Val& val) {
        m_timelineRulerModeChanged.send(val.toEnum<TimelineRulerMode>());
    });
}

bool ProjectSceneConfiguration::isVerticalRulersVisible() const
{
    return muse::settings()->value(IS_VERTICAL_RULERS_VISIBLE).toBool();
}

void ProjectSceneConfiguration::setVerticalRulersVisible(bool visible)
{
    muse::settings()->setSharedValue(IS_VERTICAL_RULERS_VISIBLE, muse::Val(visible));
}

muse::async::Channel<bool> ProjectSceneConfiguration::isVerticalRulersVisibleChanged() const
{
    return m_isVerticalRulersVisibleChanged;
}

double ProjectSceneConfiguration::zoom() const
{
    return uiConfiguration()->physicalDpi() / 4;
}

int ProjectSceneConfiguration::mouseZoomPrecision() const
{
    return muse::settings()->value(MOUSE_ZOOM_PRECISION).toInt();
}

void ProjectSceneConfiguration::setMouseZoomPrecision(int precision)
{
    muse::settings()->setSharedValue(MOUSE_ZOOM_PRECISION, muse::Val(precision));
}

TimelineRulerMode ProjectSceneConfiguration::timelineRulerMode() const
{
    return muse::settings()->value(TIMELINE_RULER_MODE).toEnum<TimelineRulerMode>();
}

void ProjectSceneConfiguration::setTimelineRulerMode(const TimelineRulerMode mode)
{
    muse::settings()->setSharedValue(TIMELINE_RULER_MODE, muse::Val(mode));
}

muse::async::Channel<TimelineRulerMode> ProjectSceneConfiguration::timelineRulerModeChanged() const
{
    return m_timelineRulerModeChanged;
}
