/*
* Audacity: A Digital Audio Editor
*/
#include "projectsceneconfiguration.h"
#include "NumericConverterFormats.h"
#include "types/projectscenetypes.h"

#include "settings.h"

using namespace au::projectscene;

static const std::string moduleName("projectscene");

static const muse::Settings::Key IS_VERTICAL_RULERS_VISIBLE(moduleName, "projectscene/verticalRulersVisible");
static const muse::Settings::Key TIMELINE_RULER_MODE(moduleName, "projectscene/timelineRulerMode");
static const muse::Settings::Key MOUSE_ZOOM_PRECISION(moduleName, "projectscene/zoomPrecisionMouse");
static const muse::Settings::Key INSERT_SILENCE_DURATION(moduleName, "projectscene/insertSilenceDuration");
static const muse::Settings::Key INSERT_SILENCE_DURATION_FORMAT(moduleName, "projectscene/insertSilenceDurationFormat");

void ProjectSceneConfiguration::init()
{
    muse::settings()->setDefaultValue(IS_VERTICAL_RULERS_VISIBLE, muse::Val(false));
    muse::settings()->valueChanged(IS_VERTICAL_RULERS_VISIBLE).onReceive(nullptr, [this](const muse::Val& val) {
        m_isVerticalRulersVisibleChanged.send(val.toBool());
    });

    muse::settings()->setDefaultValue(MOUSE_ZOOM_PRECISION, muse::Val(6));

    muse::settings()->setDefaultValue(INSERT_SILENCE_DURATION, muse::Val(30));
    muse::settings()->setDefaultValue(INSERT_SILENCE_DURATION_FORMAT,
                                      muse::Val(NumericConverterFormats::DefaultSelectionFormat().Translation().ToStdString()));

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

au::trackedit::secs_t ProjectSceneConfiguration::insertSilenceDuration() const
{
    return muse::settings()->value(INSERT_SILENCE_DURATION).toDouble();
}

void ProjectSceneConfiguration::setInsertSilenceDuration(const trackedit::secs_t duration)
{
    muse::settings()->setSharedValue(INSERT_SILENCE_DURATION, muse::Val(duration));
}

std::string ProjectSceneConfiguration::insertSilenceDurationFormat() const
{
    return muse::settings()->value(INSERT_SILENCE_DURATION_FORMAT).toString();
}

void ProjectSceneConfiguration::setInsertSilenceDurationFormat(const std::string& format)
{
    muse::settings()->setSharedValue(INSERT_SILENCE_DURATION_FORMAT, muse::Val(format));
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

muse::ValCh<bool> ProjectSceneConfiguration::isEffectsPanelVisible() const
{
    return m_effectsPanelVisible;
}

void ProjectSceneConfiguration::setIsEffectsPanelVisible(bool visible)
{
    if (m_effectsPanelVisible.val == visible) {
        return;
    }
    m_effectsPanelVisible.set(visible);
}

const std::vector<std::pair<std::string, std::string> >& ProjectSceneConfiguration::clipColors() const
{
    static std::vector<std::pair<std::string /*name*/, std::string /*color*/> > colors = {
        { "Blue", "#66A3FF" },
        { "Violet", "#9996FC" },
        { "Magenta", "#DA8CCC" },
        { "Red", "#F08080" },
        { "Orange", "#FF9E65" },
        { "Yellow", "#E8C050" },
        { "Green", "#74BE59" },
        { "Turquoise", "#34B494" },
        { "Cyan", "#48BECF" }
    };

    return colors;
}
