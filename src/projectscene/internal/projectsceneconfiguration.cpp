/*
* Audacity: A Digital Audio Editor
*/

#include "settings.h"

#include "global/stringutils.h"

#include "NumericConverterFormats.h"

#include "types/projectscenetypes.h"

#include "projectsceneconfiguration.h"

using namespace au::projectscene;

static const std::string moduleName("projectscene");

static const muse::Settings::Key IS_VERTICAL_RULERS_VISIBLE(moduleName, "projectscene/verticalRulersVisible");
static const muse::Settings::Key IS_RMS_IN_WAVEFORM_VISIBLE(moduleName, "projectscene/rmsInWaveformVisible");
static const muse::Settings::Key IS_CLIPPING_IN_WAVEFORM_VISIBLE(moduleName, "projectscene/clippingInWaveformVisible");
static const muse::Settings::Key MOUSE_ZOOM_PRECISION(moduleName, "projectscene/zoomPrecisionMouse");
static const muse::Settings::Key CLIP_STYLE(moduleName, "projectscene/clipStyle");
static const muse::Settings::Key STEREO_HEIGHTS_PREF(moduleName, "projectscene/asymmetricStereoHeights");
static const muse::Settings::Key ASYMMETRIC_STEREO_HEIGHTS_WORKSPACES(moduleName, "projectscene/asymmetricStereoHeightsWorkspaces");

static const QString TIMELINE_RULER_MODE("projectscene/timelineRulerMode");
static const QString EFFECTS_PANEL_VISIBILITY("projectscene/effectsPanelVisible");

void ProjectSceneConfiguration::init()
{
    muse::settings()->setDefaultValue(IS_VERTICAL_RULERS_VISIBLE, muse::Val(false));
    muse::settings()->valueChanged(IS_VERTICAL_RULERS_VISIBLE).onReceive(nullptr, [this](const muse::Val& val) {
        m_isVerticalRulersVisibleChanged.send(val.toBool());
    });

    muse::settings()->setDefaultValue(IS_RMS_IN_WAVEFORM_VISIBLE, muse::Val(false));
    muse::settings()->valueChanged(IS_RMS_IN_WAVEFORM_VISIBLE).onReceive(nullptr, [this](const muse::Val& val) {
        m_isRMSInWaveformVisibleChanged.send(val.toBool());
    });

    muse::settings()->setDefaultValue(IS_CLIPPING_IN_WAVEFORM_VISIBLE, muse::Val(false));
    muse::settings()->valueChanged(IS_CLIPPING_IN_WAVEFORM_VISIBLE).onReceive(nullptr, [this](const muse::Val& val) {
        m_isClippingInWaveformVisibleChanged.send(val.toBool());
    });

    muse::settings()->setDefaultValue(MOUSE_ZOOM_PRECISION, muse::Val(6));

    muse::settings()->setDefaultValue(CLIP_STYLE, muse::Val(ClipStyles::Style::COLORFUL));
    muse::settings()->valueChanged(CLIP_STYLE).onReceive(nullptr, [this](const muse::Val& val) {
        m_clipStyleChanged.send(val.toEnum<ClipStyles::Style>());
    });

    muse::settings()->setDefaultValue(STEREO_HEIGHTS_PREF,
                                      muse::Val(StereoHeightsPref::AsymmetricStereoHeights::WORKSPACE_DEPENDENT));
    muse::settings()->valueChanged(STEREO_HEIGHTS_PREF).onReceive(nullptr, [this](const muse::Val& val) {
        m_asymmetricStereoHeightsChanged.notify();
    });

    muse::settings()->setDefaultValue(ASYMMETRIC_STEREO_HEIGHTS_WORKSPACES,
                                      muse::Val("Advanced audio editing"));
    muse::settings()->valueChanged(ASYMMETRIC_STEREO_HEIGHTS_WORKSPACES).onReceive(nullptr, [this](const muse::Val& val) {
        m_asymmetricStereoHeightsWorkspacesChanged.notify();
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

bool ProjectSceneConfiguration::isRMSInWaveformVisible() const
{
    return muse::settings()->value(IS_RMS_IN_WAVEFORM_VISIBLE).toBool();
}

void ProjectSceneConfiguration::setRMSInWaveformVisible(bool visible)
{
    muse::settings()->setSharedValue(IS_RMS_IN_WAVEFORM_VISIBLE, muse::Val(visible));
}

muse::async::Channel<bool> ProjectSceneConfiguration::isRMSInWaveformVisibleChanged() const
{
    return m_isRMSInWaveformVisibleChanged;
}

bool ProjectSceneConfiguration::isClippingInWaveformVisible() const
{
    return muse::settings()->value(IS_CLIPPING_IN_WAVEFORM_VISIBLE).toBool();
}

void ProjectSceneConfiguration::setClippingInWaveformVisible(bool visible)
{
    muse::settings()->setSharedValue(IS_CLIPPING_IN_WAVEFORM_VISIBLE, muse::Val(visible));
}

muse::async::Channel<bool> ProjectSceneConfiguration::isClippingInWaveformVisibleChanged() const
{
    return m_isClippingInWaveformVisibleChanged;
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
    TimelineRulerMode result = TimelineRulerMode::MINUTES_AND_SECONDS;

    QString modeStr = uiConfiguration()->uiItemState(TIMELINE_RULER_MODE);
    if (!modeStr.isEmpty()) {
        result = static_cast<TimelineRulerMode>(modeStr.toInt());
    }

    return result;
}

void ProjectSceneConfiguration::setTimelineRulerMode(const TimelineRulerMode mode)
{
    uiConfiguration()->setUiItemState(TIMELINE_RULER_MODE, QString::number(static_cast<int>(mode)));
}

muse::async::Notification ProjectSceneConfiguration::timelineRulerModeChanged() const
{
    return uiConfiguration()->uiItemStateChanged(TIMELINE_RULER_MODE);
}

bool ProjectSceneConfiguration::isEffectsPanelVisible() const
{
    return uiConfiguration()->isVisible(EFFECTS_PANEL_VISIBILITY);
}

void ProjectSceneConfiguration::setIsEffectsPanelVisible(bool visible)
{
    uiConfiguration()->setIsVisible(EFFECTS_PANEL_VISIBILITY, visible);
}

muse::async::Notification ProjectSceneConfiguration::isEffectsPanelVisibleChanged() const
{
    return uiConfiguration()->isVisibleChanged(EFFECTS_PANEL_VISIBILITY);
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

ClipStyles::Style ProjectSceneConfiguration::clipStyle() const
{
    return muse::settings()->value(CLIP_STYLE).toEnum<ClipStyles::Style>();
}

void ProjectSceneConfiguration::setClipStyle(ClipStyles::Style style)
{
    muse::settings()->setSharedValue(CLIP_STYLE, muse::Val(style));
}

muse::async::Channel<ClipStyles::Style> ProjectSceneConfiguration::clipStyleChanged() const
{
    return m_clipStyleChanged;
}

StereoHeightsPref::AsymmetricStereoHeights
ProjectSceneConfiguration::stereoHeightsPref() const
{
    return muse::settings()->value(STEREO_HEIGHTS_PREF).toEnum<StereoHeightsPref::AsymmetricStereoHeights>();
}

void ProjectSceneConfiguration::setStereoHeightsPref(
    StereoHeightsPref::AsymmetricStereoHeights pref)
{
    if (stereoHeightsPref() == pref) {
        return;
    }
    muse::settings()->setSharedValue(STEREO_HEIGHTS_PREF, muse::Val(pref));
}

muse::async::Notification ProjectSceneConfiguration::stereoHeightsPrefChanged() const
{
    return m_asymmetricStereoHeightsChanged;
}

std::vector<std::string> ProjectSceneConfiguration::asymmetricStereoHeightsWorkspaces() const
{
    // workspaces that have asymmetricStereoHeights enabled are stored as a string in muse::settings
    // "Classic|Music|Advanced audio editing"
    std::vector<std::string> result;
    std::string combinedString = muse::settings()->value(ASYMMETRIC_STEREO_HEIGHTS_WORKSPACES).toString();

    muse::strings::split(combinedString, result, "|");

    return result;
}

void ProjectSceneConfiguration::setAsymmetricStereoHeightsWorkspaces(std::vector<std::string>& asymmetricWorkspaces)
{
    if (asymmetricStereoHeightsWorkspaces() == asymmetricWorkspaces) {
        return;
    }

    // convert vector to single string saveable in muse::settings
    std::string result = muse::strings::join(asymmetricWorkspaces, "|");

    muse::settings()->setSharedValue(ASYMMETRIC_STEREO_HEIGHTS_WORKSPACES, muse::Val(result));
}

muse::async::Notification ProjectSceneConfiguration::asymmetricStereoHeightsWorkspacesChanged() const
{
    return m_asymmetricStereoHeightsWorkspacesChanged;
}
