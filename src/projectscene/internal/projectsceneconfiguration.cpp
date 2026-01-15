/*
* Audacity: A Digital Audio Editor
*/
#include "settings.h"

#include "framework/global/stringutils.h"
#include "framework/global/serialization/json.h"

#include "framework/global/log.h"

#include "types/projectscenetypes.h"
#include "uicomponents/types/numerictypes.h"

#include "projectsceneconfiguration.h"

using namespace au::projectscene;

static const std::string moduleName("projectscene");

static const QString IS_VERTICAL_RULERS_VISIBLE("projectscene/verticalRulersVisible");
static constexpr bool DEFAULT_VERTICAL_RULERS_VISIBILITY = false;
static const QString IS_RMS_IN_WAVEFORM_VISIBLE("projectscene/rmsInWaveformVisible");
static constexpr bool DEFAULT_RMS_IN_WAVEFORM_VISIBILITY = false;
static const QString IS_CLIPPING_IN_WAVEFORM_VISIBLE("projectscene/clippingInWaveformVisible");
static constexpr bool DEFAULT_CLIPPING_IN_WAVEFORM_VISIBILITY = false;
static const QString TIMELINE_RULER_MODE("projectscene/timelineRulerMode");
static const QString EFFECTS_PANEL_VISIBILITY("projectscene/effectsPanelVisible");

static const muse::Settings::Key MOUSE_ZOOM_PRECISION(moduleName, "projectscene/zoomPrecisionMouse");
static const muse::Settings::Key CLIP_STYLE(moduleName, "projectscene/clipStyle");
static const muse::Settings::Key STEREO_HEIGHTS_PREF(moduleName, "projectscene/asymmetricStereoHeights");
static const muse::Settings::Key ASYMMETRIC_STEREO_HEIGHTS_WORKSPACES(moduleName, "projectscene/asymmetricStereoHeightsWorkspaces");
static const muse::Settings::Key SELECTION_TIMECODE_FORMAT(moduleName, "projectscene/selectionTimecodeFormat");
static const muse::Settings::Key PLAYBACK_ON_RULER_CLICK_ENABLED(moduleName, "projectscene/playbackOnRulerClickEnabled");
static const muse::Settings::Key LABEL_EDITOR_COLUMN_FORMAT(moduleName, "projectscene/labelEditorColumnFormat");

static const bool DEFAULT_PLAYBACK_ON_RULER_CLICK_ENABLED = false;

void ProjectSceneConfiguration::init()
{
    uiConfiguration()->isVisibleChanged(IS_VERTICAL_RULERS_VISIBLE).onNotify(nullptr, [this](){
        m_isVerticalRulersVisibleChanged.send(isVerticalRulersVisible());
    });

    uiConfiguration()->isVisibleChanged(IS_RMS_IN_WAVEFORM_VISIBLE).onNotify(nullptr, [this](){
        m_isRMSInWaveformVisibleChanged.send(isRMSInWaveformVisible());
    });

    uiConfiguration()->isVisibleChanged(IS_CLIPPING_IN_WAVEFORM_VISIBLE).onNotify(nullptr, [this](){
        m_isClippingInWaveformVisibleChanged.send(isClippingInWaveformVisible());
    });

    muse::settings()->setDefaultValue(MOUSE_ZOOM_PRECISION, muse::Val(6));

    muse::settings()->setDefaultValue(CLIP_STYLE, muse::Val(ClipStyles::Style::COLORFUL));
    muse::settings()->valueChanged(CLIP_STYLE).onReceive(nullptr, [this](const muse::Val& val) {
        m_clipStyleChanged.send(val.toEnum<ClipStyles::Style>());
    });

    muse::settings()->setDefaultValue(STEREO_HEIGHTS_PREF,
                                      muse::Val(StereoHeightsPref::AsymmetricStereoHeights::WORKSPACE_DEPENDENT));
    muse::settings()->valueChanged(STEREO_HEIGHTS_PREF).onReceive(nullptr, [this](const muse::Val&) {
        m_asymmetricStereoHeightsChanged.notify();
    });

    muse::settings()->setDefaultValue(ASYMMETRIC_STEREO_HEIGHTS_WORKSPACES,
                                      muse::Val("Modern"));
    muse::settings()->valueChanged(ASYMMETRIC_STEREO_HEIGHTS_WORKSPACES).onReceive(nullptr, [this](const muse::Val&) {
        m_asymmetricStereoHeightsWorkspacesChanged.notify();
    });

    muse::settings()->setDefaultValue(SELECTION_TIMECODE_FORMAT, muse::Val(au::uicomponents::TimecodeFormatType::HHMMSSHundredths));
    muse::settings()->valueChanged(SELECTION_TIMECODE_FORMAT).onReceive(nullptr, [this](const muse::Val& val) {
        UNUSED(val);
        m_selectionTimecodeFormatChanged.notify();
    });

    muse::settings()->setDefaultValue(PLAYBACK_ON_RULER_CLICK_ENABLED, muse::Val(DEFAULT_PLAYBACK_ON_RULER_CLICK_ENABLED));
    muse::settings()->valueChanged(PLAYBACK_ON_RULER_CLICK_ENABLED).onReceive(nullptr, [this](const muse::Val&) {
        m_playbackOnRulerClickEnabledChanged.notify();
    });
}

bool ProjectSceneConfiguration::isVerticalRulersVisible() const
{
    return uiConfiguration()->isVisible(IS_VERTICAL_RULERS_VISIBLE, DEFAULT_VERTICAL_RULERS_VISIBILITY);
}

void ProjectSceneConfiguration::setVerticalRulersVisible(bool visible)
{
    uiConfiguration()->setIsVisible(IS_VERTICAL_RULERS_VISIBLE, visible);
}

muse::async::Channel<bool> ProjectSceneConfiguration::isVerticalRulersVisibleChanged() const
{
    return m_isVerticalRulersVisibleChanged;
}

bool ProjectSceneConfiguration::isRMSInWaveformVisible() const
{
    return uiConfiguration()->isVisible(IS_RMS_IN_WAVEFORM_VISIBLE, DEFAULT_RMS_IN_WAVEFORM_VISIBILITY);
}

void ProjectSceneConfiguration::setRMSInWaveformVisible(bool visible)
{
    uiConfiguration()->setIsVisible(IS_RMS_IN_WAVEFORM_VISIBLE, visible);
}

muse::async::Channel<bool> ProjectSceneConfiguration::isRMSInWaveformVisibleChanged() const
{
    return m_isRMSInWaveformVisibleChanged;
}

bool ProjectSceneConfiguration::isClippingInWaveformVisible() const
{
    return uiConfiguration()->isVisible(IS_CLIPPING_IN_WAVEFORM_VISIBLE, DEFAULT_CLIPPING_IN_WAVEFORM_VISIBILITY);
}

void ProjectSceneConfiguration::setClippingInWaveformVisible(bool visible)
{
    uiConfiguration()->setIsVisible(IS_CLIPPING_IN_WAVEFORM_VISIBLE, visible);
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
        { "Blue", uiConfiguration()->currentTheme().extra["clip_color_1"].toString().toStdString() },
        { "Violet", uiConfiguration()->currentTheme().extra["clip_color_2"].toString().toStdString() },
        { "Magenta", uiConfiguration()->currentTheme().extra["clip_color_3"].toString().toStdString() },
        { "Red", uiConfiguration()->currentTheme().extra["clip_color_4"].toString().toStdString() },
        { "Orange", uiConfiguration()->currentTheme().extra["clip_color_5"].toString().toStdString() },
        { "Yellow", uiConfiguration()->currentTheme().extra["clip_color_6"].toString().toStdString() },
        { "Green", uiConfiguration()->currentTheme().extra["clip_color_7"].toString().toStdString() },
        { "Turquoise", uiConfiguration()->currentTheme().extra["clip_color_8"].toString().toStdString() },
        { "Cyan", uiConfiguration()->currentTheme().extra["clip_color_9"].toString().toStdString() }
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
    // "Classic|Music|Modern"
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

int ProjectSceneConfiguration::selectionTimecodeFormat() const
{
    return muse::settings()->value(SELECTION_TIMECODE_FORMAT).toInt();
}

void ProjectSceneConfiguration::setSelectionTimecodeFormat(int format)
{
    muse::settings()->setSharedValue(SELECTION_TIMECODE_FORMAT, muse::Val(format));
}

muse::async::Notification ProjectSceneConfiguration::selectionTimecodeFormatChanged() const
{
    return m_selectionTimecodeFormatChanged;
}

bool ProjectSceneConfiguration::playbackOnRulerClickEnabled() const
{
    return muse::settings()->value(PLAYBACK_ON_RULER_CLICK_ENABLED).toBool();
}

void ProjectSceneConfiguration::setPlaybackOnRulerClickEnabled(bool enabled)
{
    muse::settings()->setSharedValue(PLAYBACK_ON_RULER_CLICK_ENABLED, muse::Val(enabled));
}

muse::ByteArray ProjectSceneConfiguration::labelEditorColumnFormatJson() const
{
    return muse::ByteArray(muse::String::fromStdString(muse::settings()->value(
                                                           LABEL_EDITOR_COLUMN_FORMAT).toString()).toUtf8());
}

int ProjectSceneConfiguration::labelEditorColumnFormat(const std::string& columnName) const
{
    int result = -1;

    muse::ByteArray json = labelEditorColumnFormatJson();
    if (json.empty()) {
        return result;
    }

    std::string err;
    muse::JsonDocument jsodDoc = muse::JsonDocument::fromJson(json, &err);
    if (!err.empty()) {
        LOGE() << err;
        return result;
    }

    if (!jsodDoc.isObject()) {
        return result;
    }

    muse::JsonObject obj = jsodDoc.rootObject();
    if (obj.contains(columnName)) {
        result = obj.value(columnName).toInt();
    }

    return result;
}

void ProjectSceneConfiguration::setLabelEditorColumnFormat(const std::string& columnName, int format) const
{
    muse::ByteArray json = labelEditorColumnFormatJson();
    muse::JsonObject obj;
    if (json.empty()) {
        obj[columnName] = format;
    } else {
        obj = muse::JsonDocument::fromJson(json).rootObject();
        obj[columnName] = format;
    }

    muse::ByteArray newJson = muse::JsonDocument(obj).toJson();
    muse::settings()->setSharedValue(LABEL_EDITOR_COLUMN_FORMAT, muse::Val(muse::String::fromUtf8(newJson).toStdString()));
}

muse::async::Notification ProjectSceneConfiguration::playbackOnRulerClickEnabledChanged() const
{
    return m_playbackOnRulerClickEnabledChanged;
}
