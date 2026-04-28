/*
* Audacity: A Digital Audio Editor
*/
#include "settings.h"

#include "framework/global/stringutils.h"
#include "framework/global/serialization/json.h"
#include "framework/ui/iuicontextconfiguration.h"

#include "framework/global/log.h"

#include "types/projectscenetypes.h"
#include "uicomponents/types/numerictypes.h"

#include "projectsceneconfiguration.h"

using namespace au::projectscene;

static const std::string moduleName("projectscene");

static const muse::Settings::Key IS_VERTICAL_RULERS_VISIBLE(moduleName, "projectscene/verticalRulersVisible");
static const muse::Settings::Key IS_RMS_IN_WAVEFORM_VISIBLE(moduleName, "projectscene/rmsInWaveformVisible");
static const muse::Settings::Key IS_CLIPPING_IN_WAVEFORM_VISIBLE(moduleName, "projectscene/clippingInWaveformVisible");
static const muse::Settings::Key TIMELINE_RULER_MODE(moduleName, "projectscene/timelineRulerMode");
static const muse::Settings::Key EFFECTS_PANEL_VISIBILITY(moduleName, "projectscene/effectsPanelVisible");

static const muse::Settings::Key MOUSE_ZOOM_PRECISION(moduleName, "projectscene/zoomPrecisionMouse");
static const muse::Settings::Key CLIP_STYLE(moduleName, "projectscene/clipStyle");
static const muse::Settings::Key STEREO_HEIGHTS_PREF(moduleName, "projectscene/asymmetricStereoHeights");
static const muse::Settings::Key ASYMMETRIC_STEREO_HEIGHTS_WORKSPACES(moduleName, "projectscene/asymmetricStereoHeightsWorkspaces");
static const muse::Settings::Key SELECTION_TIMECODE_FORMAT(moduleName, "projectscene/selectionTimecodeFormat");
static const muse::Settings::Key DURATION_TIMECODE_FORMAT(moduleName, "projectscene/durationTimecodeFormat");
static const muse::Settings::Key PLAYBACK_ON_RULER_CLICK_ENABLED(moduleName, "projectscene/playbackOnRulerClickEnabled");
static const muse::Settings::Key LABEL_EDITOR_COLUMN_FORMAT(moduleName, "projectscene/labelEditorColumnFormat");
static const muse::Settings::Key UPDATE_DISPLAY_WHILE_PLAYING_ENABLED(moduleName, "projectscene/updateDisplayWhilePlayingEnabled");
static const muse::Settings::Key PINNED_PLAY_HEAD_ENABLED(moduleName, "projectscene/pinnedPlayHeadEnabled");
static const muse::Settings::Key ZOOM_PRESET_1(moduleName, "projectscene/zoomPreset1");
static const muse::Settings::Key ZOOM_PRESET_2(moduleName, "projectscene/zoomPreset2");

static constexpr bool DEFAULT_VERTICAL_RULERS_VISIBILITY = false;
static constexpr bool DEFAULT_RMS_IN_WAVEFORM_VISIBILITY = false;
static constexpr bool DEFAULT_CLIPPING_IN_WAVEFORM_VISIBILITY = false;
static const bool DEFAULT_PLAYBACK_ON_RULER_CLICK_ENABLED = false;
static const bool DEFAULT_UPDATE_DISPLAY_WHILE_PLAYING_ENABLED = true;
static const bool DEFAULT_PINNED_PLAY_HEAD_ENABLED = false;

void ProjectSceneConfiguration::init()
{
    muse::settings()->setDefaultValue(IS_VERTICAL_RULERS_VISIBLE, muse::Val(DEFAULT_VERTICAL_RULERS_VISIBILITY));
    muse::settings()->valueChanged(IS_VERTICAL_RULERS_VISIBLE).onReceive(nullptr, [this](const muse::Val&) {
        m_isVerticalRulersVisibleChanged.send(isVerticalRulersVisible());
    });

    muse::settings()->setDefaultValue(IS_RMS_IN_WAVEFORM_VISIBLE, muse::Val(DEFAULT_RMS_IN_WAVEFORM_VISIBILITY));
    muse::settings()->valueChanged(IS_RMS_IN_WAVEFORM_VISIBLE).onReceive(nullptr, [this](const muse::Val&) {
        m_isRMSInWaveformVisibleChanged.send(isRMSInWaveformVisible());
    });

    muse::settings()->setDefaultValue(IS_CLIPPING_IN_WAVEFORM_VISIBLE, muse::Val(DEFAULT_CLIPPING_IN_WAVEFORM_VISIBILITY));
    muse::settings()->valueChanged(IS_CLIPPING_IN_WAVEFORM_VISIBLE).onReceive(nullptr, [this](const muse::Val&) {
        m_isClippingInWaveformVisibleChanged.send(isClippingInWaveformVisible());
    });

    muse::settings()->setDefaultValue(TIMELINE_RULER_MODE,
                                      muse::Val(static_cast<int>(TimelineRulerMode::MINUTES_AND_SECONDS)));
    muse::settings()->valueChanged(TIMELINE_RULER_MODE).onReceive(nullptr, [this](const muse::Val&) {
        m_timelineRulerModeChanged.notify();
    });

    muse::settings()->setDefaultValue(EFFECTS_PANEL_VISIBILITY, muse::Val(true));
    muse::settings()->valueChanged(EFFECTS_PANEL_VISIBILITY).onReceive(nullptr, [this](const muse::Val&) {
        m_effectsPanelVisible.notify();
    });

    muse::settings()->setDefaultValue(MOUSE_ZOOM_PRECISION, muse::Val(6));

    muse::settings()->setDefaultValue(CLIP_STYLE, muse::Val(ClipStyles::Style::COLORFUL));
    muse::settings()->valueChanged(CLIP_STYLE).onReceive(nullptr, [this](const muse::Val& val) {
        m_clipStyleChanged.send(val.toEnum<ClipStyles::Style>());
    });

    muse::settings()->setDefaultValue(STEREO_HEIGHTS_PREF,
                                      muse::Val(StereoHeightsPref::AsymmetricStereoHeights::NEVER));
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

    muse::settings()->setDefaultValue(DURATION_TIMECODE_FORMAT, muse::Val(au::uicomponents::TimecodeFormatType::HHMMSSHundredths));
    muse::settings()->valueChanged(DURATION_TIMECODE_FORMAT).onReceive(nullptr, [this](const muse::Val& val) {
        UNUSED(val);
        m_durationTimecodeFormatChanged.notify();
    });

    muse::settings()->setDefaultValue(PLAYBACK_ON_RULER_CLICK_ENABLED, muse::Val(DEFAULT_PLAYBACK_ON_RULER_CLICK_ENABLED));
    muse::settings()->valueChanged(PLAYBACK_ON_RULER_CLICK_ENABLED).onReceive(nullptr, [this](const muse::Val&) {
        m_playbackOnRulerClickEnabledChanged.notify();
    });

    muse::settings()->setDefaultValue(UPDATE_DISPLAY_WHILE_PLAYING_ENABLED, muse::Val(DEFAULT_UPDATE_DISPLAY_WHILE_PLAYING_ENABLED));
    muse::settings()->valueChanged(UPDATE_DISPLAY_WHILE_PLAYING_ENABLED).onReceive(nullptr, [this](const muse::Val&) {
        m_updateDisplayWhilePlayingEnabledChanged.notify();
    });

    muse::settings()->setDefaultValue(PINNED_PLAY_HEAD_ENABLED, muse::Val(DEFAULT_PINNED_PLAY_HEAD_ENABLED));
    muse::settings()->valueChanged(PINNED_PLAY_HEAD_ENABLED).onReceive(nullptr, [this](const muse::Val&) {
        m_pinnedPlayHeadEnabledChanged.notify();
    });

    muse::settings()->setDefaultValue(ZOOM_PRESET_1, muse::Val(static_cast<int>(ZoomPresets::ZoomDefault)));
    muse::settings()->setDefaultValue(ZOOM_PRESET_2, muse::Val(static_cast<int>(ZoomPresets::Zoom4To1)));
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

double ProjectSceneConfiguration::zoom(const muse::modularity::ContextPtr& ctx) const
{
    auto uiCtxConfig = muse::modularity::ioc(ctx)->resolve<muse::ui::IUiContextConfiguration>("projectscene");
    return uiCtxConfig ? uiCtxConfig->physicalDpi() / 4 : 96.0 / 4;
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
    return muse::settings()->value(TIMELINE_RULER_MODE)
           .toEnum<TimelineRulerMode>();
}

void ProjectSceneConfiguration::setTimelineRulerMode(const TimelineRulerMode mode)
{
    muse::settings()->setSharedValue(TIMELINE_RULER_MODE, muse::Val(static_cast<int>(mode)));
}

muse::async::Notification ProjectSceneConfiguration::timelineRulerModeChanged() const
{
    return m_timelineRulerModeChanged;
}

bool ProjectSceneConfiguration::isEffectsPanelVisible() const
{
    return muse::settings()->value(EFFECTS_PANEL_VISIBILITY).toBool();
}

void ProjectSceneConfiguration::setIsEffectsPanelVisible(bool visible)
{
    muse::settings()->setSharedValue(EFFECTS_PANEL_VISIBILITY, muse::Val(visible));
}

muse::async::Notification ProjectSceneConfiguration::isEffectsPanelVisibleChanged() const
{
    return m_effectsPanelVisible;
}

const std::vector<ClipColorInfo>& ProjectSceneConfiguration::clipColorInfos() const
{
    static const std::vector<ClipColorInfo> infos = {
        { "Blue", 1 },
        { "Violet", 2 },
        { "Magenta", 3 },
        { "Red", 4 },
        { "Orange", 5 },
        { "Yellow", 6 },
        { "Green", 7 },
        { "Turquoise", 8 },
        { "Cyan", 9 }
    };

    return infos;
}

muse::Color ProjectSceneConfiguration::clipColor(trackedit::ClipColorIndex index) const
{
    QString key = QString("clip_color_%1").arg(index);
    QColor color = uiConfiguration()->currentTheme().extra[key].value<QColor>();
    if (!color.isValid()) {
        color = uiConfiguration()->currentTheme().extra["clip_color_1"].value<QColor>();
    }
    return muse::Color::fromQColor(color);
}

muse::Color ProjectSceneConfiguration::clipSelectedColor(trackedit::ClipColorIndex index) const
{
    QString key = QString("clip_selected_color_%1").arg(index);
    QColor color = uiConfiguration()->currentTheme().extra[key].value<QColor>();
    if (!color.isValid()) {
        color = uiConfiguration()->currentTheme().extra["clip_selected_color_1"].value<QColor>();
    }
    return muse::Color::fromQColor(color);
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

int ProjectSceneConfiguration::durationTimecodeFormat() const
{
    return muse::settings()->value(DURATION_TIMECODE_FORMAT).toInt();
}

void ProjectSceneConfiguration::setDurationTimecodeFormat(int format)
{
    muse::settings()->setSharedValue(DURATION_TIMECODE_FORMAT, muse::Val(format));
}

muse::async::Notification ProjectSceneConfiguration::durationTimecodeFormatChanged() const
{
    return m_durationTimecodeFormatChanged;
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

bool ProjectSceneConfiguration::updateDisplayWhilePlayingEnabled() const
{
    return muse::settings()->value(UPDATE_DISPLAY_WHILE_PLAYING_ENABLED).toBool();
}

void ProjectSceneConfiguration::setUpdateDisplayWhilePlayingEnabled(bool enabled)
{
    muse::settings()->setSharedValue(UPDATE_DISPLAY_WHILE_PLAYING_ENABLED, muse::Val(enabled));
}

muse::async::Notification ProjectSceneConfiguration::updateDisplayWhilePlayingEnabledChanged() const
{
    return m_updateDisplayWhilePlayingEnabledChanged;
}

bool ProjectSceneConfiguration::pinnedPlayHeadEnabled() const
{
    return muse::settings()->value(PINNED_PLAY_HEAD_ENABLED).toBool();
}

void ProjectSceneConfiguration::setPinnedPlayHeadEnabled(bool enabled)
{
    muse::settings()->setSharedValue(PINNED_PLAY_HEAD_ENABLED, muse::Val(enabled));
}

muse::async::Notification ProjectSceneConfiguration::pinnedPlayHeadEnabledChanged() const
{
    return m_pinnedPlayHeadEnabledChanged;
}

ZoomPresets::Preset ProjectSceneConfiguration::zoomPreset1() const
{
    return muse::settings()->value(ZOOM_PRESET_1).toEnum<ZoomPresets::Preset>();
}

void ProjectSceneConfiguration::setZoomPreset1(ZoomPresets::Preset preset)
{
    muse::settings()->setSharedValue(ZOOM_PRESET_1, muse::Val(static_cast<int>(preset)));
}

ZoomPresets::Preset ProjectSceneConfiguration::zoomPreset2() const
{
    return muse::settings()->value(ZOOM_PRESET_2).toEnum<ZoomPresets::Preset>();
}

void ProjectSceneConfiguration::setZoomPreset2(ZoomPresets::Preset preset)
{
    muse::settings()->setSharedValue(ZOOM_PRESET_2, muse::Val(static_cast<int>(preset)));
}
