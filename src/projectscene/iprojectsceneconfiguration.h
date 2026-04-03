#pragma once

#include <QColor>

#include "async/channel.h"
#include "async/notification.h"

#include "modularity/imoduleinterface.h"
#include "modularity/ioc.h"

#include "trackedit/trackedittypes.h"
#include "types/projectscenetypes.h"

namespace au::projectscene {
struct ClipColorInfo {
    std::string name;
    trackedit::ClipColorIndex index = 0;
};
class IProjectSceneConfiguration : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IProjectSceneConfiguration)
public:
    virtual ~IProjectSceneConfiguration() = default;

    virtual bool isVerticalRulersVisible() const = 0;
    virtual void setVerticalRulersVisible(bool visible) = 0;
    virtual muse::async::Channel<bool> isVerticalRulersVisibleChanged() const = 0;

    virtual bool isRMSInWaveformVisible() const = 0;
    virtual void setRMSInWaveformVisible(bool visible) = 0;
    virtual muse::async::Channel<bool> isRMSInWaveformVisibleChanged() const = 0;

    virtual bool isClippingInWaveformVisible() const = 0;
    virtual void setClippingInWaveformVisible(bool visible) = 0;
    virtual muse::async::Channel<bool> isClippingInWaveformVisibleChanged() const = 0;

    virtual double zoom(const muse::modularity::ContextPtr& ctx) const = 0;

    virtual int mouseZoomPrecision() const = 0;
    virtual void setMouseZoomPrecision(int precision) = 0;
    virtual TimelineRulerMode timelineRulerMode() const = 0;
    virtual void setTimelineRulerMode(const TimelineRulerMode mode) = 0;
    virtual muse::async::Notification timelineRulerModeChanged() const = 0;

    virtual bool isEffectsPanelVisible() const = 0;
    virtual void setIsEffectsPanelVisible(bool visible) = 0;
    virtual muse::async::Notification isEffectsPanelVisibleChanged() const = 0;

    virtual const std::vector<ClipColorInfo>& clipColorInfos() const = 0;
    virtual QColor clipColor(trackedit::ClipColorIndex index) const = 0;
    virtual QColor clipSelectedColor(trackedit::ClipColorIndex index) const = 0;

    virtual ClipStyles::Style clipStyle() const = 0;
    virtual void setClipStyle(ClipStyles::Style style) = 0;
    virtual muse::async::Channel<ClipStyles::Style> clipStyleChanged() const = 0;

    virtual StereoHeightsPref::AsymmetricStereoHeights stereoHeightsPref() const = 0;
    virtual void setStereoHeightsPref(StereoHeightsPref::AsymmetricStereoHeights pref) = 0;
    virtual muse::async::Notification stereoHeightsPrefChanged() const = 0;

    virtual std::vector<std::string> asymmetricStereoHeightsWorkspaces() const = 0;
    virtual void setAsymmetricStereoHeightsWorkspaces(std::vector<std::string>& workspaces) = 0;
    virtual muse::async::Notification asymmetricStereoHeightsWorkspacesChanged() const = 0;

    virtual int selectionTimecodeFormat() const = 0;
    virtual void setSelectionTimecodeFormat(int) = 0;
    virtual muse::async::Notification selectionTimecodeFormatChanged() const = 0;

    virtual int durationTimecodeFormat() const = 0;
    virtual void setDurationTimecodeFormat(int) = 0;
    virtual muse::async::Notification durationTimecodeFormatChanged() const = 0;

    virtual bool playbackOnRulerClickEnabled() const = 0;
    virtual void setPlaybackOnRulerClickEnabled(bool enabled) = 0;
    virtual muse::async::Notification playbackOnRulerClickEnabledChanged() const = 0;

    virtual bool updateDisplayWhilePlayingEnabled() const = 0;
    virtual void setUpdateDisplayWhilePlayingEnabled(bool enabled) = 0;
    virtual muse::async::Notification updateDisplayWhilePlayingEnabledChanged() const = 0;

    virtual bool pinnedPlayHeadEnabled() const = 0;
    virtual void setPinnedPlayHeadEnabled(bool enabled) = 0;
    virtual muse::async::Notification pinnedPlayHeadEnabledChanged() const = 0;

    virtual int labelEditorColumnFormat(const std::string& columnName) const = 0;
    virtual void setLabelEditorColumnFormat(const std::string& columnName, int format) const = 0;
};
}
