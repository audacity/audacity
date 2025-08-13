#pragma once

#include "async/channel.h"
#include "async/notification.h"

#include "modularity/imoduleinterface.h"

#include "types/projectscenetypes.h"

namespace au::projectscene {
class IProjectSceneConfiguration : MODULE_EXPORT_INTERFACE
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

    virtual double zoom() const = 0;

    virtual int mouseZoomPrecision() const = 0;
    virtual void setMouseZoomPrecision(int precision) = 0;
    virtual TimelineRulerMode timelineRulerMode() const = 0;
    virtual void setTimelineRulerMode(const TimelineRulerMode mode) = 0;
    virtual muse::async::Notification timelineRulerModeChanged() const = 0;

    virtual bool isEffectsPanelVisible() const = 0;
    virtual void setIsEffectsPanelVisible(bool visible) = 0;
    virtual muse::async::Notification isEffectsPanelVisibleChanged() const = 0;

    virtual const std::vector<std::pair<std::string, std::string> >& clipColors() const = 0;

    virtual ClipStyles::Style clipStyle() const = 0;
    virtual void setClipStyle(ClipStyles::Style style) = 0;
    virtual muse::async::Channel<ClipStyles::Style> clipStyleChanged() const = 0;

    virtual StereoHeightsPref::AsymmetricStereoHeights stereoHeightsPref() const = 0;
    virtual void setStereoHeightsPref(StereoHeightsPref::AsymmetricStereoHeights pref) = 0;
    virtual muse::async::Notification stereoHeightsPrefChanged() const = 0;

    virtual std::vector<std::string> asymmetricStereoHeightsWorkspaces() const = 0;
    virtual void setAsymmetricStereoHeightsWorkspaces(std::vector<std::string>& workspaces) = 0;
    virtual muse::async::Notification asymmetricStereoHeightsWorkspacesChanged() const = 0;
};
}
