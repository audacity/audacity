/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"

#include "ui/iuiconfiguration.h"
#include "workspace/iworkspacemanager.h"

#include "../iprojectsceneconfiguration.h"

namespace au::projectscene {
class ProjectSceneConfiguration : public IProjectSceneConfiguration
{
public:
    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;
    muse::Inject<muse::workspace::IWorkspaceManager> workspaceManager;

public:
    ProjectSceneConfiguration() = default;

    void init();

    bool isVerticalRulersVisible() const override;
    void setVerticalRulersVisible(bool visible) override;
    muse::async::Channel<bool> isVerticalRulersVisibleChanged() const override;

    bool isRMSInWaveformVisible() const override;
    void setRMSInWaveformVisible(bool visible) override;
    muse::async::Channel<bool> isRMSInWaveformVisibleChanged() const override;

    bool isClippingInWaveformVisible() const override;
    void setClippingInWaveformVisible(bool visible) override;
    muse::async::Channel<bool> isClippingInWaveformVisibleChanged() const override;

    double zoom() const override;

    int mouseZoomPrecision() const override;
    void setMouseZoomPrecision(int precision) override;

    TimelineRulerMode timelineRulerMode() const override;
    void setTimelineRulerMode(const TimelineRulerMode mode) override;
    muse::async::Notification timelineRulerModeChanged() const override;

    bool isEffectsPanelVisible() const override;
    void setIsEffectsPanelVisible(bool visible) override;
    muse::async::Notification isEffectsPanelVisibleChanged() const override;

    const std::vector<std::pair<std::string, std::string> >& clipColors() const override;

    ClipStyles::Style clipStyle() const override;
    void setClipStyle(ClipStyles::Style style) override;
    muse::async::Channel<ClipStyles::Style> clipStyleChanged() const override;

    StereoHeightsPref::AsymmetricStereoHeights stereoHeightsPref() const override;
    void setStereoHeightsPref(StereoHeightsPref::AsymmetricStereoHeights pref) override;
    muse::async::Notification stereoHeightsPrefChanged() const override;

    std::vector<std::string> asymmetricStereoHeightsWorkspaces() const override;
    void setAsymmetricStereoHeightsWorkspaces(std::vector<std::string>& workspaces) override;
    muse::async::Notification asymmetricStereoHeightsWorkspacesChanged() const override;

private:
    muse::async::Channel<bool> m_isVerticalRulersVisibleChanged;
    muse::async::Channel<bool> m_isRMSInWaveformVisibleChanged;
    muse::async::Channel<bool> m_isClippingInWaveformVisibleChanged;
    muse::async::Channel<ClipStyles::Style> m_clipStyleChanged;
    muse::async::Notification m_effectsPanelVisible;
    muse::async::Notification m_asymmetricStereoHeightsChanged;
    muse::async::Notification m_asymmetricStereoHeightsWorkspacesChanged;
};
}
