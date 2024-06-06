#pragma once

#include "../iprojectsceneconfiguration.h"

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

namespace au::projectscene {
class ProjectSceneConfiguration : public IProjectSceneConfiguration
{
public:
    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;

public:
    ProjectSceneConfiguration() = default;

    void init();

    bool isVerticalRulersVisible() const override;
    void setVerticalRulersVisible(bool visible) override;
    muse::async::Channel<bool> isVerticalRulersVisibleChanged() const override;

private:
    muse::async::Channel<bool> m_isVerticalRulersVisibleChanged;
};
}
