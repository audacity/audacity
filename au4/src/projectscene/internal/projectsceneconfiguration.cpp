/*
* Audacity: A Digital Audio Editor
*/
#include "projectsceneconfiguration.h"

#include "settings.h"

using namespace au::projectscene;

static const std::string moduleName("projectscene");

static const muse::Settings::Key IS_VERTICAL_RULERS_VISIBLE(moduleName, "projectscene/verticalrulersEnabled");

void ProjectSceneConfiguration::init()
{
    muse::settings()->setDefaultValue(IS_VERTICAL_RULERS_VISIBLE, muse::Val(false));
    muse::settings()->valueChanged(IS_VERTICAL_RULERS_VISIBLE).onReceive(nullptr, [this](const muse::Val& val) {
        m_isVerticalRulersVisibleChanged.send(val.toBool());
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
