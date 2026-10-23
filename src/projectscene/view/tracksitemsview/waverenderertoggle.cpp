/*
* Audacity: A Digital Audio Editor
*/
#include "waverenderertoggle.h"

using namespace au::projectscene;

WaveRendererToggle* WaveRendererToggle::instance()
{
    static WaveRendererToggle* toggle = new WaveRendererToggle();
    return toggle;
}

bool WaveRendererToggle::sceneGraph() const
{
    return m_sceneGraph;
}

void WaveRendererToggle::setSceneGraph(bool enabled)
{
    if (m_sceneGraph == enabled) {
        return;
    }

    m_sceneGraph = enabled;
    emit sceneGraphChanged();
}
