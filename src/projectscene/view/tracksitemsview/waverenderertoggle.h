/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

namespace au::projectscene {
//! TEMPORARY: switches between the scene graph and the QPainter waveform renderer for testing
class WaveRendererToggle : public QObject
{
    Q_OBJECT
    Q_PROPERTY(bool sceneGraph READ sceneGraph WRITE setSceneGraph NOTIFY sceneGraphChanged FINAL)

public:
    static WaveRendererToggle* instance();

    bool sceneGraph() const;
    void setSceneGraph(bool enabled);

signals:
    void sceneGraphChanged();

private:
    bool m_sceneGraph = true;
};
}
