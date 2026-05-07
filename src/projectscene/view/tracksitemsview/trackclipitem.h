/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "projectscene/iprojectsceneconfiguration.h"
#include "trackedit/dom/clip.h"

#include "viewtrackitem.h"

namespace au::projectscene {
class TrackClipItem : public ViewTrackItem
{
    Q_OBJECT

    muse::GlobalInject<IProjectSceneConfiguration> configuration;

    Q_PROPERTY(int groupId READ groupId NOTIFY groupIdChanged FINAL)
    Q_PROPERTY(int pitch READ pitch NOTIFY pitchChanged FINAL)
    Q_PROPERTY(int speedPercentage READ speedPercentage NOTIFY speedPercentageChanged FINAL)
    Q_PROPERTY(bool isPitchModified READ isPitchModified NOTIFY pitchChanged FINAL)
    Q_PROPERTY(bool isSpeedModified READ isSpeedModified NOTIFY speedPercentageChanged FINAL)
    // Q_PROPERTY(QObject * envelopeModel READ envelopeModel NOTIFY envelopeModelChanged)

public:
    explicit TrackClipItem(QObject* parent);

    void setClip(const trackedit::Clip& clip);

    int groupId() const;

    int pitch() const;
    int speedPercentage() const;
    bool isPitchModified() const;
    bool isSpeedModified() const;
    double speed() const;

signals:
    void groupIdChanged();
    void pitchChanged();
    void speedPercentageChanged();
    void contentChanged();

private:
    int m_groupId = -1;
    int m_pitch = 0;
    double m_speed = 1.0;
};
}
