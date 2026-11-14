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

    Q_PROPERTY(bool isGrouped READ isGrouped NOTIFY groupIdChanged FINAL)
    Q_PROPERTY(int pitch READ pitch NOTIFY pitchChanged FINAL)
    Q_PROPERTY(int speedPercentage READ speedPercentage NOTIFY speedPercentageChanged FINAL)
    Q_PROPERTY(bool isPitchModified READ isPitchModified NOTIFY pitchChanged FINAL)
    Q_PROPERTY(bool isSpeedModified READ isSpeedModified NOTIFY speedPercentageChanged FINAL)
    Q_PROPERTY(double fadeInDuration READ fadeInDuration NOTIFY fadesChanged FINAL)
    Q_PROPERTY(double fadeOutDuration READ fadeOutDuration NOTIFY fadesChanged FINAL)
    Q_PROPERTY(FadeTypes::Type fadeInType READ fadeInType NOTIFY fadesChanged FINAL)
    Q_PROPERTY(FadeTypes::Type fadeOutType READ fadeOutType NOTIFY fadesChanged FINAL)
    // Q_PROPERTY(QObject * envelopeModel READ envelopeModel NOTIFY envelopeModelChanged)

public:
    explicit TrackClipItem(QObject* parent);

    void setClip(const trackedit::Clip& clip);

    bool isGrouped() const;

    int pitch() const;
    int speedPercentage() const;
    bool isPitchModified() const;
    bool isSpeedModified() const;
    double speed() const;

    double fadeInDuration() const;
    double fadeOutDuration() const;
    FadeTypes::Type fadeInType() const;
    FadeTypes::Type fadeOutType() const;

signals:
    void groupIdChanged();
    void pitchChanged();
    void speedPercentageChanged();
    void contentChanged();
    void fadesChanged();

private:
    int m_groupId = -1;
    int m_pitch = 0;
    double m_speed = 1.0;
    double m_fadeInDuration = 0.0;
    double m_fadeOutDuration = 0.0;
    FadeTypes::Type m_fadeInType = FadeTypes::Type::LINEAR;
    FadeTypes::Type m_fadeOutType = FadeTypes::Type::LINEAR;
};
}
