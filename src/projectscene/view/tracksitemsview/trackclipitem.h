/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "trackedit/dom/clip.h"

#include "viewtrackitem.h"

namespace au::projectscene {
class TrackClipItem : public ViewTrackItem
{
    Q_OBJECT

    Q_PROPERTY(int groupId READ groupId NOTIFY groupIdChanged FINAL)
    Q_PROPERTY(int pitch READ pitch NOTIFY pitchChanged FINAL)
    Q_PROPERTY(int speedPercentage READ speedPercentage NOTIFY speedPercentageChanged FINAL)

public:
    explicit TrackClipItem(QObject* parent);

    void setClip(const trackedit::Clip& clip);

    int groupId() const;

    int pitch() const;
    int speedPercentage() const;

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
