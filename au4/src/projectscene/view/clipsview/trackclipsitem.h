#ifndef AU_PROJECTSCENE_TRACKCLIPSITEM_H
#define AU_PROJECTSCENE_TRACKCLIPSITEM_H

#include <QObject>
#include "clipitem.h"

namespace au::projectscene {
class TrackClipsItem : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QList<ClipItem*> clips READ clips NOTIFY clipsChanged FINAL)

public:
    TrackClipsItem(QObject* parent = nullptr);

    QList<ClipItem*> clips() const;

    void setClips(QList<ClipItem*> list);

signals:
    void clipsChanged();

private:
    QList<ClipItem*> m_clips;
};
}

#endif // AU_PROJECTSCENE_TRACKCLIPSITEM_H
