#ifndef AU_PROJECTSCENE_CLIPSMODEL_H
#define AU_PROJECTSCENE_CLIPSMODEL_H

#include <QObject>
#include <QList>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "trackclipsitem.h"

namespace au::projectscene {
class ClipsModel : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QList<TrackClipsItem*> tracks READ tracks WRITE setTracks NOTIFY tracksChanged FINAL)

    mu::Inject<au::context::IGlobalContext> globalContext;

public:
    ClipsModel() = default;

    QList<TrackClipsItem*> tracks() const;
    void setTracks(const QList<TrackClipsItem*>& newTracks);

    Q_INVOKABLE void load();

signals:
    void tracksChanged();

private:
    QList<TrackClipsItem*> m_tracks;
};
}

#endif // AU_PROJECTSCENE_CLIPSMODEL_H
