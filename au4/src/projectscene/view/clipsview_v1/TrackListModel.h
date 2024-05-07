#pragma once

#include <vector>

#include <QAbstractListModel>
#include <QQmlEngine>

#include "Observer.h"
#include "TrackAdapter.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

class TrackList;
class TrackListEvent;

class TracksListClipsModel : public QAbstractListModel
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> globalContext;

public:

    enum RoleNames
    {
        TypeRole = Qt::UserRole + 1,
        TrackRole
    };

    TracksListClipsModel(QObject* parent = nullptr);

    int rowCount(const QModelIndex& parent) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    Q_INVOKABLE void load();
    Q_INVOKABLE void move(int from, int to);

private:

    void OnTrackListEvent(TrackListEvent event);

    void HandleTrackAdded(Track& track);
    void HandleTrackRemoved(Track& track);

    using TrackAdapterList = std::vector<TrackAdapterBase*>;

    std::shared_ptr<TrackList> mTrackList;
    Observer::Subscription mTrackListSubscription;
    TrackAdapterList mTrackAdapters;
};
