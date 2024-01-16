#pragma once

#include <vector>

#include <QAbstractListModel>
#include <QQmlEngine>

#include "Observer.h"
#include "TrackAdapter.h"

class TrackList;
class TrackListEvent;

class TrackListModel : public QAbstractListModel
{
   Q_OBJECT
   QML_ELEMENT

   using TrackAdapterList = std::vector<TrackAdapterBase*>;

   std::shared_ptr<TrackList> mTrackList;
   Observer::Subscription mTrackListSubscription;
   TrackAdapterList mTrackAdapters;

public:

   enum RoleNames
   {
      TypeRole = Qt::UserRole + 1,
      TrackRole
   };

   TrackListModel(std::shared_ptr<TrackList> trackList, QObject* parent = nullptr);

   int rowCount(const QModelIndex& parent) const override;
   QVariant data(const QModelIndex& index, int role) const override;

   QHash<int, QByteArray> roleNames() const override;

   Q_INVOKABLE
   void move(int from, int to);

private:


   void OnTrackListEvent(TrackListEvent event);

   void HandleTrackAdded(Track& track);
   void HandleTrackRemoved(Track& track);

};
