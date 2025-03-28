/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "realtimeeffectlistitemmodel.h"
#include "internal/irealtimeeffectpaneltrackselection.h"

#include "context/iglobalcontext.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include "async/asyncable.h"
#include "modularity/ioc.h"
#include <QAbstractListModel>
#include <map>

namespace au::projectscene {
class RealtimeEffectListModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QString trackName READ prop_trackName NOTIFY trackNameChanged)
    Q_PROPERTY(bool isMasterTrack READ prop_isMasterTrack WRITE prop_setIsMasterTrack NOTIFY isMasterTrackChanged)
    Q_PROPERTY(bool trackEffectsActive READ prop_trackEffectsActive WRITE prop_setTrackEffectsActive NOTIFY trackEffectsActiveChanged)

    muse::Inject<effects::IRealtimeEffectService> realtimeEffectService;
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<IRealtimeEffectPanelTrackSelection> trackSelection;

public:
    explicit RealtimeEffectListModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE int count() const;
    Q_INVOKABLE void moveRow(int from, int to);

    QString prop_trackName() const;

    bool prop_trackEffectsActive() const;
    void prop_setTrackEffectsActive(bool active);

    bool prop_isMasterTrack() const;
    void prop_setIsMasterTrack(bool isMasterTrack);

signals:
    void isMasterTrackChanged();
    void availableEffectsChanged();
    void trackNameChanged();
    void trackEffectsActiveChanged();

private:
    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;

private:
    enum RoleNames
    {
        rItemData = Qt::UserRole + 1
    };

    void onSelectedTrackIdChanged();
    std::optional<effects::TrackId> trackId() const;

    bool belongsWithMe(effects::TrackId trackId) const;
    void onAdded(effects::TrackId trackId, const effects::RealtimeEffectStatePtr& newState);
    void onReplaced(effects::TrackId trackId, effects::EffectChainLinkIndex index, const effects::RealtimeEffectStatePtr& newState);
    void onRemoved(effects::TrackId trackId, const effects::RealtimeEffectStatePtr& state);
    void onMoved(effects::TrackId trackId, effects::EffectChainLinkIndex from, effects::EffectChainLinkIndex to);
    void onChanged(effects::TrackId trackId);
    void onProjectChanged();

    using EffectList = QList<RealtimeEffectListItemModelPtr>;
    std::map<effects::TrackId, EffectList> m_trackEffectLists;
    bool m_isMasterTrack = false;
};
}
