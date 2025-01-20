/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "realtimeeffectmenumodelbase.h"
#include "realtimeeffectlistitemmodel.h"
#include "context/iglobalcontext.h"
#include <QObject>
#include <map>

namespace au::projectscene {
class RealtimeEffectListModel : public RealtimeEffectMenuModelBase
{
    Q_OBJECT

    Q_PROPERTY(QVariantList availableEffects READ availableEffects NOTIFY availableEffectsChanged)
    Q_PROPERTY(QString trackName READ prop_trackName NOTIFY trackNameChanged)
    Q_PROPERTY(bool trackEffectsActive READ prop_trackEffectsActive WRITE prop_setTrackEffectsActive NOTIFY trackEffectsActiveChanged)

    muse::Inject<context::IGlobalContext> globalContext;

public:
    explicit RealtimeEffectListModel(QObject* parent = nullptr);

    Q_INVOKABLE void handleMenuItemWithState(const QString& menuItemId, const RealtimeEffectListItemModel*);
    QVariantList availableEffects();
    QString prop_trackName() const;

    bool prop_trackEffectsActive() const;
    void prop_setTrackEffectsActive(bool active);

signals:
    void availableEffectsChanged();
    void trackNameChanged();
    void trackEffectsActiveChanged();

private:
    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;

    void doResetList() override;
    void doRemoveTrack(const au::trackedit::TrackId& trackId) override;
    void onTrackIdChanged() override;

private:
    enum RoleNames
    {
        rItemData = Qt::UserRole + 1
    };

    void doLoad() override;
    void populateMenu() override;
    void onProjectChanged();
    void insertEffect(effects::TrackId trackId, effects::EffectChainLinkIndex index, const effects::RealtimeEffectStatePtr& item);
    void removeEffect(effects::TrackId trackId, const effects::RealtimeEffectStatePtr& item);

    using EffectList = std::vector<RealtimeEffectListItemModelPtr>;
    std::map<effects::TrackId, EffectList> m_trackEffectLists;
};
}
