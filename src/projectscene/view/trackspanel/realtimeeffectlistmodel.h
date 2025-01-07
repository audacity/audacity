/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "realtimeeffectmenumodelbase.h"
#include "effects/effects_base/irealtimeeffectservice.h"
#include <QObject>
#include <map>

namespace au::projectscene {
class RealtimeEffectListItemModel;

class RealtimeEffectListModel : public RealtimeEffectMenuModelBase
{
    Q_OBJECT

    Q_PROPERTY(QVariantList availableEffects READ availableEffects NOTIFY availableEffectsChanged)

    muse::Inject<effects::IRealtimeEffectService> realtimeEffectService;

public:
    explicit RealtimeEffectListModel(QObject* parent = nullptr);

    Q_INVOKABLE void handleMenuItemWithState(const QString& menuItemId, const RealtimeEffectListItemModel*);
    QVariantList availableEffects();

signals:
    void availableEffectsChanged();

private:
    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;

private:
    enum RoleNames
    {
        rItemData = Qt::UserRole + 1
    };

    void doLoad() override;
    void populateMenu() override;

    void insertEffect(effects::TrackId trackId, effects::EffectChainLinkIndex index, const effects::EffectStateId& item);
    void removeEffect(effects::TrackId trackId, effects::EffectChainLinkIndex index, const effects::EffectStateId& item);

    using EffectList = std::vector<RealtimeEffectListItemModel*>;
    std::map<effects::TrackId, EffectList> m_trackEffectLists;
};
}
