/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "rteffectmenumodelbase.h"
#include "au3audio/iaudioengine.h"
#include <QObject>
#include <map>

namespace au::projectscene {
class ModelEffectItem : public QObject
{
    Q_OBJECT
public:
    ModelEffectItem(QObject* parent, std::string effectName, const void* effectState);

    const void* const effectState;
    Q_INVOKABLE QString effectName() const;

private:
    const std::string m_effectName;
};

class RtEffectListModel : public RtEffectMenuModelBase
{
    Q_OBJECT

    Q_PROPERTY(QVariantList availableEffects READ availableEffects NOTIFY availableEffectsChanged)

    muse::Inject<effects::IEffectsProvider> effectsProvider;
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<audio::IAudioEngine> audioEngine;

public:
    explicit RtEffectListModel(QObject* parent = nullptr);

    Q_INVOKABLE void handleMenuItemWithState(const QString& menuItemId, const ModelEffectItem*);
    Q_INVOKABLE void load() override;
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

    void populateMenu();
    void setListenerOnCurrentTrackeditProject();
    void insertEffect(audio::TrackId trackId, audio::EffectChainLinkIndex index, const audio::EffectChainLink& item);
    void removeEffect(audio::TrackId trackId, audio::EffectChainLinkIndex index, const audio::EffectChainLink& item);

    using EffectList = std::vector<ModelEffectItem*>;
    std::map<audio::TrackId, EffectList> m_trackEffectLists;
};
}
