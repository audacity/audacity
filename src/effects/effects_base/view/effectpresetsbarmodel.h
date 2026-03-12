/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include "effects/effects_base/ieffectparametersprovider.h"
#include "effects/effects_base/ieffectpresetsprovider.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectsconfiguration.h"
#include "effects/effects_base/ieffectsprovider.h"

#include "effectsavecontextmenu.h"
#include "presetscontextmenumodel.h"

namespace au::effects {
class EffectPresetsBarModel : public QObject, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(int instanceId READ instanceId_prop WRITE setInstanceId_prop NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(QString sessionStateKey READ sessionStateKey WRITE setSessionStateKey NOTIFY sessionStateKeyChanged FINAL)
    Q_PROPERTY(QVariantList presets READ presets NOTIFY presetsChanged FINAL)
    Q_PROPERTY(QString preset READ preset WRITE setPreset NOTIFY presetChanged FINAL)
    Q_PROPERTY(bool presetsDropdownEnabled READ presetsDropdownEnabled NOTIFY presetsChanged FINAL)
    Q_PROPERTY(bool canDeletePreset READ canDeletePreset NOTIFY canDeletePresetChanged FINAL)
    Q_PROPERTY(bool canResetPreset READ canResetPreset NOTIFY canResetPresetChanged FINAL)
    Q_PROPERTY(bool useVendorUI READ useVendorUI NOTIFY useVendorUIChanged FINAL)
    Q_PROPERTY(
        bool persistLastUsedPreset READ persistLastUsedPreset WRITE setPersistLastUsedPreset NOTIFY persistLastUsedPresetChanged FINAL)

    muse::GlobalInject<IEffectsConfiguration> configuration;

    muse::Inject<IEffectPresetsProvider> presetsController { this };
    muse::Inject<IEffectInstancesRegister> instancesRegister { this };
    muse::Inject<IEffectsProvider> effectsProvider { this };
    muse::Inject<IEffectParametersProvider> parametersProvider { this };
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };

public:
    explicit EffectPresetsBarModel(QObject* parent = nullptr);

    int instanceId_prop() const;
    void setInstanceId_prop(int newInstanceId);
    QString sessionStateKey() const;
    void setSessionStateKey(const QString& newSessionStateKey);
    QVariantList presets();
    QString preset() const;
    void setPreset(QString presetId);
    bool presetsDropdownEnabled() const;
    bool canDeletePreset() const;
    bool canResetPreset() const;
    bool useVendorUI() const;
    bool persistLastUsedPreset() const;
    void setPersistLastUsedPreset(bool value);

    Q_INVOKABLE void resetPreset();
    Q_INVOKABLE void savePresetAs();
    Q_INVOKABLE void deletePreset();
    Q_INVOKABLE void commitSelectedPreset();
    Q_INVOKABLE void restoreInitialSessionPresetState();
    Q_INVOKABLE muse::uicomponents::AbstractMenuModel* saveContextMenu();
    Q_INVOKABLE muse::uicomponents::AbstractMenuModel* presetContextMenu();

    Q_INVOKABLE void load();

signals:
    void instanceIdChanged();
    void sessionStateKeyChanged();
    void presetsChanged();
    void presetChanged();
    void canDeletePresetChanged();
    void canResetPresetChanged();
    void useVendorUIChanged();
    void persistLastUsedPresetChanged();

private:

    void reload(const EffectId& effectId, const EffectInstanceId& instanceId);
    bool hasPreset(const QString& presetId) const;
    bool isUserPreset(const QString& presetId) const;
    bool isFactoryPreset(const QString& presetId) const;
    int factoryPresetIndex(const QString& presetId) const;
    QString matchPresetForCurrentSettings() const;
    void captureInitialSessionPresetState();
    bool restoreSessionPresetState();
    void restoreLastUsedPreset(const EffectId& effectId);
    void restoreMatchedPresetForCurrentSettings();
    QString sessionPresetStateKey() const;
    void persistSessionPresetState();
    bool isCurrentPresetUnsaved() const;
    void setPresetUnsaved(bool unsaved);
    void updatePresetDisplayNames();
    void updatePresetBar();

    int m_instanceId = -1;
    QString m_sessionStateKey;

    QString m_currentPreset;
    QStringList m_userPresets;
    QStringList m_factoryPresets;
    QHash<QString, QString> m_basePresetNames;
    QVariantList m_allPresets;

    bool m_isPresetUnsaved = false;
    bool m_usedDestructively = false;
    std::optional<QString> m_initialPreset;
    bool m_initialPresetUnsaved = false;

    EffectSaveContextMenu* m_saveContextMenu = nullptr;
    PresetsContextMenuModel* m_presetsContextMenu = nullptr;
};
}
