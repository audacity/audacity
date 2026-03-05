#pragma once

#include "framework/uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"
#include "framework/global/modularity/ioc.h"

#include <QStringList>
#include <QVariant>

#include "effects/effects_base/ieffectpresetsprovider.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectsconfiguration.h"
#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class EffectManageMenu : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT
    Q_PROPERTY(int instanceId READ instanceId_prop WRITE setInstanceId_prop NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(QVariantList presets READ presets NOTIFY presetsChanged FINAL)
    Q_PROPERTY(QString preset READ preset WRITE setPreset NOTIFY presetChanged FINAL)
    Q_PROPERTY(bool enabled READ enabled NOTIFY presetsChanged FINAL)
    Q_PROPERTY(bool canDeletePreset READ canDeletePreset NOTIFY canDeletePresetChanged FINAL)
    Q_PROPERTY(bool useVendorUI READ useVendorUI WRITE setUseVendorUI NOTIFY useVendorUIChanged FINAL)
    Q_PROPERTY(bool persistLastUsedPreset READ persistLastUsedPreset WRITE setPersistLastUsedPreset NOTIFY persistLastUsedPresetChanged FINAL)

    muse::GlobalInject<IEffectsConfiguration> configuration;

    muse::Inject<IEffectPresetsProvider> presetsController { this };
    muse::Inject<IEffectInstancesRegister> instancesRegister { this };
    muse::Inject<IEffectsProvider> effectsProvider { this };
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };

public:

    int instanceId_prop() const;
    void setInstanceId_prop(int newInstanceId);
    QVariantList presets();
    QString preset() const;
    void setPreset(QString presetId);
    bool enabled() const;
    bool canDeletePreset() const;
    bool useVendorUI() const;
    void setUseVendorUI(bool value);
    bool persistLastUsedPreset() const;
    void setPersistLastUsedPreset(bool value);

    Q_INVOKABLE void resetPreset();
    Q_INVOKABLE void savePresetAs();
    Q_INVOKABLE void deletePreset();
    Q_INVOKABLE void commitSelectedPreset();

    Q_INVOKABLE void load() override;

signals:
    void instanceIdChanged();
    void presetsChanged();
    void presetChanged();
    void canDeletePresetChanged();
    void useVendorUIChanged();
    void persistLastUsedPresetChanged();

private:

    void reload(const EffectId& effectId, const EffectInstanceId& instanceId);
    bool hasPreset(const QString& presetId) const;

    int m_instanceId = -1;
    QString m_currentPreset;
    QStringList m_userPresets;
    QVariantList m_presets;
    bool m_persistLastUsedPreset = false;
    bool m_hasLoadedInitialPreset = false;
};
}
