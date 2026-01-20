#pragma once

#include "framework/uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"
#include "framework/global/modularity/ioc.h"

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
    Q_PROPERTY(bool useVendorUI READ useVendorUI WRITE setUseVendorUI NOTIFY useVendorUIChanged FINAL)

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
    bool useVendorUI() const;
    void setUseVendorUI(bool value);

    Q_INVOKABLE void resetPreset();
    Q_INVOKABLE void savePresetAs();

    Q_INVOKABLE void load() override;

signals:
    void instanceIdChanged();
    void presetsChanged();
    void presetChanged();
    void useVendorUIChanged();

private:

    void reload(const EffectId& effectId, const EffectInstanceId& instanceId);

    int m_instanceId = -1;
    QString m_currentPreset;
    QVariantList m_presets;
};
}
