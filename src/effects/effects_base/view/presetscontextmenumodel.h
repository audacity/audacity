/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"

#include "framework/global/modularity/ioc.h"
#include "effects/effects_base/ieffectsconfiguration.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class PresetsContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT
    Q_PROPERTY(int instanceId READ instanceId_prop WRITE setInstanceId_prop NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(bool useVendorUI READ useVendorUI NOTIFY useVendorUIChanged FINAL)

    muse::GlobalInject<IEffectsConfiguration> configuration;
    muse::Inject<IEffectInstancesRegister> instancesRegister { this };
    muse::Inject<IEffectsProvider> effectsProvider { this };

public:
    explicit PresetsContextMenuModel(QObject* parent = nullptr);

    int instanceId_prop() const;
    void setInstanceId_prop(int newInstanceId);
    bool useVendorUI() const;

    Q_INVOKABLE void load() override;

signals:
    void instanceIdChanged();
    void useVendorUIChanged();

private:
    void reload();

    int m_instanceId = -1;
};
}
