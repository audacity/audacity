#pragma once

#include "uicomponents/view/abstractmenumodel.h"

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectpresetsprovider.h"
#include "effects/effects_base/ieffectinstancesregister.h"

namespace au::effects {
class EffectManageMenu : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT
    Q_PROPERTY(QString instanceId READ instanceId_prop WRITE setInstanceId_prop NOTIFY instanceIdChanged FINAL)

    muse::Inject<IEffectPresetsProvider> presetsController;
    muse::Inject<IEffectInstancesRegister> instancesRegister;

public:

    QString instanceId_prop() const;
    void setInstanceId_prop(const QString& newInstanceId);

    Q_INVOKABLE void load() override;

signals:
    void instanceIdChanged();

private:

    void reload(const EffectId& effectId, const EffectInstanceId& instanceId);

    QString m_instanceId;
};
}
