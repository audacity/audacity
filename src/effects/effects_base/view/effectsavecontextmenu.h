/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"

#include "framework/global/modularity/ioc.h"
#include "effects/effects_base/ieffectpresetsprovider.h"
#include "effects/effects_base/ieffectinstancesregister.h"

#include "effects/effects_base/effectstypes.h"

namespace au::effects {
class EffectSaveContextMenu : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    Q_PROPERTY(int instanceId READ instanceId_prop WRITE setInstanceId_prop NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(QString preset READ preset WRITE setPreset NOTIFY presetChanged FINAL)

    muse::Inject<IEffectPresetsProvider> presetsController { this };
    muse::Inject<IEffectInstancesRegister> instancesRegister { this };

public:
    explicit EffectSaveContextMenu(QObject* parent = nullptr);

    int instanceId_prop() const;
    void setInstanceId_prop(int newInstanceId);

    QString preset() const;
    void setPreset(QString newPreset);

    bool canSave() const;
    Q_INVOKABLE void load() override;

signals:
    void instanceIdChanged();
    void presetChanged();

private:
    void reload();

    int m_instanceId = -1;
    QString m_preset;
    bool m_canSave = false;
};
}
