#pragma once

#include "uicomponents/view/abstractmenumodel.h"

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectspresetscontroller.h"

namespace au::effects {
class EffectManageMenu : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT
    Q_PROPERTY(QString effectId READ effectId_prop WRITE setEffectId_prop NOTIFY effectIdChanged FINAL)

    muse::Inject<IEffectsPresetsController> presetsController;

public:

    QString effectId_prop() const;
    void setEffectId_prop(const QString& newEffectId);

    Q_INVOKABLE void load() override;

signals:
    void effectIdChanged();

private:
    QString m_effectId;
};
}
