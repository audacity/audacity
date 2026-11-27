#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

namespace au::effects {
class ReverbEffect;
struct ReverbSettings;
class ReverbViewModel : public BuiltinEffectModel
{
    Q_OBJECT
    Q_PROPERTY(bool wetOnly READ wetOnly WRITE setWetOnly NOTIFY wetOnlyChanged FINAL)

public:
    ReverbViewModel(QObject* parent, int instanceId);

    bool wetOnly() const;
    void setWetOnly(bool newWetOnly);

signals:
    void wetOnlyChanged();

private:

    void doReload() override;
    void doUpdateSettings() override
    {
        // TODO: let https://github.com/audacity/audacity/pull/8565 get merged and then implement this.
    }
};

class ReverbViewModelFactory : public EffectViewModelFactory<ReverbViewModel>
{
};
}
