#include "reverbviewmodel.h"

#include "reverbeffect.h"

using namespace au::effects;

ReverbViewModel::ReverbViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void ReverbViewModel::doReload()
{
    emit wetOnlyChanged();
}

bool ReverbViewModel::wetOnly() const
{
    return settings<ReverbSettings>().mWetOnly;
}

void ReverbViewModel::setWetOnly(bool newWetOnly)
{
    modifySettings<ReverbSettings>([newWetOnly](EffectSettings& settings)
    {
        settings.cast<ReverbSettings>()->mWetOnly = newWetOnly;
    });
    emit wetOnlyChanged();
}
