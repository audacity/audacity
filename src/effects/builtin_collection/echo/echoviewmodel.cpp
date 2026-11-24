#include "echoviewmodel.h"

#include "echoeffect.h"
#include "../common/measureunits.h"

#include "framework/global/translation.h"

#include <algorithm>

using namespace au::effects;

namespace {
constexpr double DELAY_STEP = 0.1;
constexpr int DELAY_DECIMALS = 3;
constexpr double DECAY_STEP = 0.1;
constexpr int DECAY_DECIMALS = 3;
}

EchoViewModel::EchoViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void EchoViewModel::doReload()
{
    emit delayChanged();
    emit decayChanged();
}

double EchoViewModel::delay() const
{
    return settings<EchoSettings>().delay;
}

void EchoViewModel::setDelay(double newDelay)
{
    const double clamped = std::clamp<double>(newDelay, EchoEffect::Delay.min, EchoEffect::Delay.max);
    modifySettings<EchoSettings>([clamped](EffectSettings& settings) {
        settings.cast<EchoSettings>()->delay = clamped;
    });
    emit delayChanged();
}

double EchoViewModel::decay() const
{
    return settings<EchoSettings>().decay;
}

void EchoViewModel::setDecay(double newDecay)
{
    const double clamped = std::clamp<double>(newDecay, EchoEffect::Decay.min, EchoEffect::Decay.max);
    modifySettings<EchoSettings>([clamped](EffectSettings& settings) {
        settings.cast<EchoSettings>()->decay = clamped;
    });
    emit decayChanged();
}

QString EchoViewModel::delayLabel() const
{
    //: Label of the Echo effect control that sets the time between the original sound and each echo, in seconds
    return muse::qtrc("effects/echo", "Delay time");
}

double EchoViewModel::delayMin() const
{
    return EchoEffect::Delay.min;
}

double EchoViewModel::delayMax() const
{
    return EchoEffect::Delay.max;
}

double EchoViewModel::delayStep() const
{
    return DELAY_STEP;
}

int EchoViewModel::delayDecimals() const
{
    return DELAY_DECIMALS;
}

QString EchoViewModel::delayUnitSymbol() const
{
    return units::seconds().m_symbol;
}

QString EchoViewModel::decayLabel() const
{
    //: Label of the Echo effect control that sets how much quieter each echo is than the previous one. A factor, not a time.
    return muse::qtrc("effects/echo", "Decay factor");
}

double EchoViewModel::decayMin() const
{
    return EchoEffect::Decay.min;
}

double EchoViewModel::decayMax() const
{
    return EchoEffect::Decay.max;
}

double EchoViewModel::decayStep() const
{
    return DECAY_STEP;
}

int EchoViewModel::decayDecimals() const
{
    return DECAY_DECIMALS;
}
