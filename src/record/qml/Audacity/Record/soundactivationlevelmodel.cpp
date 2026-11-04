/*
* Audacity: A Digital Audio Editor
*/
#include "soundactivationlevelmodel.h"

#include <algorithm>

#include "framework/global/realfn.h"

using namespace au::record;

SoundActivationLevelModel::SoundActivationLevelModel(QObject* parent)
    : MeterModel(parent)
{
}

void SoundActivationLevelModel::init()
{
    MeterModel::init();

    recordConfiguration()->soundActivationLevelChanged().onNotify(this, [this]() {
        setLevel(recordConfiguration()->soundActivationLevel());
    });

    setLevel(recordConfiguration()->soundActivationLevel());
}

void SoundActivationLevelModel::apply()
{
    recordConfiguration()->setSoundActivationLevel(m_level);
}

double SoundActivationLevelModel::level() const
{
    return m_level;
}

void SoundActivationLevelModel::setLevel(double level)
{
    level = muse::RealRound(std::clamp(level, static_cast<double>(dbRange()), 0.0), 1);
    if (muse::RealIsEqual(m_level, level)) {
        return;
    }

    m_level = level;
    setVolume(static_cast<float>(level));
    emit levelChanged();
}
