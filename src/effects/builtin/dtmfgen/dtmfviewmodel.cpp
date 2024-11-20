/*
 * Audacity: A Digital Audio Editor
 */
#include "dtmfviewmodel.h"

using namespace au::effects;

DtmfViewModel::DtmfViewModel()
{
    connect(this, &DtmfViewModel::durationChanged, this, &DtmfViewModel::recalculateDurations);
}

DtmfViewModel::~DtmfViewModel()
{
}

QString DtmfViewModel::sequence() const
{
    return QString::fromStdString(settings().dtmfSequence);
}

void DtmfViewModel::setSequence(const QString& newSequence)
{
    mutSettings().dtmfSequence = newSequence.toStdString();
    emit sequenceChanged();
    recalculateDurations();
}

double DtmfViewModel::amplitude() const
{
    return settings().dtmfAmplitude;
}

void DtmfViewModel::setAmplitude(double newAmplitude)
{
    mutSettings().dtmfAmplitude = newAmplitude;
    emit amplitudeChanged();
}

double DtmfViewModel::dutyCycle() const
{
    return settings().dtmfDutyCycle;
}

void DtmfViewModel::setDutyCycle(double newDutyCycle)
{
    mutSettings().dtmfDutyCycle = newDutyCycle;
    emit dutyCycleChanged();
    recalculateDurations();
}

double DtmfViewModel::toneDuration() const
{
    return settings().dtmfTone;
}

double DtmfViewModel::silenceDuration() const
{
    return settings().dtmfSilence;
}

DtmfSettings& DtmfViewModel::mutSettings()
{
    return DtmfGenerator::GetSettings(*AbstractEffectModel::settings());
}

const DtmfSettings& DtmfViewModel::settings() const
{
    return DtmfGenerator::GetSettings(*AbstractEffectModel::settings());
}

void DtmfViewModel::recalculateDurations()
{
    mutSettings().Recalculate(*AbstractEffectModel::settings());
    emit toneDurationChanged();
    emit silenceDurationChanged();
}
