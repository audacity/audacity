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

void DtmfViewModel::doReload()
{
    emit amplitudeChanged();
    emit sequenceChanged();
    emit dutyCycleChanged();
    emit toneDurationChanged();
    emit silenceDurationChanged();
}

QString DtmfViewModel::sequence() const
{
    return QString::fromStdString(settings<DtmfSettings>().dtmfSequence);
}

void DtmfViewModel::setSequence(const QString& newSequence)
{
    mutSettings<DtmfSettings>().dtmfSequence = newSequence.toStdString();
    emit sequenceChanged();
    recalculateDurations();
}

double DtmfViewModel::amplitude() const
{
    return settings<DtmfSettings>().dtmfAmplitude;
}

void DtmfViewModel::setAmplitude(double newAmplitude)
{
    mutSettings<DtmfSettings>().dtmfAmplitude = newAmplitude;
    emit amplitudeChanged();
}

double DtmfViewModel::dutyCycle() const
{
    return settings<DtmfSettings>().dtmfDutyCycle;
}

void DtmfViewModel::setDutyCycle(double newDutyCycle)
{
    mutSettings<DtmfSettings>().dtmfDutyCycle = newDutyCycle;
    emit dutyCycleChanged();
    recalculateDurations();
}

double DtmfViewModel::toneDuration() const
{
    return settings<DtmfSettings>().dtmfTone;
}

double DtmfViewModel::silenceDuration() const
{
    return settings<DtmfSettings>().dtmfSilence;
}

void DtmfViewModel::recalculateDurations()
{
    mutSettings<DtmfSettings>().Recalculate(*AbstractEffectModel::settings());
    emit toneDurationChanged();
    emit silenceDurationChanged();
}
