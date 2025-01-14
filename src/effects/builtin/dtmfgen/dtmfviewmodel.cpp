/*
 * Audacity: A Digital Audio Editor
 */
#include "dtmfviewmodel.h"
#include "dtmfgenerator.h"

using namespace au::effects;

DtmfViewModel::DtmfViewModel()
{
    connect(this, &DtmfViewModel::durationChanged, this, &DtmfViewModel::recalculateDurations);
}

DtmfViewModel::~DtmfViewModel()
{
}

bool DtmfViewModel::isApplyAllowed() const
{
    return settings<DtmfSettings>().isApplyAllowed() && GeneratorEffectModel::isApplyAllowed();
}

void DtmfViewModel::doEmitSignals()
{
    emit amplitudeChanged();
    emit sequenceChanged();
    emit dutyCycleChanged();
    emit toneDurationChanged();
    emit silenceDurationChanged();
    emit isApplyAllowedChanged();
}

QString DtmfViewModel::sequence() const
{
    return QString::fromStdString(settings<DtmfSettings>().dtmfSequence);
}

void DtmfViewModel::prop_setSequence(const QString& newSequence)
{
    if (!m_inited) {
        return;
    }
    const bool wasAllowed = isApplyAllowed();

    mutSettings<DtmfSettings>().dtmfSequence = newSequence.toStdString();
    emit sequenceChanged();
    recalculateDurations();

    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

double DtmfViewModel::amplitude() const
{
    return settings<DtmfSettings>().dtmfAmplitude;
}

void DtmfViewModel::prop_setAmplitude(double newAmplitude)
{
    if (!m_inited) {
        return;
    }

    const bool wasAllowed = isApplyAllowed();

    mutSettings<DtmfSettings>().dtmfAmplitude = newAmplitude;

    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

double DtmfViewModel::dutyCycle() const
{
    return settings<DtmfSettings>().dtmfDutyCycle;
}

void DtmfViewModel::prop_setDutyCycle(double newDutyCycle)
{
    if (!m_inited) {
        return;
    }

    const bool wasAllowed = isApplyAllowed();

    mutSettings<DtmfSettings>().dtmfDutyCycle = newDutyCycle;
    emit dutyCycleChanged();
    recalculateDurations();

    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
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
    modifySettings([this](EffectSettings& settings) {
        mutSettings<DtmfSettings>().Recalculate(settings);
    });
    emit toneDurationChanged();
    emit silenceDurationChanged();
}
