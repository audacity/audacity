/*
 * Audacity: A Digital Audio Editor
 */
#include "generatoreffect.h"
#include "libraries/lib-components/EffectInterface.h"
#include "playback/iaudiooutput.h"
#include "trackedit/itrackeditproject.h"
#include "log.h"

using namespace au::effects;

GeneratorEffect::GeneratorEffect(const double& t0, double& t1)
    : m_t0{t0}
    , m_t1{t1}
{
}

void GeneratorEffect::init(EffectSettings* settings)
{
    IF_ASSERT_FAILED(settings) {
        return;
    }
    m_settings = settings;
    setDuration(settings->extra.GetDuration());
    doInit();
}

double GeneratorEffect::sampleRate() const
{
    return playback()->audioOutput()->sampleRate();
}

double GeneratorEffect::tempo() const
{
    return globalContext()->currentTrackeditProject()->timeSignature().tempo;
}

int GeneratorEffect::upperTimeSignature() const
{
    return globalContext()->currentTrackeditProject()->timeSignature().upper;
}

int GeneratorEffect::lowerTimeSignature() const
{
    return globalContext()->currentTrackeditProject()->timeSignature().lower;
}

double GeneratorEffect::duration() const
{
    // Rely on settings for the duration: we don't have a signal when it gets changed, so we can't expect `m_t1` to be up-to-date.
    return m_settings ? m_settings->extra.GetDuration() : 0.0;
}

void GeneratorEffect::setDuration(double newDuration)
{
    m_t1 = m_t0 + newDuration;
    if (m_settings) {
        m_settings->extra.SetDuration(newDuration);
    }
}

bool GeneratorEffect::isApplyAllowed() const
{
    return m_settings ? m_settings->extra.GetDuration() > 0 : false;
}

QString GeneratorEffect::durationFormat() const
{
    if (m_settings) {
        return QString::fromStdString(
            m_settings->extra.GetDurationFormat().GET().ToStdString());
    }
    return "";
}

void GeneratorEffect::setDurationFormat(const QString& newDurationFormat)
{
    if (m_settings) {
        m_settings->extra.SetDurationFormat(NumericFormatID { newDurationFormat.toStdString() });
    }
}
