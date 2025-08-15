/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyviewmodel.h"

#include "amplifyeffect.h"

#include "global/log.h"
#include "global/translation.h"

using namespace au::effects;

void AmplifyViewModel::doReload()
{
    auto& ae = effect<AmplifyEffect>();

    const db_t initAmp = ae.defaultAmp();
    ae.setAmp(initAmp);
    m_amp.val = std::numeric_limits<float>::lowest();

    m_canClip = ae.canClip();
    emit canClipChanged();

    update();
}

void AmplifyViewModel::update()
{
    const auto& ae = effect<AmplifyEffect>();

    Param<db_t> amp = ae.amp();
    db_t newPeak = muse::linear_to_db(ae.ratio() * ae.peak());

    if (!muse::is_equal(m_amp.val, amp.val)) {
        m_amp = amp;
        m_newPeak = newPeak;
        emit ampValueChanged();
        emit newPeakValueChanged();
    }

    bool isApplyAllowed = ae.isApplyAllowed();
    setIsApplyAllowed(isApplyAllowed);
}

QString AmplifyViewModel::effectTitle() const
{
    return muse::qtrc("effects/amplify", "Amplify");
}

QString AmplifyViewModel::ampLabel() const
{
    return muse::qtrc("effects/amplify", "Amplification");
}

float AmplifyViewModel::ampValue() const
{
    return m_amp.val;
}

void AmplifyViewModel::setAmpValue(float newAmpValue)
{
    db_t newAmp = newAmpValue;
    if (muse::is_equal(m_amp.val, newAmp)) {
        return;
    }

    auto& ae = effect<AmplifyEffect>();
    ae.setAmp(newAmp);

    update();
}

float AmplifyViewModel::ampMin() const
{
    return m_amp.min;
}

float AmplifyViewModel::ampMax() const
{
    return m_amp.max;
}

QString AmplifyViewModel::ampMeasureUnitsSymbol() const
{
    return muse::qtrc("global", "dB");
}

int AmplifyViewModel::ampDecimals() const
{
    return 4;
}

double AmplifyViewModel::ampStep() const
{
    return 0.02;
}

QString AmplifyViewModel::newPeakLabel() const
{
    return muse::qtrc("effects/amplify", "New peak amplitude");
}

float AmplifyViewModel::newPeakValue() const
{
    return m_newPeak;
}

void AmplifyViewModel::setNewPeakValue(float newNewPeakValue)
{
    db_t newNewPeak = newNewPeakValue;
    if (muse::is_equal(m_newPeak, newNewPeak)) {
        return;
    }

    auto& ae = effect<AmplifyEffect>();

    ratio_t ratio = muse::db_to_linear(newNewPeak) / ae.peak();
    db_t amp = muse::linear_to_db(ratio);
    ae.setAmp(amp);

    update();
}

float AmplifyViewModel::newPeakMin() const
{
    const auto& ae = effect<AmplifyEffect>();
    return m_amp.min + muse::linear_to_db(ae.peak());
}

float AmplifyViewModel::newPeakMax() const
{
    const auto& ae = effect<AmplifyEffect>();
    return m_amp.max + muse::linear_to_db(ae.peak());
}

QString AmplifyViewModel::newPeakMeasureUnitsSymbol() const
{
    return muse::qtrc("global", "dB");
}

int AmplifyViewModel::newPeakDecimals() const
{
    return 4;
}

double AmplifyViewModel::newPeakStep() const
{
    return 0.02;
}

QString AmplifyViewModel::canClipLabel() const
{
    return muse::qtrc("effects/amplify", "Allow clipping");
}

bool AmplifyViewModel::canClip() const
{
    return m_canClip;
}

void AmplifyViewModel::setCanClip(bool newClipping)
{
    if (m_canClip == newClipping) {
        return;
    }

    auto& ae = effect<AmplifyEffect>();

    ae.setCanClip(newClipping);

    m_canClip = newClipping;
    emit canClipChanged();

    update();
}

bool AmplifyViewModel::isApplyAllowed() const
{
    return m_isApplyAllowed;
}

void AmplifyViewModel::setIsApplyAllowed(bool isApplyAllowed)
{
    if (m_isApplyAllowed == isApplyAllowed) {
        return;
    }
    m_isApplyAllowed = isApplyAllowed;
    emit isApplyAllowedChanged();
}
