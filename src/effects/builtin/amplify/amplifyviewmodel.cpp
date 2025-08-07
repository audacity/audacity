/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyviewmodel.h"

#include "amplifyeffect.h"

#include "global/log.h"
#include "global/translation.h"

using namespace au::effects;

AmplifyEffect* AmplifyViewModel::effect() const
{
    EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* e = effectsProvider()->effect(effectId);
    AmplifyEffect* ae = dynamic_cast<AmplifyEffect*>(e);
    return ae;
}

void AmplifyViewModel::doReload()
{
    AmplifyEffect* ae = effect();
    IF_ASSERT_FAILED(ae) {
        return;
    }

    const db_t initAmp = ae->defaultAmp();
    ae->setAmp(initAmp);
    m_amp.val = std::numeric_limits<float>::lowest();

    m_canClip = ae->canClip();
    emit canClipChanged();

    update();
}

void AmplifyViewModel::update()
{
    AmplifyEffect* ae = effect();
    IF_ASSERT_FAILED(ae) {
        return;
    }

    Param<db_t> amp = ae->amp();
    db_t newPeak = muse::linear_to_db(ae->ratio() * ae->peak());

    if (!muse::is_equal(m_amp.val, amp.val)) {
        m_amp = amp;
        m_newPeak = newPeak;
        emit ampChanged();
        emit newPeakChanged();
    }

    bool isApplyAllowed = ae->isApplyAllowed();
    setIsApplyAllowed(isApplyAllowed);
}

QString AmplifyViewModel::title() const
{
    return muse::qtrc("effects/amplify", "Amplify");
}

QString AmplifyViewModel::ampLabel() const
{
    return muse::qtrc("effects/amplify", "Amplification");
}

float AmplifyViewModel::amp() const
{
    return m_amp.val;
}

void AmplifyViewModel::setAmp(float newAmp_)
{
    db_t newAmp = newAmp_;
    if (muse::is_equal(m_amp.val, newAmp)) {
        return;
    }

    AmplifyEffect* ae = effect();
    IF_ASSERT_FAILED(ae) {
        return;
    }

    ae->setAmp(newAmp);

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

float AmplifyViewModel::newPeak() const
{
    return m_newPeak;
}

void AmplifyViewModel::setNewPeak(float newNewPeak_)
{
    db_t newNewPeak = newNewPeak_;
    if (muse::is_equal(m_newPeak, newNewPeak)) {
        return;
    }

    AmplifyEffect* ae = effect();
    IF_ASSERT_FAILED(ae) {
        return;
    }

    ratio_t ratio = muse::db_to_linear(newNewPeak) / ae->peak();
    db_t amp = muse::linear_to_db(ratio);
    ae->setAmp(amp);

    update();
}

float AmplifyViewModel::newPeakMin() const
{
    AmplifyEffect* ae = effect();
    if (!ae) {
        return 0.0;
    }

    return m_amp.min + muse::linear_to_db(ae->peak());
}

float AmplifyViewModel::newPeakMax() const
{
    AmplifyEffect* ae = effect();
    if (!ae) {
        return 0.0;
    }

    return m_amp.max + muse::linear_to_db(ae->peak());
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

void AmplifyViewModel::setCanClip(bool newCliping)
{
    if (m_canClip == newCliping) {
        return;
    }

    AmplifyEffect* ae = effect();
    IF_ASSERT_FAILED(ae) {
        return;
    }

    //! NOTE
    ae->setCanClip(newCliping);

    m_canClip = newCliping;
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
