/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyviewmodel.h"

#include "amplifyeffect.h"

#include "log.h"

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
        emit ampChanged();
        emit newPeakChanged();
    }

    bool isApplyAllowed = ae.isApplyAllowed();
    setIsApplyAllowed(isApplyAllowed);
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

bool AmplifyViewModel::canClip() const
{
    return m_canClip;
}

void AmplifyViewModel::setCanClip(bool newCliping)
{
    if (m_canClip == newCliping) {
        return;
    }

    auto& ae = effect<AmplifyEffect>();

    ae.setCanClip(newCliping);

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
