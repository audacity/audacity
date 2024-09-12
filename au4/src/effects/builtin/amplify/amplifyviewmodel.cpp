/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyviewmodel.h"

#include "global/types/number.h"

#include "amplifyeffect.h"

#include "log.h"

using namespace au::effects;

AmplifyEffect* AmplifyViewModel::effect() const
{
    AmplifyEffect* e = dynamic_cast<AmplifyEffect*>(AbstractEffectModel::effect());
    IF_ASSERT_FAILED(e) {
        return nullptr;
    }
    return e;
}

void AmplifyViewModel::init()
{
    AmplifyEffect* ae = effect();
    IF_ASSERT_FAILED(ae) {
        return;
    }

    const double initAmp = ae->defaultAmp();
    ae->setAmp(initAmp);
    m_amp.val = std::numeric_limits<double>::lowest();

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

    Param<double> amp = ae->amp();
    if (!muse::is_equal(m_amp.val, amp.val)) {
        m_amp = amp;
        emit ampChanged();
    }

    double newPeak = LINEAR_TO_DB(ae->ratio() * ae->peak());
    if (!muse::is_equal(m_newPeak, newPeak)) {
        m_newPeak = newPeak;
        emit newPeakChanged();
    }

    bool isApplyAllowed = ae->isApplyAllowed();
    setIsApplyAllowed(isApplyAllowed);
}

double AmplifyViewModel::amp() const
{
    return m_amp.val;
}

void AmplifyViewModel::setAmp(double newAmp)
{
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

double AmplifyViewModel::ampMin() const
{
    return m_amp.min;
}

double AmplifyViewModel::ampMax() const
{
    return m_amp.max;
}

double AmplifyViewModel::newPeak() const
{
    return m_newPeak;
}

void AmplifyViewModel::setNewPeak(double newNewPeak)
{
    if (muse::is_equal(m_newPeak, newNewPeak)) {
        return;
    }

    AmplifyEffect* ae = effect();
    IF_ASSERT_FAILED(ae) {
        return;
    }

    double ratio = DB_TO_LINEAR(newNewPeak) / ae->peak();
    double amp = LINEAR_TO_DB(ratio);
    ae->setAmp(amp);

    update();
}

double AmplifyViewModel::newPeakMin() const
{
    AmplifyEffect* ae = effect();
    IF_ASSERT_FAILED(ae) {
        return 0.0;
    }

    return m_amp.min + LINEAR_TO_DB(ae->peak());
}

double AmplifyViewModel::newPeakMax() const
{
    AmplifyEffect* ae = effect();
    IF_ASSERT_FAILED(ae) {
        return 0.0;
    }

    return m_amp.max + LINEAR_TO_DB(ae->peak());
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
