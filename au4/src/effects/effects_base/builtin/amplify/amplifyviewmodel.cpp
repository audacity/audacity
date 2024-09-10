/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyviewmodel.h"

#include "amplifyeffect.h"

#include "log.h"

using namespace au::effects;

void AmplifyViewModel::init()
{
    m_effect = dynamic_cast<AmplifyEffect*>(this->effect());
    IF_ASSERT_FAILED(m_effect) {
        return;
    }
}

double AmplifyViewModel::ratio() const
{
    return m_effect ? m_effect->ratio() : 0.0;
}

void AmplifyViewModel::setRatio(double newRatio)
{
    if (!m_effect) {
        return;
    }

    if (qFuzzyCompare(m_effect->ratio(), newRatio)) {
        return;
    }

    m_effect->setRatio(newRatio);
    emit ratioChanged();
}
