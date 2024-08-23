/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyviewmodel.h"

#include "amplifyeffect.h"

#include "log.h"

using namespace au::effects;

void AmplifyViewModel::init()
{
    IF_ASSERT_FAILED(!instanceId().isEmpty()) {
        return;
    }

    //! NOTE Temporary solution
    m_effect = reinterpret_cast<AmplifyEffect*>(instanceId().toLongLong());
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
