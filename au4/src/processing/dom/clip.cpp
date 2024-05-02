#include "clip.h"

using namespace au::processing;

const Wave& Clip::wave() const
{
    return m_wave;
}

void Clip::setWave(const Wave& w)
{
    m_wave = w;
}
