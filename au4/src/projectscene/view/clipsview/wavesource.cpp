#include "wavesource.h"

using namespace au::projectscene;
using namespace au::processing;

WaveSource::WaveSource(processing::Wave w)
    : m_wave(w)
{
}

const Wave& WaveSource::wave() const
{
    return m_wave;
}
