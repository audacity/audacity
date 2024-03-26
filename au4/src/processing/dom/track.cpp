#include "track.h"

using namespace au::processing;

const Clips& Track::clips() const
{
    return m_clips;
}

void Track::setClips(const Clips& c)
{
    m_clips = c;
}
