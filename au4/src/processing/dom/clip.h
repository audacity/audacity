#ifndef AU_PROСESSING_CLIP_H
#define AU_PROСESSING_CLIP_H

#include <vector>

#include "wave.h"

namespace au::processing {
class Clip
{
public:
    Clip() = default;

    const Wave& wave() const;
    void setWave(const Wave& w);

private:

    Wave m_wave;
};

using Clips = std::vector<Clip>;
}

#endif // AU_PROСESSING_CLIP_H
