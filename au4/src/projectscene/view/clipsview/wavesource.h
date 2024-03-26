#ifndef AU_PROJECTSCENE_WAVESOURCE_H
#define AU_PROJECTSCENE_WAVESOURCE_H

#include <QObject>

#include "processing/dom/wave.h"

namespace au::projectscene {
class WaveSource
{
    Q_GADGET

public:
    WaveSource() = default;
    WaveSource(processing::Wave w);

    const processing::Wave& wave() const;

private:

    processing::Wave m_wave;
};
}

#endif // AU_PROJECTSCENE_WAVESOURCE_H
