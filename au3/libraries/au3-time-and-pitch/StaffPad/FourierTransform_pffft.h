// FFT wrapper for the PFFFT library. It is possible to use different wrappers for
// other FFT libraries or platforms, as long as the CSS complex data format is used.

#pragma once

#include <stdint.h>

#include "SamplesFloat.h"

struct PFFFT_Setup;

namespace staffpad::audio {
class FourierTransform
{
public:
    FourierTransform(int32_t newBlockSize);
    ~FourierTransform();

    int getSize() const { return static_cast<int>(_blockSize); }

    void forwardReal(const SamplesReal& t, SamplesComplex& c);
    void inverseReal(const SamplesComplex& c, SamplesReal& t);

private:
    PFFFT_Setup* realFftSpec = nullptr;
    PFFFT_Setup* complexFftSpec = nullptr;
    float* _pffft_scratch = nullptr;

    const int32_t _blockSize;
    int32_t _order = 0;
};
} // namespace staffpad::audio
