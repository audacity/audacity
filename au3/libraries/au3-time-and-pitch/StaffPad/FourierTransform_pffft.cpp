#include "FourierTransform_pffft.h"

#include "pffft.h"

namespace staffpad::audio {
FourierTransform::FourierTransform(int32_t newBlockSize)
    : _blockSize{newBlockSize}
{
    _pffft_scratch = (float*)pffft_aligned_malloc(_blockSize * sizeof(float));
    realFftSpec = pffft_new_setup(_blockSize, PFFFT_REAL);
}

FourierTransform::~FourierTransform()
{
    if (_pffft_scratch) {
        pffft_aligned_free(_pffft_scratch);
        _pffft_scratch = nullptr;
    }
    if (realFftSpec) {
        pffft_destroy_setup(realFftSpec);
        realFftSpec = nullptr;
    }
}

void FourierTransform::forwardReal(const SamplesReal& t, SamplesComplex& c)
{
    assert(t.getNumSamples() == _blockSize);

    for (auto ch = 0; ch < t.getNumChannels(); ++ch) {
        auto* spec = c.getPtr(ch); // interleaved complex numbers, size _blockSize + 2
        auto* cpx_flt = (float*)spec;
        pffft_transform_ordered(realFftSpec, t.getPtr(ch), cpx_flt, _pffft_scratch, PFFFT_FORWARD);
        // pffft combines dc and nyq values into the first complex value,
        // adjust to CCS format.
        auto dc = cpx_flt[0];
        auto nyq = cpx_flt[1];
        spec[0] = { dc, 0.f };
        spec[c.getNumSamples() - 1] = { nyq, 0.f };
    }
}

void FourierTransform::inverseReal(const SamplesComplex& c, SamplesReal& t)
{
    assert(c.getNumSamples() == _blockSize / 2 + 1);

    for (auto ch = 0; ch < c.getNumChannels(); ++ch) {
        auto* spec = c.getPtr(ch);
        // Use t to convert in-place from CCS to pffft format
        t.assignSamples(ch, (float*)spec);
        auto* ts = t.getPtr(ch);
        ts[0] = spec[0].real();
        ts[1] = spec[c.getNumSamples() - 1].real();
        pffft_transform_ordered(realFftSpec, ts, ts, _pffft_scratch, PFFFT_BACKWARD);
    }
}
} // namespace  staffpad::audio
