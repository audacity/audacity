#pragma once

#include <complex>
#include <vector>

#include "SimdTypes.h"
#include "VectorOps.h"

namespace staffpad {
template<typename T = float>
class SamplesFloat
{
public:
    ~SamplesFloat()
    {
        for (int ch = 0; ch < num_channels; ch++) {
            dealloc(ch);
        }
    }

    void setSize(int32_t numChannels, int32_t samples)
    {
        for (int ch = 0; ch < num_channels; ch++) {
            dealloc(ch);
        }

        num_channels = numChannels;
        num_samples = samples;
        data.resize(num_channels);
        for (int ch = 0; ch < num_channels; ch++) {
            alloc(ch, num_samples);
        }
    }

    int32_t getNumChannels() const
    {
        return num_channels;
    }

    int32_t getNumSamples() const
    {
        return num_samples;
    }

    float** getPtrs()
    {
        return data.data();
    }

    T* getPtr(int32_t channel)
    {
        assert(channel < num_channels);
        assert(data[channel]);
        return data[channel];
    }

    const T* getPtr(int32_t channel) const
    {
        assert(channel < num_channels);
        assert(data[channel]);
        return data[channel];
    }

    void assignSamples(int32_t channel, const T* input)
    {
        assert(channel < num_channels);
        assert(data[channel]);
        vo::copy(input, data[channel], num_samples);
    }

    void assignSamples(const SamplesFloat& rhs)
    {
        assert(num_channels == rhs.num_channels);
        assert(num_samples == rhs.num_samples);
        for (int ch = 0; ch < num_channels; ch++) {
            assert(data[ch]);
            vo::copy(rhs.getPtr(ch), getPtr(ch), num_samples);
        }
    }

    void zeroOut()
    {
        for (int ch = 0; ch < num_channels; ch++) {
            vo::setToZero(data[ch], num_samples);
        }
    }

private:
    int32_t num_channels{ 0 };
    int32_t num_samples{ 0 };
    std::vector<T*> data;

    void alloc(int32_t channel, int32_t samples)
    {
        assert(channel < num_channels);
        if (data[channel]) {
            dealloc(channel);
        }
        data[channel] = (T*)audio::simd::aligned_malloc(samples * sizeof(T), 64);
    }

    void dealloc(int32_t channel)
    {
        assert(channel < num_channels);
        if (data[channel]) {
            audio::simd::aligned_free(data[channel]);
            data[channel] = nullptr;
        }
    }
};

typedef SamplesFloat<float> SamplesReal;
typedef SamplesFloat<std::complex<float> > SamplesComplex;
} // namespace staffpad
