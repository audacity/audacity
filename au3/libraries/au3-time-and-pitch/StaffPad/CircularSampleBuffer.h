#pragma once

#include <cassert>

#include "VectorOps.h"

/*
 * Utility buffer class for delay-based effects
 * Allocates a 2^n size buffer for branchless write and read using a bitmask
 * It can read and write to past and future samples, but does not check for overlaps
 */

namespace staffpad::audio {
template<typename SampleT>
class CircularSampleBuffer
{
public:
    CircularSampleBuffer()
    {
    }

    ~CircularSampleBuffer()
    {
        if (_buffer) {
            free(_buffer);
        }
    }

    void setSize(int n)
    {
        if (n > _allocatedSize) {
            auto oldSize = _allocatedSize;

            auto findLargerPowerOfTwo = [](int32_t number) {
                int32_t powerOf2 = 1;
                while (powerOf2 < number) {
                    powerOf2 *= 2;
                }
                return powerOf2;
            };

            _allocatedSize = findLargerPowerOfTwo(n);
            _bufferSizeMask = _allocatedSize - 1;
            _buffer = (SampleT*)std::realloc(_buffer, _allocatedSize * sizeof(SampleT));

            // Reset the new memory region
            assert(_buffer);
            std::fill(_buffer + oldSize, _buffer + _allocatedSize, 0.f);
        }
    }

    int getAllocatedSize() const
    {
        return _allocatedSize;
    }

    void reset()
    {
        if (_buffer && _allocatedSize > 0) {
            memset(_buffer, 0, sizeof(SampleT) * _allocatedSize);
        }
        _position0 = 0;
    }

    void write(int offset, const SampleT& sample)
    {
        _buffer[(_position0 + offset) & _bufferSizeMask] = sample;
    }

    void writeOffset0(const SampleT& sample)
    {
        _buffer[_position0] = sample;
    }

    const SampleT& read(int offset) const
    {
        return _buffer[(_position0 + offset) & _bufferSizeMask];
    }

    /// change the 0 position by n
    void advance(int n)
    {
        _position0 += n;
        _position0 &= _bufferSizeMask;
    }

private:
    template<typename fnc>
    void _splitBlockOffsetFunction(int startOffset, int n, fnc f) const
    {
        assert(n <= _allocatedSize);
        int firstIndex = (_position0 + startOffset) & _bufferSizeMask;
        int n_to_end = _allocatedSize - firstIndex;
        if (n_to_end > n) {
            f(firstIndex, 0, n);
        } else {
            f(firstIndex, 0, n_to_end);
            f(0, n_to_end, n - n_to_end);
        }
    }

public:
    void writeBlock(int startOffset, int n, const SampleT* sourceBlock)
    {
        _splitBlockOffsetFunction(startOffset, n, [=](int bufferOff, int sampleOff, int n) {
            vo::copy(&sourceBlock[sampleOff], &_buffer[bufferOff], n);
        });
    }

    void readBlockWithGain(int startOffset, int n, SampleT* targetBlock, float gainFactor) const
    {
        _splitBlockOffsetFunction(startOffset, n, [=](int bufferOff, int sampleOff, int n) {
            vo::constantMultiply(&_buffer[bufferOff], gainFactor, &targetBlock[sampleOff], n);
        });
    }

    void readAddBlockWithGain(int startOffset, int n, SampleT* targetBlock, float gainFactor) const
    {
        _splitBlockOffsetFunction(startOffset, n, [=](int bufferOff, int sampleOff, int n) {
            vo::constantMultiplyAndAdd(&_buffer[bufferOff], gainFactor, &targetBlock[sampleOff], n);
        });
    }

    void writeAddBlockWithGain(int startOffset, int n, const SampleT* sourceBlock, float gainFactor)
    {
        _splitBlockOffsetFunction(startOffset, n, [=](int bufferOff, int sampleOff, int n) {
            vo::constantMultiplyAndAdd(&sourceBlock[sampleOff], gainFactor, &_buffer[bufferOff], n);
        });
    }

    void readBlock(int startOffset, int n, SampleT* targetBlock) const
    {
        _splitBlockOffsetFunction(startOffset, n, [=](int bufferOff, int sampleOff, int n) {
            vo::copy(&_buffer[bufferOff], &targetBlock[sampleOff], n);
        });
    }

    void readAndClearBlock(int startOffset, int n, SampleT* targetBlock)
    {
        _splitBlockOffsetFunction(startOffset, n, [=](int bufferOff, int sampleOff, int n) {
            vo::copy(&_buffer[bufferOff], &targetBlock[sampleOff], n);
            vo::setToZero(&_buffer[bufferOff], n);
        });
    }

    void clearBlock(int startOffset, int n)
    {
        _splitBlockOffsetFunction(startOffset, n,
                                  [=](int bufferOff, int sampleOff, int n) { vo::setToZero(&_buffer[bufferOff], n); });
    }

private:
    SampleT* _buffer = nullptr;

    int _position0 = 0;   // position of sample index 0 inside _buffer. This is where the next sample will be written to
    int _allocatedSize = 0; // 2^n buffer size
    int _bufferSizeMask = 0; // 2^n-1 buffer mask
};
} // namespace staffpad::audio
