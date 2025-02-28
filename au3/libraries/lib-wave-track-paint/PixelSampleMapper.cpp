/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  PixelSampleMapper.h

  Dmitry Vedenko

**********************************************************************/
#include "PixelSampleMapper.h"

#include <cassert>
#include <cmath>

#include "SampleCount.h"
#include "Variant.h"

PixelSampleMapper::PixelSampleMapper(
    double t0, double rate, double samplesPerPixel) noexcept
    : mMapper(LinearMapper { (0.5 + t0 * rate), samplesPerPixel })
{
    assert((0.5 + t0 * rate) >= 0.0);
}

void PixelSampleMapper::applyBias(double bias) noexcept
{
    auto mapper = std::get_if<LinearMapper>(&mMapper);

    if (mapper != nullptr) {
        mapper->mInitialValue += bias;
    }
}

double PixelSampleMapper::applyCorrection(
    const PixelSampleMapper& oldMapper, size_t oldLen, size_t newLen)
{
    assert(mMapper.index() == 0);
    assert(oldMapper.mMapper.index() == 0);

    LinearMapper* currentMapper = std::get_if<LinearMapper>(&mMapper);

    if (currentMapper == nullptr) {
        return {};
    }

    const LinearMapper* oldLinearMapper = std::get_if<LinearMapper>(&oldMapper.mMapper);

    if (oldLinearMapper == nullptr) {
        return {};
    }

    // Find the sample position that is the origin in the old cache.
    const double oldWhere0 =(*oldLinearMapper)(1).as_double() - currentMapper->mSamplesPerPixel;
    const double oldWhereLast = oldWhere0 + oldLen * currentMapper->mSamplesPerPixel;
    // Find the length in samples of the old cache.
    const double denom = oldWhereLast - oldWhere0;

    // What sample would go in where[0] with no correction?
    const double guessWhere0
        =currentMapper->mInitialValue - 0.5; // Why do we ignore initial bias?

    if ( // Skip if old and NEW are disjoint:
        oldWhereLast <= guessWhere0
        || guessWhere0 + newLen * currentMapper->mSamplesPerPixel <= oldWhere0
        ||// Skip unless denom rounds off to at least 1.
        denom < 0.5) {
        // The computation of oldX0 in the other branch
        // may underflow and the assertion would be violated.
        return oldLen;
    } else {
        // What integer position in the old cache array does that map to?
        // (even if it is out of bounds)
        const auto oldX0
            =std::floor(0.5 + oldLen * (guessWhere0 - oldWhere0) / denom);
        // What sample count would the old cache have put there?
        const double where0
            =oldWhere0 + double(oldX0) * currentMapper->mSamplesPerPixel;
        // What correction is needed to align the NEW cache with the old?
        const double correction0 = where0 - guessWhere0;
        const double correction = std::max(
            -currentMapper->mSamplesPerPixel,
            std::min(currentMapper->mSamplesPerPixel, correction0));

        assert(correction == correction0);

        currentMapper->mInitialValue += correction;

        return oldX0;
    }
}

sampleCount PixelSampleMapper::GetFirstSample(uint32_t column) const
{
    return Variant::Visit(
        [column](const auto& mapper) { return mapper(column); }, mMapper);
}

sampleCount PixelSampleMapper::GetLastSample(uint32_t column) const
{
    return GetFirstSample(column + 1);
}

std::pair<sampleCount, sampleCount>
PixelSampleMapper::GetSampleRange(uint32_t column) const
{
    return { GetFirstSample(column), GetLastSample(column) };
}

void PixelSampleMapper::setCustomMapper(CustomMapper mapper)
{
    mMapper = std::move(mapper);
}

bool PixelSampleMapper::IsValid() const
{
    return Variant::Visit([](const auto& mapper) { return !!mapper; }, mMapper);
}

bool PixelSampleMapper::IsLinear() const noexcept
{
    return std::get_if<LinearMapper>(&mMapper) != nullptr;
}

sampleCount
PixelSampleMapper::LinearMapper::operator()(uint32_t column) const noexcept
{
    // Previous code used floor, but "required" mInitialValue to be positive.
    // For positive values, let's just trunc the value, as it is at least twice
    // as fast.
    return sampleCount(mInitialValue + column * mSamplesPerPixel);
}

PixelSampleMapper::LinearMapper::operator bool() const noexcept
{
    return mSamplesPerPixel > 0.0;
}
