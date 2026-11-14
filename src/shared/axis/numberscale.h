/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "axisscale.h"

#include <algorithm>
#include <cassert>
#include <cmath>

namespace au::shared {
class NumberScale
{
public:
    NumberScale()
        : mType(AxisScale::Undefined), mValue0(0), mValue1(1)
    {}

    NumberScale(AxisScale type, double value0, double value1)
        : NumberScale(type, static_cast<float>(value0), static_cast<float>(value1)) {}

    NumberScale(AxisScale type, float value0, float value1)
        : mType(type)
    {
        switch (mType) {
        case AxisScale::Linear:
        case AxisScale::Undefined:
        {
            mValue0 = value0;
            mValue1 = value1;
        }
        break;
        case AxisScale::Logarithmic:
        {
            mValue0 = logf(value0);
            mValue1 = logf(value1);
        }
        break;
        case AxisScale::Mel:
        {
            mValue0 = hzToMel(value0);
            mValue1 = hzToMel(value1);
        }
        break;
        case AxisScale::Bark:
        {
            mValue0 = hzToBark(value0);
            mValue1 = hzToBark(value1);
        }
        break;
        case AxisScale::ERB:
        {
            mValue0 = hzToErb(value0);
            mValue1 = hzToErb(value1);
        }
        break;
        case AxisScale::Period:
        {
            mValue0 = hzToPeriod(value0);
            mValue1 = hzToPeriod(value1);
        }
        break;
        default:
            assert(false);
        }
    }

    NumberScale reversal() const
    {
        NumberScale result(*this);
        std::swap(result.mValue0, result.mValue1);
        return result;
    }

    bool operator ==(const NumberScale& other) const
    {
        return mType == other.mType
               && mValue0 == other.mValue0
               && mValue1 == other.mValue1;
    }

    bool operator !=(const NumberScale& other) const
    {
        return !(*this == other);
    }

    static inline float hzToMel(float hz)
    {
        return 1127 * log(1 + hz / 700);
    }

    static inline float melToHz(float mel)
    {
        return 700 * (exp(mel / 1127) - 1);
    }

    static inline float hzToBark(float hz)
    {
        // Traunmueller's formula
        const float z1 = 26.81 * hz / (1960 + hz) - 0.53;
        if (z1 < 2.0) {
            return z1 + 0.15 * (2.0 - z1);
        } else if (z1 > 20.1) {
            return z1 + 0.22 * (z1 - 20.1);
        } else {
            return z1;
        }
    }

    static inline float barkToHz(float z1)
    {
        if (z1 < 2.0) {
            z1 = 2.0 + (z1 - 2.0) / 0.85;
        } else if (z1 > 20.1) {
            z1 = 20.1 + (z1 - 20.1) / 1.22;
        }
        return 1960 * (z1 + 0.53) / (26.28 - z1);
    }

    static inline float hzToErb(float hz)
    {
        return 11.17268 * log(1 + (46.06538 * hz) / (hz + 14678.49));
    }

    static inline float erbToHz(float erb)
    {
        return 676170.4 / (47.06538 - exp(0.08950404 * erb)) - 14678.49;
    }

    static inline float hzToPeriod(float hz)
    {
        return -1.0 / std::max(1.0f, hz);
    }

    static inline float periodToHz(float u)
    {
        return -1.0 / u;
    }

    // Random access
    float positionToValue(float pp) const
    {
        switch (mType) {
        default:
            assert(false);
        case AxisScale::Linear:
        case AxisScale::Undefined:
            return mValue0 + pp * (mValue1 - mValue0);
        case AxisScale::Logarithmic:
            return exp(mValue0 + pp * (mValue1 - mValue0));
        case AxisScale::Mel:
            return melToHz(mValue0 + pp * (mValue1 - mValue0));
        case AxisScale::Bark:
            return barkToHz(mValue0 + pp * (mValue1 - mValue0));
        case AxisScale::ERB:
            return erbToHz(mValue0 + pp * (mValue1 - mValue0));
        case AxisScale::Period:
            return periodToHz(mValue0 + pp * (mValue1 - mValue0));
        }
    }

    // STL-idiom iteration

    class Iterator
    {
    public:
        Iterator(AxisScale type, float step, float value)
            : mType(type), mStep(step), mValue(value)
        {
        }

        float operator *() const
        {
            switch (mType) {
            default:
                assert(false);
            case AxisScale::Linear:
            case AxisScale::Undefined:
            case AxisScale::Logarithmic:
                return mValue;
            case AxisScale::Mel:
                return melToHz(mValue);
            case AxisScale::Bark:
                return barkToHz(mValue);
            case AxisScale::ERB:
                return erbToHz(mValue);
            case AxisScale::Period:
                return periodToHz(mValue);
            }
        }

        Iterator& operator ++()
        {
            switch (mType) {
            case AxisScale::Linear:
            case AxisScale::Undefined:
            case AxisScale::Mel:
            case AxisScale::Bark:
            case AxisScale::ERB:
            case AxisScale::Period:
                mValue += mStep;
                break;
            case AxisScale::Logarithmic:
                mValue *= mStep;
                break;
            default:
                assert(false);
            }
            return *this;
        }

    private:
        const AxisScale mType;
        const float mStep;
        float mValue;
    };

    Iterator begin(float nPositions) const
    {
        switch (mType) {
        default:
            assert(false);
        case AxisScale::Linear:
        case AxisScale::Undefined:
        case AxisScale::Mel:
        case AxisScale::Bark:
        case AxisScale::ERB:
        case AxisScale::Period:
            return Iterator
                       (mType,
                       nPositions == 1 ? 0 : (mValue1 - mValue0) / (nPositions - 1),
                       mValue0);
        case AxisScale::Logarithmic:
            return Iterator
                       (mType,
                       nPositions == 1 ? 1 : exp((mValue1 - mValue0) / (nPositions - 1)),
                       exp(mValue0));
        }
    }

    // Inverse
    float valueToPosition(float val) const
    {
        switch (mType) {
        default:
            assert(false);
        case AxisScale::Linear:
        case AxisScale::Undefined:
            return (val - mValue0) / (mValue1 - mValue0);
        case AxisScale::Logarithmic:
            return (log(val) - mValue0) / (mValue1 - mValue0);
        case AxisScale::Mel:
            return (hzToMel(val) - mValue0) / (mValue1 - mValue0);
        case AxisScale::Bark:
            return (hzToBark(val) - mValue0) / (mValue1 - mValue0);
        case AxisScale::ERB:
            return (hzToErb(val) - mValue0) / (mValue1 - mValue0);
        case AxisScale::Period:
            return (hzToPeriod(val) - mValue0) / (mValue1 - mValue0);
        }
    }

private:
    const AxisScale mType;
    float mValue0;
    float mValue1;
};
}
