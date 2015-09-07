/**********************************************************************

Audacity: A Digital Audio Editor

NumberScale.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_NUMBER_SCALE__
#define __AUDACITY_NUMBER_SCALE__

#include <algorithm>
#include <cmath>
#include <wx/defs.h>
#include <wx/debug.h>

enum NumberScaleType {
   nstLinear,
   nstLogarithmic,
   nstMel,
   nstBark,
   nstErb,
   nstPeriod,

   nstNumScaleTypes,
};


class NumberScale
{
public:
   NumberScale()
      : mType(nstLinear), mValue0(0), mValue1(1), mUnit(1)
   {}

   NumberScale(NumberScaleType type,
      float value0, float value1, float unit)
      : mType(type)
   {
      switch (mType) {
      case nstLinear:
      {
         mValue0 = value0 / unit;
         mValue1 = value1 / unit;
         mUnit = 1.0;
      }
      break;
      case nstLogarithmic:
      {
         mValue0 = logf(value0 / unit);
         mValue1 = logf(value1 / unit);
         mUnit = 1.0;
      }
      break;
      case nstMel:
      {
         mValue0 = hzToMel(value0);
         mValue1 = hzToMel(value1);
         mUnit = unit;
      }
      break;
      case nstBark:
      {
         mValue0 = hzToBark(value0);
         mValue1 = hzToBark(value1);
         mUnit = unit;
      }
      break;
      case nstErb:
      {
         mValue0 = hzToErb(value0);
         mValue1 = hzToErb(value1);
         mUnit = unit;
      }
      break;
      case nstPeriod:
      {
         mValue0 = hzToPeriod(value0);
         mValue1 = hzToPeriod(value1);
         mUnit = unit;
      }
      break;
      default:
         wxASSERT(false);
      }
   }

   NumberScale Reversal() const
   {
      NumberScale result(*this);
      std::swap(result.mValue0, result.mValue1);
      return result;
   }

   bool operator == (const NumberScale& other) const
   {
      return mType == other.mType
         && mValue0 == other.mValue0
         && mValue1 == other.mValue1
         && mUnit == other.mUnit;
   }

   bool operator != (const NumberScale &other) const
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
      if (z1 < 2.0)
         return z1 + 0.15 * (2.0 - z1);
      else if (z1 > 20.1)
         return z1 + 0.22 * (z1 - 20.1);
      else
         return z1;
   }

   static inline float barkToHz(float z1)
   {
      if (z1 < 2.0)
         z1 = 2.0 + (z1 - 2.0) / 0.85;
      else if (z1 > 20.1)
         z1 = 20.1 + (z1 - 20.1) / 1.22;
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
      return -1.0 / std::max (1.0f, hz);
   }

   static inline float periodToHz(float u)
   {
      return -1.0 / u;
   }

   // Random access
   float PositionToValue(float pp) const
   {
      switch (mType) {
      default:
         wxASSERT(false);
      case nstLinear:
         return mValue0 + pp * (mValue1 - mValue0);
      case nstLogarithmic:
         return exp(mValue0 + pp * (mValue1 - mValue0));
      case nstMel:
         return melToHz(mValue0 + pp * (mValue1 - mValue0)) / mUnit;
      case nstBark:
         return barkToHz(mValue0 + pp * (mValue1 - mValue0)) / mUnit;
      case nstErb:
         return erbToHz(mValue0 + pp * (mValue1 - mValue0)) / mUnit;
      case nstPeriod:
         return periodToHz(mValue0 + pp * (mValue1 - mValue0)) / mUnit;
      }
   }

   // STL-idiom iteration

   class Iterator
   {
   public:
      Iterator(NumberScaleType type, float step, float value, float unit)
         : mType(type), mStep(step), mValue(value), mUnit(unit)
      {
      }

      float operator * () const
      {
         switch (mType) {
         default:
            wxASSERT(false);
         case nstLinear:
         case nstLogarithmic:
            return mValue;
         case nstMel:
            return melToHz(mValue) / mUnit;
         case nstBark:
            return barkToHz(mValue) / mUnit;
         case nstErb:
            return erbToHz(mValue) / mUnit;
         case nstPeriod:
            return periodToHz(mValue) / mUnit;
         }
      }

      Iterator &operator ++()
      {
         switch (mType) {
         case nstLinear:
         case nstMel:
         case nstBark:
         case nstErb:
         case nstPeriod:
            mValue += mStep;
            break;
         case nstLogarithmic:
            mValue *= mStep;
            break;
         default:
            wxASSERT(false);
         }
         return *this;
      }

   private:
      const NumberScaleType mType;
      const float mStep;
      float mValue;
      float mUnit;
   };

   Iterator begin(float nPositions) const
   {
      switch (mType) {
      default:
         wxASSERT(false);
      case nstLinear:
      case nstMel:
      case nstBark:
      case nstErb:
      case nstPeriod:
         return Iterator
            (mType,
             nPositions == 1 ? 0 : (mValue1 - mValue0) / (nPositions - 1),
             mValue0, mUnit);
      case nstLogarithmic:
         return Iterator
            (mType,
             nPositions == 1 ? 1 : exp((mValue1 - mValue0) / (nPositions - 1)),
             exp(mValue0), mUnit);
      }
   }

   // Inverse
   float ValueToPosition(float val) const
   {
      switch (mType) {
      default:
         wxASSERT(false);
      case nstLinear:
         return ((val - mValue0) / (mValue1 - mValue0));
      case nstLogarithmic:
         return ((log(val) - mValue0) / (mValue1 - mValue0));
      case nstMel:
         return ((hzToMel(val * mUnit) - mValue0) / (mValue1 - mValue0));
      case nstBark:
         return ((hzToBark(val * mUnit) - mValue0) / (mValue1 - mValue0));
      case nstErb:
         return ((hzToErb(val * mUnit) - mValue0) / (mValue1 - mValue0));
      case nstPeriod:
         return ((hzToPeriod(val * mUnit) - mValue0) / (mValue1 - mValue0));
      }
   }

private:
   NumberScaleType mType;
   float mValue0;
   float mValue1;
   float mUnit;
};

#endif
