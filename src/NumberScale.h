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

   nstNumScaleTypes,
};


class NumberScale
{
public:
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
         }
      }

      Iterator &operator ++()
      {
         switch (mType) {
         case nstLinear:
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
         return Iterator
            (mType, (mValue1 - mValue0) / nPositions, mValue0, mUnit);
      case nstLogarithmic:
         return Iterator
            (mType, exp((mValue1 - mValue0) / nPositions), exp(mValue0), mUnit);
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
      }
   }

private:
   const NumberScaleType mType;
   float mValue0;
   float mValue1;
   float mUnit;
};

#endif
