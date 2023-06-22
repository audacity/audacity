/*  SPDX-License-Identifier: GPL-2.0-or-later */

#include "NumericConverterFormatterAdapter.h"

#include <QVariantMap>

#include "CodeConversions.h"
#include "NumericConverterFormatter.h"

NumericConverterFormatterAdapter::NumericConverterFormatterAdapter(std::unique_ptr<NumericConverterFormatter> formatter)
   : mFormatter(std::move(formatter))
{
   mFormatChangedSubscription =
      mFormatter->Subscribe(*this, &NumericConverterFormatterAdapter::onFormatChanged);
}

NumericConverterFormatterAdapter::~NumericConverterFormatterAdapter() = default;

QVariantList NumericConverterFormatterAdapter::formatLayout() const
{
   QVariantList result;
   for(auto& field : mFormatter->GetFields())
   {
      QVariantMap f;
      f["digits"] = static_cast<qulonglong>(field.digits);
      f["label"] = audacity::ToQString(field.label);
      result.push_back(f);
   }
   return result;
}

QVariantList NumericConverterFormatterAdapter::valueToDigits(double value) const
{
   auto convResult = mFormatter->ValueToString(value, true);

   QVariantList result;

   for(auto& digit : mFormatter->GetDigitInfos())
   {
      const auto ch = static_cast<char32_t>(convResult.valueString[digit.pos].GetValue());
      QVariantMap obj;
      obj["field"] = static_cast<qulonglong>(digit.field);
      obj["index"] = static_cast<qulonglong>(digit.index);
      obj["value"] = QString::fromUcs4(&ch, 1);
      result.push_back(obj);
   }
   return result;
}

Q_INVOKABLE
double NumericConverterFormatterAdapter::singleStep(double value, int digitIndex, bool forward) const
{
   return mFormatter->SingleStep(value, digitIndex, forward);
}

void NumericConverterFormatterAdapter::onFormatChanged(NumericConverterFormatChangedMessage)
{
   emit formatChanged();
}

