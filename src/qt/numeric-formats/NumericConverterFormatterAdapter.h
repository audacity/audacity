/*  SPDX-License-Identifier: GPL-2.0-or-later */

#pragma once

#include <QObject>
#include <QVariantList>
#include <QtQml/qqmlregistration.h>

#include "Observer.h"

struct NumericConverterFormatChangedMessage;
struct NumericConverterFormatter;

class NumericConverterFormatterAdapter final : public QObject
{
   Q_OBJECT
   QML_ELEMENT

   std::unique_ptr<NumericConverterFormatter> mFormatter;
public:
   NumericConverterFormatterAdapter(std::unique_ptr<NumericConverterFormatter> formatter);
   ~NumericConverterFormatterAdapter() override;

   Q_INVOKABLE
   QVariantList formatLayout() const;

   Q_INVOKABLE
   QVariantList valueToDigits(double value) const;

   Q_INVOKABLE
   double singleStep(double value, int digitIndex, bool forward) const;

signals:

   void formatChanged();

private:
   void onFormatChanged(NumericConverterFormatChangedMessage);

   Observer::Subscription mFormatChangedSubscription;
};
