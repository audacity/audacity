/*  SPDX-License-Identifier: GPL-2.0-or-later */

#pragma once

#include <QObject>
#include <QStringList>
#include <QtQml/qqmlregistration.h>

#include "NumericConverterFormatterContext.h"

class NumericConverterFormatterAdapter;

class NumericConverterFormatterAdapterFactory final : public QObject
{
   Q_OBJECT
   QML_ELEMENT

   const FormatterContext mFormatterContext;
public:
   NumericConverterFormatterAdapterFactory(const FormatterContext& context, QObject* parent = nullptr);
   ~NumericConverterFormatterAdapterFactory() override;

   Q_INVOKABLE
   QStringList formats(const QString& type) const;

   Q_INVOKABLE
   NumericConverterFormatterAdapter* create(const QString& type, int index) const;
};


