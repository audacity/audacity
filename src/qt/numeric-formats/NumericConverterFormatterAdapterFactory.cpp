/*  SPDX-License-Identifier: GPL-2.0-or-later */

#include "NumericConverterFormatterAdapterFactory.h"

#include <QQmlContext>

#include "CodeConversions.h"
#include "NumericConverterFormatter.h"

#include "NumericConverterFormatterAdapter.h"
#include "NumericConverterRegistry.h"
#include "ProjectQMLEnvironment.h"

NumericConverterFormatterAdapterFactory::NumericConverterFormatterAdapterFactory(const FormatterContext& context, QObject* parent)
      : QObject(parent), mFormatterContext(context)
{
   
}

NumericConverterFormatterAdapterFactory::~NumericConverterFormatterAdapterFactory() = default;

QStringList NumericConverterFormatterAdapterFactory::formats(const QString& type) const
{
   QStringList result;
   NumericConverterRegistry::Visit(mFormatterContext, audacity::ToWXString(type),
      [&](const NumericConverterRegistryItem& item)
      {
         result.append( audacity::ToQString(item.symbol.Translation()));
      });
   return result;
}

NumericConverterFormatterAdapter* NumericConverterFormatterAdapterFactory::create(const QString& type, int index) const
{
   int counter = 0;
   NumericConverterFormatterAdapter* result{};
   NumericConverterRegistry::Visit(mFormatterContext, audacity::ToWXString(type),
      [&](const NumericConverterRegistryItem& item)
      {
         if(counter == index)
         {
            result = new NumericConverterFormatterAdapter(
               item.factory->Create(mFormatterContext)
            );
         }
         ++counter;
      });
   return result;
}

static audacity::ProjectQMLEnvironment::Property sFormatterFactory {
   "formatterFactory",
   [](QQmlEngine&, AudacityProject& project)
   {
      return std::make_unique<NumericConverterFormatterAdapterFactory>(
         FormatterContext::ProjectContext(project)
      );
   }
};
