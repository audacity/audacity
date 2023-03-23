/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterRegistry.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include <functional>
#include <memory>

#include "Registry.h"
#include "NumericConverterType.h"

struct NumericConverterFormatter;

struct NUMERIC_FORMATS_API NumericConverterConfig final
{
   double sampleRate { 44100.0 };

   double tempo { 120.0 };
   int upperTimeSignature { 4 };
   int lowerTimeSignature { 4 };
};

using NumericConverterFormatterFactory =
   std::function<std::unique_ptr<NumericConverterFormatter>(
      const NumericConverterConfig&)>;

struct NUMERIC_FORMATS_API NumericConverterRegistryGroup :
    public Registry::InlineGroupItem<NumericConverterRegistryGroup>
{
   template <typename... Args>
   NumericConverterRegistryGroup(
      const Identifier& internalName, NumericConverterType _type,
      Args&&... args)
       : InlineGroupItem { internalName, std::forward<Args>(args)... }
       , type { _type }
   {
   }

   ~NumericConverterRegistryGroup() override;

   bool Transparent() const override;
   
   NumericConverterType type;
};

struct NUMERIC_FORMATS_API NumericConverterRegistryItem : public Registry::SingleItem
{
   NumericConverterRegistryItem(
      const Identifier& internalName, const NumericFormatSymbol& symbol,
      NumericConverterFormatterFactory factory);

   NumericConverterRegistryItem(
      const Identifier& internalName, const NumericFormatSymbol& symbol,
      const TranslatableString& fractionLabel,
      NumericConverterFormatterFactory factory);
   
   ~NumericConverterRegistryItem() override;

   const NumericFormatSymbol symbol;
   const TranslatableString fractionLabel;
   
   const NumericConverterFormatterFactory factory;
};

struct NUMERIC_FORMATS_API NumericConverterRegistry final
{
   static Registry::GroupItem& Registry();

   using Visitor = std::function<void(const NumericConverterRegistryItem&)>;
   
   static void Visit(NumericConverterType type, Visitor visitor);

   static const NumericConverterRegistryItem*
   Find(NumericConverterType type, const NumericFormatSymbol& symbol);
};

NUMERIC_FORMATS_API Registry::BaseItemPtr NumericConverterFormatterItem(
   const Identifier& functionId, const TranslatableString& label,
   NumericConverterFormatterFactory factory);

NUMERIC_FORMATS_API Registry::BaseItemPtr NumericConverterFormatterItem(
   const Identifier& functionId, const TranslatableString& label,
   const TranslatableString& fractionLabel,
   NumericConverterFormatterFactory factory);

template <typename... Args>
Registry::BaseItemPtr NumericConverterFormatterGroup(
   const Identifier& groupId, NumericConverterType type,
   Args&&... args)
{
   return std::make_unique<NumericConverterRegistryGroup>(
      groupId, type, std::forward<Args>(args)...);
}


struct NUMERIC_FORMATS_API NumericConverterItemRegistrator final :
    public Registry::RegisteredItem<
       Registry::BaseItem, NumericConverterRegistry>
{
   NumericConverterItemRegistrator(
      const Registry::Placement& placement, Registry::BaseItemPtr pItem);

   NumericConverterItemRegistrator(
      const wxString& path, Registry::BaseItemPtr pItem)
       // Delegating constructor
       : NumericConverterItemRegistrator(
            Registry::Placement { path }, std::move(pItem))
   {
   }
};
