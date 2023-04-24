/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterRegistry.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include <functional>
#include <memory>
#include <optional>

#include "Callable.h"
#include "Registry.h"
#include "NumericConverterType.h"

struct NumericConverterFormatter;
class FormatterContext;

class NumericConverterFormatterFactory /* not final */
{
public:
   virtual ~NumericConverterFormatterFactory() = default;

   virtual std::unique_ptr<NumericConverterFormatter>
   Create(const FormatterContext& context) const = 0;

   virtual bool IsAcceptableInContext(const FormatterContext& context) const = 0;
};

using NumericConverterFormatterFactoryPtr =
   std::unique_ptr<NumericConverterFormatterFactory>;

// NumericConverterRegistryGroupTag is a fake type needed to fix the link on Windows
struct NumericConverterRegistryGroupTag {};

struct NUMERIC_FORMATS_API NumericConverterRegistryGroup :
    public Registry::GroupItem<NumericConverterRegistryGroupTag>
{
   template <typename... Args>
   NumericConverterRegistryGroup(
      const Identifier& internalName, NumericConverterType _type,
      Args&&... args)
       : GroupItem { internalName, std::forward<Args>(args)... }
       , type { std::move(_type) }
   {
   }

   ~NumericConverterRegistryGroup() override;

   NumericConverterType type;
};

struct NUMERIC_FORMATS_API NumericConverterRegistryItem : public Registry::SingleItem
{
   NumericConverterRegistryItem(
      const Identifier& internalName, const NumericFormatSymbol& symbol,
      NumericConverterFormatterFactoryPtr factory);

   NumericConverterRegistryItem(
      const Identifier& internalName, const NumericFormatSymbol& symbol,
      const TranslatableString& fractionLabel,
      NumericConverterFormatterFactoryPtr factory);
   
   ~NumericConverterRegistryItem() override;

   const NumericFormatSymbol symbol;
   const TranslatableString fractionLabel;
   
   const NumericConverterFormatterFactoryPtr factory;
};

struct NUMERIC_FORMATS_API NumericConverterRegistry final
{
   static Registry::GroupItemBase& Registry();

   using Visitor = std::function<void(const NumericConverterRegistryItem&)>;
   
   static void Visit(
      const FormatterContext& context, const NumericConverterType& type,
      Visitor visitor);

   static const NumericConverterRegistryItem* Find(
      const FormatterContext& context, const NumericConverterType& type,
      const NumericFormatSymbol& symbol);
};

constexpr auto NumericConverterFormatterItem =
   Callable::UniqueMaker<NumericConverterRegistryItem>();

constexpr auto NumericConverterFormatterGroup =
   Callable::UniqueMaker<NumericConverterRegistryGroup>();

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
