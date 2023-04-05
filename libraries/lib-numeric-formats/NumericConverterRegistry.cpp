/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterRegistry.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "NumericConverterRegistry.h"

namespace
{
const auto PathStart = L"NumericConverterRegistry";

struct RegistryVisitor : public Registry::Visitor
{
   RegistryVisitor(
      NumericConverterRegistry::Visitor _visitor,
      const FormatterContext& context, NumericConverterType requestedType)
       : visitor { std::move(_visitor) }
       , mContext { context }
       , requestedType { std::move(requestedType) }
   {
   }

   void BeginGroup(Registry::GroupItemBase& item, const Path&) override
   {
      auto concreteGroup = dynamic_cast<NumericConverterRegistryGroup*>(&item);

      mInMatchingGroup =
         concreteGroup != nullptr && concreteGroup->GetType() == requestedType;
   }

   void EndGroup(Registry::GroupItemBase&, const Path&) override
   {
      mInMatchingGroup = false;
   }

   void Visit(Registry::SingleItem& item, const Path&) override
   {
      if (!mInMatchingGroup)
         return;

      auto concreteItem = dynamic_cast<NumericConverterRegistryItem*>(&item);

      if (concreteItem == nullptr)
      {
         // This is unexpected so fail the debug build early
         assert(false);
         return;
      }

      // Skip the items that are not acceptable in this context
      if (!concreteItem->factory->IsAcceptableInContext(mContext))
         return;

      visitor(*concreteItem);
   }

   NumericConverterRegistry::Visitor visitor;
   const NumericConverterType requestedType;
   // Visitor life time is always shorter than the Visit has,
   // which guarantees that FormatterContext outlives the visitor
   const FormatterContext& mContext;
   bool mInMatchingGroup { false };
};
}

 NumericConverterRegistryItem::NumericConverterRegistryItem(
   const Identifier& internalName, const NumericFormatSymbol& _symbol,
   NumericConverterFormatterFactoryPtr _factory)
    : SingleItem { internalName }
    , symbol { _symbol }
    , factory { std::move(_factory) }
{
 }

 NumericConverterRegistryItem::NumericConverterRegistryItem(
    const Identifier& internalName, const NumericFormatSymbol& _symbol,
    const TranslatableString& _fractionLabel,
    NumericConverterFormatterFactoryPtr _factory)
     : SingleItem { internalName }
     , symbol { _symbol }
     , fractionLabel { _fractionLabel }
     , factory { std::move(_factory) }
 {
 }

 NumericConverterRegistryItem::~NumericConverterRegistryItem()
 {
 }

Registry::GroupItemBase& NumericConverterRegistry::Registry()
{
   static Registry::GroupItem<NumericConverterRegistryTraits>
      registry { PathStart };
   return registry;
}

void NumericConverterRegistry::Visit(
   const FormatterContext& context, const NumericConverterType& type,
   Visitor visitor)
{
   static Registry::OrderingPreferenceInitializer init {
      PathStart,
      { { L"", L"parsedTime,beats,parsedFrequency,parsedBandwith" } },
   };

   RegistryVisitor registryVisitor { std::move(visitor), context, type };

   Registry::GroupItem<NumericConverterRegistryTraits> top { PathStart };
   Registry::Visit(registryVisitor, &top, &Registry());
}

const NumericConverterRegistryItem* NumericConverterRegistry::Find(
   const FormatterContext& context, 
   const NumericConverterType& type, const NumericFormatSymbol& symbol)
{
   const NumericConverterRegistryItem* result = nullptr;

   Visit(
      context,
      type,
      [&result, symbol](const NumericConverterRegistryItem& item)
      {
         if (item.symbol == symbol)
            result = &item;
      });

   return result;
}

NumericConverterRegistryGroup::~NumericConverterRegistryGroup()
{
}

NumericConverterItemRegistrator::NumericConverterItemRegistrator(
   const Registry::Placement& placement, Registry::BaseItemPtr pItem)
    : RegisteredItem { std::move(pItem), placement }
{
}
