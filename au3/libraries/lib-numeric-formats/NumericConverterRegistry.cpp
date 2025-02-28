/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterRegistry.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "NumericConverterRegistry.h"

namespace {
const auto PathStart = L"NumericConverterRegistry";
}

NumericConverterRegistryItem::NumericConverterRegistryItem(
    const Identifier& internalName, const NumericFormatSymbol& _symbol,
    NumericConverterFormatterFactoryPtr _factory)
    : SingleItem{internalName}
    , symbol{_symbol}
    , factory{std::move(_factory)}
{
}

NumericConverterRegistryItem::NumericConverterRegistryItem(
    const Identifier& internalName, const NumericFormatSymbol& _symbol,
    const TranslatableString& _fractionLabel,
    NumericConverterFormatterFactoryPtr _factory)
    : SingleItem{internalName}
    , symbol{_symbol}
    , fractionLabel{_fractionLabel}
    , factory{std::move(_factory)}
{
}

NumericConverterRegistryItem::~NumericConverterRegistryItem()
{
}

Registry::GroupItem<NumericConverterRegistryTraits>&
NumericConverterRegistry::Registry()
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

    Registry::GroupItem<NumericConverterRegistryTraits> top { PathStart };
    bool inMatchingGroup = false;
    Registry::Visit(std::tuple {
        [&](const NumericConverterRegistryGroup& group, auto&) {
            inMatchingGroup = group.GetType() == type;
        },
        [&](const NumericConverterRegistryItem& item, auto&) {
            if (!inMatchingGroup) {
                return;
            }
            // Skip the items that are not acceptable in this context
            if (!item.factory->IsAcceptableInContext(context)) {
                return;
            }
            visitor(item);
        },
        [&](const NumericConverterRegistryGroup&, auto&) {
            inMatchingGroup = false;
        }
    }, &top, &Registry());
}

const NumericConverterRegistryItem* NumericConverterRegistry::Find(
    const FormatterContext& context,
    const NumericConverterType& type, const NumericFormatID& symbol)
{
    const NumericConverterRegistryItem* result = nullptr;

    Visit(
        context,
        type,
        [&result, symbol](const NumericConverterRegistryItem& item)
    {
        if (item.symbol.Internal() == symbol) {
            result = &item;
        }
    });

    return result;
}

NumericConverterRegistryGroup::~NumericConverterRegistryGroup()
{
}
