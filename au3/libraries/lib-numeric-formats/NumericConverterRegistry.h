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

using NumericConverterFormatterFactoryPtr
    =std::unique_ptr<NumericConverterFormatterFactory>;

struct NumericConverterRegistryItem;
struct NumericConverterRegistryGroup;
struct NumericConverterRegistrySuperGroup;

struct NumericConverterRegistryTraits : Registry::DefaultTraits {
    using LeafTypes = List<NumericConverterRegistryItem>;
    using NodeTypes
        =List<NumericConverterRegistryGroup, NumericConverterRegistrySuperGroup>;
};

struct NumericConverterRegistryGroupData {
    NumericConverterType type;
    NumericConverterRegistryGroupData(NumericConverterType type)
        : type{std::move(type)}
    {}
};

struct NUMERIC_FORMATS_API NumericConverterRegistryGroup final : Composite::Extension<
        Registry::GroupItem<NumericConverterRegistryTraits>,
        NumericConverterRegistryGroupData,
        const Identifier&
        >
{
    using Extension::Extension;
    ~NumericConverterRegistryGroup() override;

    const auto& GetType() const { return type; }
};

struct NUMERIC_FORMATS_API NumericConverterRegistryItem : public Registry::SingleItem
{
    NumericConverterRegistryItem(
        const Identifier& internalName, const NumericFormatSymbol& symbol, NumericConverterFormatterFactoryPtr factory);

    NumericConverterRegistryItem(
        const Identifier& internalName, const NumericFormatSymbol& symbol, const TranslatableString& fractionLabel,
        NumericConverterFormatterFactoryPtr factory);

    ~NumericConverterRegistryItem() override;

    const NumericFormatSymbol symbol;
    const TranslatableString fractionLabel;

    const NumericConverterFormatterFactoryPtr factory;
};

struct NUMERIC_FORMATS_API NumericConverterRegistry final
{
    static Registry::GroupItem<NumericConverterRegistryTraits>& Registry();

    using Visitor = std::function<void (const NumericConverterRegistryItem&)>;

    static void Visit(
        const FormatterContext& context, const NumericConverterType& type, Visitor visitor);

    static const NumericConverterRegistryItem* Find(
        const FormatterContext& context, const NumericConverterType& type, const NumericFormatID& symbol);
};

constexpr auto NumericConverterFormatterItem
    =Callable::UniqueMaker<NumericConverterRegistryItem>();

constexpr auto NumericConverterFormatterGroup
    =Callable::UniqueMaker<NumericConverterRegistryGroup,
                           const Identifier&, NumericConverterRegistryGroupData>();

using NumericConverterItemRegistrator
    =Registry::RegisteredItem<NumericConverterRegistry>;

struct NumericConverterRegistrySuperGroup : Composite::Extension<
        Registry::GroupItem<NumericConverterRegistryTraits>,
        void, const Identifier&
        > {
    using Extension::Extension;
};

constexpr auto NumericConverterItems
    =Callable::UniqueMaker<NumericConverterRegistrySuperGroup>();
