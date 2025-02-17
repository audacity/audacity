/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SnapUtils.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "Callable.h"
#include "Prefs.h"
#include "Registry.h"

#include <functional>
#include <string_view>

enum class SnapMode
{
    SNAP_OFF,
    SNAP_NEAREST,
    SNAP_PRIOR
};

extern SNAPPING_API StringSetting SnapToSetting;
extern SNAPPING_API EnumSetting<SnapMode> SnapModeSetting;

SNAPPING_API SnapMode ReadSnapMode();
SNAPPING_API Identifier ReadSnapTo();

struct SnapRegistryGroup;
struct SnapRegistryItem;
struct SnapFunctionSuperGroup;

class AudacityProject;

struct SNAPPING_API SnapResult final
{
    //! Snapped time.
    /*!
       In case the snapping has failed for any reason, this will match the input.
       Snapping can fail if the function matching the identifier was not found
       or if the function decides the input is incorrect.
       */
    double time {};
    //! Set to true, if snapping has succeeded.
    bool snapped {};
};

struct SnapRegistryTraits : Registry::DefaultTraits {
    using LeafTypes = List<SnapRegistryItem>;
    using NodeTypes = List<SnapRegistryGroup, SnapFunctionSuperGroup>;
};
using SnapRegistryVisitor = Registry::VisitorFunctions<SnapRegistryTraits>;

struct SnapRegistryGroupData {
    const TranslatableString label;
    const bool inlined;
};

struct SNAPPING_API SnapRegistryGroup final : Composite::Extension<
        Registry::GroupItem<SnapRegistryTraits>,
        SnapRegistryGroupData,
        const Identifier&
        >
{
    using Extension::Extension;
    ~SnapRegistryGroup() override;
    bool Inlined() const { return inlined; }
    const TranslatableString& Label() const { return label; }
};

struct SNAPPING_API SnapRegistryItem : public Registry::SingleItem
{
    SnapRegistryItem(
        const Identifier& internalName, const TranslatableString& label);
    ~SnapRegistryItem() override;

    const TranslatableString label;

    virtual SnapResult Snap(const AudacityProject& project, double time, bool nearest) const = 0;
    virtual SnapResult SingleStep(const AudacityProject& project, double time, bool upwards) const = 0;
};

std::unique_ptr<SnapRegistryItem> TimeInvariantSnapFunction(
    const Identifier& functionId, const TranslatableString& label, double multiplier);

using MultiplierFunctor = std::function<double (const AudacityProject&)>;

std::unique_ptr<SnapRegistryItem> TimeInvariantSnapFunction(
    const Identifier& functionId, const TranslatableString& label, MultiplierFunctor functor);

constexpr auto SnapFunctionGroup = Callable::UniqueMaker<
    SnapRegistryGroup, const Identifier&, SnapRegistryGroupData>();

struct SNAPPING_API SnapFunctionsRegistry final {
    static Registry::GroupItem<SnapRegistryTraits>& Registry();

    static void Visit(const SnapRegistryVisitor& visitor);

    static const SnapRegistryItem* Find(const Identifier& id);

    static SnapResult Snap(const Identifier& id, const AudacityProject& project, double time, bool nearest);
    static SnapResult SingleStep(
        const Identifier& id, const AudacityProject& project, double time, bool upwards);
};

using SnapRegistryItemRegistrator
    =Registry::RegisteredItem<SnapFunctionsRegistry>;

struct SnapFunctionSuperGroup : Composite::Extension<
        Registry::GroupItem<SnapRegistryTraits>, void, const Identifier&
        > {
    using Extension::Extension;
};

constexpr auto SnapFunctionItems
    =Callable::UniqueMaker<SnapFunctionSuperGroup>();
