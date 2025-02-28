/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SnapUtils.cpp

  Dmitry Vedenko

**********************************************************************/

#include "SnapUtils.h"

#include <algorithm>
#include <cmath>
#include <unordered_map>

namespace {
const wxString SnapModeKey = L"/Snap/Mode";
const wxString SnapToKey = L"/Snap/To";
const wxString OldSnapToKey = L"/SnapTo";
const wxString SelectionFormatKey = L"/SelectionFormat";

const auto PathStart = L"SnapFunctions";
} // namespace

StringSetting SnapToSetting { SnapToKey, "bar_1_8" };

EnumSetting<SnapMode> SnapModeSetting {
    SnapModeKey,
    EnumValueSymbols { L"OFF", L"NEAREST", L"PRIOR" },
    0,
    { SnapMode::SNAP_OFF, SnapMode::SNAP_NEAREST, SnapMode::SNAP_PRIOR }
};

SnapMode ReadSnapMode()
{
    if (gPrefs->HasEntry(SnapModeKey)) {
        return SnapModeSetting.ReadEnum();
    }

    return static_cast<SnapMode>(gPrefs->Read(OldSnapToKey, 0L));
}

wxString DeduceSnapTo()
{
    const auto& defaultSnapTo = SnapToSetting.GetDefault();

    if (!gPrefs->HasEntry(SelectionFormatKey)) {
        return defaultSnapTo;
    }

    // It appears that we are migrating from an older version of Audacity
    // where snapping was controlled by the "/SelectionFormat".
    // We are only trying to match the known values from the
    // config to the appropriate snapping function, this matching
    // does not introduce any kind of dependency to the lib-numeric-format.

    const auto selectionFormat = gPrefs->Read(SelectionFormatKey);

    static const std::unordered_map<wxString, wxString> selectionFormatLookup = {
        { L"seconds", L"seconds" },
        { L"seconds + milliseconds", L"milliseconds" },
        { L"hh:mm:ss", L"seconds" },
        { L"dd:hh:mm:ss", L"seconds" },
        { L"hh:mm:ss + hundredths", L"centiseconds" },
        { L"hh:mm:ss + milliseconds", L"milliseconds" },
        { L"hh:mm:ss + samples", L"samples" },
        { L"samples", L"samples" },
        { L"hh:mm:ss + film frames (24 fps)", L"film_24_fps" },
        { L"film frames (24 fps)", L"film_24_fps" },
        { L"hh:mm:ss + NTSC drop frames", L"ntsc_29.97_fps" },
        // Well, not really. Snapping with the "bent" time sounds funny anyway.
        { L"hh:mm:ss + NTSC non-drop frames", L"ntsc_30_fps" },
        { L"NTSC frames", L"ntsc_29.97_fps" },
        { L"hh:mm:ss + PAL frames (25 fps)", L"film_25_fps" },
        { L"PAL frames (25 fps)", L"film_25_fps" },
        { L"hh:mm:ss + CDDA frames (75 fps)", L"cd_75_fps" },
        { L"CDDA frames (75 fps)", L"cd_75_fps" },
    };

    auto it = selectionFormatLookup.find(selectionFormat);

    if (it != selectionFormatLookup.end()) {
        return it->second;
    }

    return defaultSnapTo;
}

Identifier ReadSnapTo()
{
    if (gPrefs->HasEntry(SnapToKey)) {
        return SnapToSetting.Read();
    }

    // Try to perform the config migration once
    const auto snapTo = DeduceSnapTo();
    SnapToSetting.Write(snapTo);
    gPrefs->Flush();

    return snapTo;
}

Registry::GroupItem<SnapRegistryTraits>& SnapFunctionsRegistry::Registry()
{
    static Registry::GroupItem<SnapRegistryTraits> registry { PathStart };
    return registry;
}

void SnapFunctionsRegistry::Visit(const SnapRegistryVisitor& visitor)
{
    static Registry::OrderingPreferenceInitializer init {
        PathStart,
        { { L"", L"beats,triplets,time,video,cd" } },
    };

    Registry::GroupItem<SnapRegistryTraits> top { PathStart };
    Registry::VisitWithFunctions(visitor, &top, &Registry());
}

const SnapRegistryItem* SnapFunctionsRegistry::Find(const Identifier& id)
{
    using Cache = std::unordered_map<Identifier, const SnapRegistryItem*>;
    static Cache cache;

    auto it = cache.find(id);
    if (it != cache.end()) {
        return it->second;
    }

    auto visitor = [&](const SnapRegistryItem& item, auto&) {
        auto it = cache.find(item.name);

        if (it == cache.end()) {
            cache.insert({ item.name, &item });
        }
    };

    Registry::Visit(visitor, &Registry());

    it = cache.find(id);

    return it != cache.end() ? it->second : nullptr;
}

SnapResult SnapFunctionsRegistry::Snap(
    const Identifier& id, const AudacityProject& project, double time,
    bool nearest)
{
    auto item = Find(id);

    if (item == nullptr) {
        return SnapResult { time, false };
    }

    return item->Snap(project, time, nearest);
}

SnapResult SnapFunctionsRegistry::SingleStep(
    const Identifier& id, const AudacityProject& project, double time,
    bool upwards)
{
    auto item = Find(id);

    if (item == nullptr) {
        return SnapResult { time, false };
    }

    return item->SingleStep(project, time, upwards);
}

SnapRegistryGroup::~SnapRegistryGroup()
{
}

SnapRegistryItem::SnapRegistryItem(
    const Identifier& internalName, const TranslatableString& _label)
    : SingleItem{internalName}
    , label{_label}
{
}

SnapRegistryItem::~SnapRegistryItem()
{
}

namespace {
SnapResult SnapWithMultiplier(double value, double multiplier, bool nearest)
{
    if (multiplier <= 0.0) {
        return SnapResult { value, false };
    }

    auto result = nearest ? std::round(value * multiplier) / multiplier
                  : std::floor(value * multiplier) / multiplier;

    return SnapResult { result, true };
}

class ConstantMultiplierSnapItem final : public SnapRegistryItem
{
public:
    ConstantMultiplierSnapItem(
        const Identifier& internalName, const TranslatableString& label,
        double multiplier)
        : SnapRegistryItem{internalName, label}
        , mMultiplier{multiplier}
    {
        assert(mMultiplier > 0.0);
    }

    SnapResult
    Snap(const AudacityProject&, double time, bool nearest) const override
    {
        return SnapWithMultiplier(time, mMultiplier, nearest);
    }

    SnapResult SingleStep(
        const AudacityProject& project, double time, bool upwards) const override
    {
        const auto step = (upwards ? 1.0 : -1.0) / mMultiplier;
        const double result = time + step;

        if (result < 0.0) {
            return { 0.0, false };
        }

        return SnapWithMultiplier(result, mMultiplier, true);
    }

private:
    const double mMultiplier;
};

class ProjectDependentMultiplierSnapItem final : public SnapRegistryItem
{
public:
    ProjectDependentMultiplierSnapItem(
        const Identifier& internalName, const TranslatableString& label,
        MultiplierFunctor functor)
        : SnapRegistryItem{internalName, label}
        , mMultiplierFunctor{std::move(functor)}
    {
        assert(mMultiplierFunctor);
    }

    SnapResult
    Snap(const AudacityProject& project, double time, bool nearest) const override
    {
        if (!mMultiplierFunctor) {
            return { time, false };
        }
        return SnapWithMultiplier(time, mMultiplierFunctor(project), nearest);
    }

    SnapResult SingleStep(
        const AudacityProject& project, double time, bool upwards) const override
    {
        if (!mMultiplierFunctor) {
            return { time, false };
        }

        const auto multiplier = mMultiplierFunctor(project);

        const auto eps
            =std::max(1.0, time) * std::numeric_limits<double>::epsilon();

        const auto current = static_cast<int>(std::floor(time * (1.0 + eps) * multiplier));
        const auto next = upwards ? current + 1 : current - 1;

        double result = next / multiplier;

        if (result < 0.0) {
            return { 0.0, false };
        }

        while (static_cast<int>(std::floor(result * multiplier)) < next) {
            result += eps;
        }

        while (static_cast<int>(std::floor(result * multiplier)) > next) {
            result -= eps;
        }

        return { result, true };
    }

private:
    const MultiplierFunctor mMultiplierFunctor;
};
}

std::unique_ptr<SnapRegistryItem> TimeInvariantSnapFunction(
    const Identifier& functionId, const TranslatableString& label,
    MultiplierFunctor functor)
{
    return std::make_unique<ProjectDependentMultiplierSnapItem>(
        functionId, label, std::move(functor));
}

std::unique_ptr<SnapRegistryItem> TimeInvariantSnapFunction(
    const Identifier& functionId, const TranslatableString& label,
    double multiplier)
{
    return std::make_unique<ConstantMultiplierSnapItem>(
        functionId, label, multiplier);
}
