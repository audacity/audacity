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

namespace
{
const wxString SnapModeKey = L"/Snap/Mode";
const wxString SnapToKey = L"/Snap/To";
const wxString OldSnapToKey = L"/SnapTo";
const auto PathStart = L"SnapFunctions";

struct RegistryVisitor : public Registry::Visitor
{
   explicit RegistryVisitor(SnapRegistryVisitor& _visitor)
       : visitor(_visitor)
   {
   }
   
   void BeginGroup(Registry::GroupItem& item, const Path&) final
   {
      auto group = dynamic_cast<SnapRegistryGroup*>(&item);

      if (group != nullptr)
         visitor.BeginGroup(*group);
   }

   void EndGroup(Registry::GroupItem& item, const Path&) final
   {
      auto group = dynamic_cast<SnapRegistryGroup*>(&item);

      if (group != nullptr)
         visitor.EndGroup(*group);
   }

   void Visit(Registry::SingleItem& item, const Path&) final
   {
      auto concreteItem = dynamic_cast<SnapRegistryItem*>(&item);

      if (concreteItem != nullptr)
         visitor.Visit(*concreteItem);
   }

   SnapRegistryVisitor& visitor;
};
} // namespace

StringSetting SnapToSetting { SnapToKey, "seconds" };

EnumSetting<SnapMode> SnapModeSetting {
   SnapModeKey,
   EnumValueSymbols { L"OFF", L"NEAREST", L"PRIOR" },
   0,
   { SnapMode::SNAP_OFF, SnapMode::SNAP_NEAREST, SnapMode::SNAP_PRIOR }
};

SnapMode ReadSnapMode()
{
   if (gPrefs->HasEntry(SnapModeKey))
      return SnapModeSetting.ReadEnum();

   return static_cast<SnapMode>(gPrefs->Read(OldSnapToKey, 0L));
}

Identifier ReadSnapTo()
{
   if (gPrefs->HasEntry(SnapToKey))
      return SnapToSetting.Read();

   // TODO: deduce snap-to based on the selection format

   return SnapToSetting.GetDefault();
}

Registry::GroupItem& SnapFunctionsRegistry::Registry()
{
   static Registry::TransparentGroupItem<> registry { PathStart };
   return registry;
}

void SnapFunctionsRegistry::Visit(SnapRegistryVisitor& visitor)
{
   static Registry::OrderingPreferenceInitializer init {
      PathStart,
      { { L"", L"beats,triplets,time,video,cd" } },
   };

   RegistryVisitor registryVisitor { visitor };
   Registry::TransparentGroupItem<> top { PathStart };
   Registry::Visit(registryVisitor, &top, &Registry());
}

SnapRegistryItem* SnapFunctionsRegistry::Find(const Identifier& id)
{
   using Cache = std::unordered_map<Identifier, SnapRegistryItem*>;
   static Cache cache;

   auto it = cache.find(id);
   if (it != cache.end())
      return it->second;

   struct CacheUpdater final : Registry::Visitor
   {
      explicit CacheUpdater(Cache& _cache)
          : cache(_cache)
      {
      }

      void Visit(Registry::SingleItem& item, const Path&) override
      {
         auto concreteItem = dynamic_cast<SnapRegistryItem*>(&item);

         if (concreteItem == nullptr)
            return;

         auto it = cache.find(concreteItem->name);

         if (it == cache.end())
         {
            cache.insert(
               std::make_pair(concreteItem->name, concreteItem));
         }
      }

      Cache& cache;
   };

   CacheUpdater update { cache };
   Registry::Visit(update, &Registry());

   it = cache.find(id);

   return it != cache.end() ? it->second : nullptr;
}

SnapResult SnapFunctionsRegistry::Snap(
   const Identifier& id, SnapConfig config, double time, bool nearest)
{
   auto item = Find(id);

   if (item == nullptr)
      return SnapResult { time, false };

   return item->snapFunction(config, time, nearest);
}

SnapRegistryGroup::~SnapRegistryGroup()
{
}

bool SnapRegistryGroup::Transparent() const
{
   return transparent;
}

SnapRegistryItem::SnapRegistryItem(
   const Identifier& internalName, const TranslatableString& _label,
   SnapFunctor _snapFunction)
    : SingleItem { internalName }
    , label { _label }
    , snapFunction { _snapFunction }
{
}

SnapRegistryItem::~SnapRegistryItem()
{
}

Registry::BaseItemPtr SnapFunction(
   const Identifier& functionId, const TranslatableString& label,
   SnapFunctor functor)
{
   return std::make_unique<SnapRegistryItem>(
      functionId, label, std::move(functor));
}

SnapRegistryItemRegistrator::SnapRegistryItemRegistrator(
   const Registry::Placement& placement, Registry::BaseItemPtr pItem)
    : RegisteredItem { std::move(pItem), placement }
{
}

namespace
{
SnapResult SnapWithMultiplier (double value, double multiplier, bool nearest)
{
   auto result = nearest ? std::round(value * multiplier) / multiplier :
                           std::floor(value * multiplier) / multiplier;

   return SnapResult { result, true };
}


SnapFunctor SnapToTime(double multiplier) {
   return [multiplier](SnapConfig, double value, bool nearest)
   { return SnapWithMultiplier(value, multiplier, nearest); };
}

SnapResult SnapToSamples (SnapConfig cfg, double value, bool nearest)
{
   return SnapWithMultiplier(value, cfg.rate, nearest);
}

SnapFunctor SnapToFrames(double fps)
{
   return [fps](SnapConfig, double value, bool nearest)
   { return SnapWithMultiplier(value, fps, nearest); };
}

/*
   bps = tempo / 60
   1/lower takes 1/bps
*/

SnapFunctor SnapToBar ()
{
   return [](SnapConfig cfg, double value, bool nearest)
   {
      const auto multiplier = (cfg.tempo / 60.0) / cfg.timeSignature.first;
      return SnapWithMultiplier(value, multiplier, nearest);
   };
}

SnapFunctor SnapToBeat(int divisor)
{
   return [divisor](SnapConfig cfg, double value, bool nearest)
   {
      const auto multiplier =
         (cfg.tempo / 60.0) * divisor / cfg.timeSignature.second;
      return SnapWithMultiplier(value, multiplier, nearest);
   };
}

SnapFunctor SnapToTriplets(int divisor)
{
   return [divisor](SnapConfig cfg, double value, bool nearest)
   {
      const auto tripletDivisor = 3 * (divisor / 2);
      const auto multiplier =
         (cfg.tempo / 60.0) * tripletDivisor / cfg.timeSignature.second;
      return SnapWithMultiplier(value, multiplier, nearest);
   };
}

SnapRegistryItemRegistrator beats {
   Registry::Placement { {}, { Registry::OrderingHint::Begin } },
   SnapFunctionGroup(
      "beats", XO("Beats"), true, SnapFunction("bar", XO("Bar"), SnapToBar()),
      SnapFunction("bar_1_2", XO("1/2"), SnapToBeat(2)),
      SnapFunction("bar_1_4", XO("1/4"), SnapToBeat(4)),
      SnapFunction("bar_1_8", XO("1/8"), SnapToBeat(8)),
      SnapFunction("bar_1_16", XO("1/16"), SnapToBeat(16)),
      SnapFunction("bar_1_32", XO("1/32"), SnapToBeat(32)),
      SnapFunction("bar_1_64", XO("1/64"), SnapToBeat(64)),
      SnapFunction("bar_1_128", XO("1/128"), SnapToBeat(128)))
};

SnapRegistryItemRegistrator triplets {
   Registry::Placement { {},
                         { Registry::OrderingHint::After, "beats" } },
   SnapFunctionGroup(
      "triplets", XO("Triplets"), true,
      SnapFunction("triplet_1_2", XO("1/2 (triplets)"), SnapToTriplets(2)),
      SnapFunction("triplet_1_4", XO("1/4 (triplets)"), SnapToTriplets(4)),
      SnapFunction("triplet_1_8", XO("1/8 (triplets)"), SnapToTriplets(8)),
      SnapFunction("triplet_1_16", XO("1/16 (triplets)"), SnapToTriplets(16)),
      SnapFunction("triplet_1_32", XO("1/32 (triplets)"), SnapToTriplets(32)),
      SnapFunction("triplet_1_64", XO("1/64 (triplets)"), SnapToTriplets(64)),
      SnapFunction(
         "triplet_1_128", XO("1/128 (triplets)"), SnapToTriplets(128)))
};

SnapRegistryItemRegistrator secondsAndSamples {
   Registry::Placement { {},
                         { Registry::OrderingHint::After, "triplets" } },
   SnapFunctionGroup(
      "time", XO("Seconds && samples"), false,
      SnapFunction("seconds", XO("Seconds"), SnapToTime(1.0)),
      SnapFunction("deciseconds", XO("Deciseconds"), SnapToTime(10.0)),
      SnapFunction("centiseconds", XO("Centiseconds"), SnapToTime(100.0)),
      SnapFunction("milliseconds", XO("Milliseconds"), SnapToTime(1000.0)),
      SnapFunction("samples", XO("Samples"), SnapToSamples))
};

SnapRegistryItemRegistrator videoFrames {
   Registry::Placement { {},
                         { Registry::OrderingHint::After, "time" } },
   SnapFunctionGroup(
      "video", XO("Video frames"), false,
      SnapFunction(
         "film_24_fps", XO("Film frames (24 fps)"), SnapToFrames(24.0)),
      SnapFunction(
         "film_23.97_fps", XO("NTSC frames (23.97 fps)"),
         SnapToFrames(24.0 / 1.001)),
      SnapFunction("film_25_fps", XO("PAL frames"), SnapToFrames(25.0)))
};

SnapRegistryItemRegistrator cdFrames {
   Registry::Placement { {},
                         { Registry::OrderingHint::After, "video" } },
   SnapFunctionGroup(
      "cd", XO("CD frames"), false,
      SnapFunction("cd_75_fps", XO("CDDA frames (75 fps)"), SnapToFrames(75.0)))
};

}
