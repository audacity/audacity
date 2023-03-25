/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SnapUtils.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

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

struct SNAPPING_API SnapRegistryVisitor /* not final */
{
   virtual void BeginGroup(const SnapRegistryGroup& item) = 0;
   virtual void EndGroup(const SnapRegistryGroup& item) = 0;
   virtual void Visit(const SnapRegistryItem& item) = 0;
};

struct SNAPPING_API SnapConfig final
{
   double rate {};
   double tempo {};
   std::pair<int, int> timeSignature {};
};

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

struct SNAPPING_API SnapRegistryGroup :
    public Registry::InlineGroupItem<SnapRegistryVisitor>
{
   template <typename... Args>
   SnapRegistryGroup(
      const Identifier& internalName, const TranslatableString& _label,
      bool _transparent, Args&&... args)
       : InlineGroupItem { internalName, std::forward<Args>(args)... }
       , label { _label }
       , transparent { _transparent}
   {}
   
   ~SnapRegistryGroup() override;

   bool Transparent() const override;
   
   const TranslatableString label;
   const bool transparent;
};

using SnapFunctor = std::function<SnapResult(SnapConfig, double, bool)>;

struct SNAPPING_API SnapRegistryItem : public Registry::SingleItem
{
   SnapRegistryItem(
      const Identifier& internalName, const TranslatableString& label,
      SnapFunctor snapFunction);
   ~SnapRegistryItem() override;

   const TranslatableString label;
   const SnapFunctor snapFunction;
};

SNAPPING_API Registry::BaseItemPtr SnapFunction(
   const Identifier& functionId, const TranslatableString& label,
   SnapFunctor functor);

template <typename... Args>
Registry::BaseItemPtr SnapFunctionGroup (
   const Identifier& groupId, TranslatableString label, bool transparent, Args&&... args)
{
   return std::make_unique<SnapRegistryGroup>(
      groupId, label, transparent, std::forward<Args>(args)...);
}

struct SNAPPING_API SnapFunctionsRegistry final {
   static Registry::GroupItem& Registry();

   static void Visit(SnapRegistryVisitor& visitor);

   static SnapRegistryItem* Find(const Identifier& id);

   static SnapResult Snap(const Identifier& id, SnapConfig config, double time, bool nearest);
};

struct SNAPPING_API SnapRegistryItemRegistrator final :
    public Registry::RegisteredItem<Registry::BaseItem, SnapFunctionsRegistry>
{
   SnapRegistryItemRegistrator(
      const Registry::Placement& placement, Registry::BaseItemPtr pItem);

   SnapRegistryItemRegistrator(
      const wxString& path, Registry::BaseItemPtr pItem)
       // Delegating constructor
       : SnapRegistryItemRegistrator(
            Registry::Placement { path }, std::move(pItem))
   {
   }
};

