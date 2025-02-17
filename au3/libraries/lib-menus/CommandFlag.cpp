/**********************************************************************

Audacity: A Digital Audio Editor

CommandFlag.cpp

Paul Licameli split from Menus.cpp

**********************************************************************/

#include "CommandFlag.h"

namespace {
ReservedCommandFlag::Predicates& sPredicates()
{
    static ReservedCommandFlag::Predicates thePredicates;
    return thePredicates;
}

std::vector< CommandFlagOptions >& sOptions()
{
    static std::vector< CommandFlagOptions > theOptions;
    return theOptions;
}
}

const ReservedCommandFlag::Predicates& ReservedCommandFlag::RegisteredPredicates()
{
    return sPredicates();
}

const std::vector< CommandFlagOptions >& ReservedCommandFlag::Options()
{
    return sOptions();
}

ReservedCommandFlag::ReservedCommandFlag(
    const Predicate& predicate, const CommandFlagOptions& options)
{
    static size_t sNextReservedFlag = 0;
    // This will throw std::out_of_range if the constant NCommandFlags is too
    // small
    set(sNextReservedFlag++);
    sPredicates().emplace_back(predicate);
    sOptions().emplace_back(options);
}

namespace {
MenuItemEnablers& sEnablers()
{
    static MenuItemEnablers enablers;
    return enablers;
}
}

const MenuItemEnablers& RegisteredMenuItemEnabler::Enablers()
{
    return sEnablers();
}

RegisteredMenuItemEnabler::RegisteredMenuItemEnabler(
    const MenuItemEnabler& enabler)
{
    sEnablers().emplace_back(enabler);
}
