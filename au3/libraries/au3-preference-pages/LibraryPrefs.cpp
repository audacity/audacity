/**********************************************************************

  Audacity: A Digital Audio Editor

  LibraryPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class LibraryPrefs
\brief A PrefsPanel used to select manage external libraries like the
MP3 and FFmpeg encoding libraries.

*//*******************************************************************/

#include "LibraryPrefs.h"

#include "ShuttleGui.h"

#include <wx/defs.h>
#include <wx/stattext.h>

////////////////////////////////////////////////////////////////////////////////
static const auto PathStart = wxT("LibraryPreferences");

auto LibraryPrefs::PopulatorItem::Registry() -> Registry::GroupItem<Traits>&
{
    static Registry::GroupItem<Traits> registry{ PathStart };
    return registry;
}

LibraryPrefs::PopulatorItem::PopulatorItem(
    const Identifier& id, Populator populator)
    : SingleItem{id}
    , mPopulator{move(populator)}
{}

LibraryPrefs::RegisteredControls::RegisteredControls(
    const Identifier& id, Populator populator,
    const Registry::Placement& placement)
    : RegisteredItem{
                     std::make_unique< PopulatorItem >(id, move(populator)),
                     placement
                     }
{}

bool LibraryPrefs::RegisteredControls::Any()
{
    return !PopulatorItem::Registry().empty();
}

LibraryPrefs::LibraryPrefs(wxWindow* parent, wxWindowID winid)
/* i18n-hint: refers to optional plug-in software libraries */
    :   PrefsPanel(parent, winid, XO("Libraries"))
{
    Populate();
}

LibraryPrefs::~LibraryPrefs()
{
}

ComponentInterfaceSymbol LibraryPrefs::GetSymbol() const
{
    return LIBRARY_PREFS_PLUGIN_SYMBOL;
}

TranslatableString LibraryPrefs::GetDescription() const
{
    return XO("Preferences for Library");
}

ManualPageID LibraryPrefs::HelpPageName()
{
    return "Libraries_Preferences";
}

/// Creates the dialog and its contents.
void LibraryPrefs::Populate()
{
    //------------------------- Main section --------------------
    // Now construct the GUI itself.
    // Use 'eIsCreatingFromPrefs' so that the GUI is
    // initialised with values from gPrefs.
    ShuttleGui S(this, eIsCreatingFromPrefs);
    PopulateOrExchange(S);
    // ----------------------- End of main section --------------
}

/// This PopulateOrExchange function is a good example of mixing the fully
/// automatic style of reading/writing from GUI to prefs with the partial form.
///
/// You'll notice that some of the Tie functions have Prefs identifiers in them
/// and others don't.
void LibraryPrefs::PopulateOrExchange(ShuttleGui& S)
{
    using namespace Registry;
    static OrderingPreferenceInitializer init{
        PathStart,
        { { wxT(""), wxT("MP3,FFmpeg") } },
    };

    S.SetBorder(2);
    S.StartScroller();

    // visit the registry to collect the plug-ins properly
    // sorted
    GroupItem<Traits> top{ PathStart };
    Registry::Visit(
        [&](const PopulatorItem& item, auto&) { item.mPopulator(S); },
        &top, &PopulatorItem::Registry());

    S.EndScroller();
}

bool LibraryPrefs::Commit()
{
    ShuttleGui S(this, eIsSavingToPrefs);
    PopulateOrExchange(S);

    return true;
}

namespace {
PrefsPanel::Registration sAttachment{ "Library",
                                      [](wxWindow* parent, wxWindowID winid, AudacityProject*) -> PrefsPanel*
    {
        wxASSERT(parent); // to justify safenew
        if (LibraryPrefs::RegisteredControls::Any()) {
            return safenew LibraryPrefs(parent, winid);
        } else {
            return nullptr;
        }
    },
                                      false,
                                      // Register with an explicit ordering hint because this one might be
                                      // absent
                                      { "", { Registry::OrderingHint::Before, "Directories" } }
};
}

LibraryPrefs::RegisteredControls::Init::Init()
{
    (void)PopulatorItem::Registry();
}
