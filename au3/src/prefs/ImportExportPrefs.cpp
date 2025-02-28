/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportExportPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class ImportExportPrefs
\brief A PrefsPanel used to select import and export options.

*//*******************************************************************/

#include "ImportExportPrefs.h"

#include <wx/defs.h>
#include <wx/statbox.h>
#include <wx/stattext.h>

#include "NoteTrack.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "WindowAccessible.h"

static const auto PathStart = wxT("ImportExportPreferences");

auto ImportExportPrefs::PopulatorItem::Registry()
-> Registry::GroupItem<Traits>&
{
    static Registry::GroupItem<Traits> registry{ PathStart };
    return registry;
}

ImportExportPrefs::PopulatorItem::PopulatorItem(
    const Identifier& id, Populator populator)
    : SingleItem{id}
    , mPopulator{move(populator)}
{}

ImportExportPrefs::RegisteredControls::RegisteredControls(
    const Identifier& id, Populator populator,
    const Registry::Placement& placement)
    : RegisteredItem{
                     std::make_unique<PopulatorItem>(id, move(populator)),
                     placement
                     }
{}

bool ImportExportPrefs::RegisteredControls::Any()
{
    return !PopulatorItem::Registry().empty();
}

ImportExportPrefs::ImportExportPrefs(wxWindow* parent, wxWindowID winid)
    :   PrefsPanel(parent, winid, XO("Import / Export"))
{
    Populate();
}

ImportExportPrefs::~ImportExportPrefs()
{
}

ComponentInterfaceSymbol ImportExportPrefs::GetSymbol() const
{
    return IMPORT_EXPORT_PREFS_PLUGIN_SYMBOL;
}

TranslatableString ImportExportPrefs::GetDescription() const
{
    return XO("Preferences for ImportExport");
}

ManualPageID ImportExportPrefs::HelpPageName()
{
    return "Import_-_Export_Preferences";
}

/// Creates the dialog and its contents.
void ImportExportPrefs::Populate()
{
    //------------------------- Main section --------------------
    // Now construct the GUI itself.
    // Use 'eIsCreatingFromPrefs' so that the GUI is
    // initialised with values from gPrefs.
    ShuttleGui S(this, eIsCreatingFromPrefs);
    PopulateOrExchange(S);
    // ----------------------- End of main section --------------
}

EnumSetting<bool> ImportExportPrefs::MusicFileImportSetting {
    wxT("/FileFormats/MusicFileImportSettingChoice"),
    {
        /* i18n-hint: The music theory "beat" */
        EnumValueSymbol {
            wxT("Yes"),
            XXO(
                "S&witch view to Beats and Measures and align with musical grid") },
        EnumValueSymbol { wxT("Ask"), XXO("&Ask me each time") },
        EnumValueSymbol { wxT("No"), XXO("Do &nothing") },
    },
    1,
    { false, true, false },
    wxT("/FileFormats/MusicFileImportSetting"),
};

void ImportExportPrefs::PopulateOrExchange(ShuttleGui& S)
{
    S.SetBorder(2);
    S.StartScroller();

    // Add registered controls
    using namespace Registry;
    static OrderingPreferenceInitializer init{
        PathStart,
        { { wxT(""), wxT("LabelStyle,AllegroTimeOption") } },
    };

    // visit the registry to collect the plug-ins properly
    // sorted
    GroupItem<Traits> top{ PathStart };
    Registry::Visit(
        [&](const PopulatorItem& item, auto&) { item.mPopulator(S); },
        &top, &PopulatorItem::Registry());

    auto musicImportsBox = S.StartStatic(XO("Music Imports"));
    {
        const auto header = S.AddVariableText(
            XO("When Audacity detects music in file imported on empty project"));
#if wxUSE_ACCESSIBILITY
        if (musicImportsBox != nullptr) {
            musicImportsBox->SetName(header->GetLabel());
            safenew WindowAccessible(musicImportsBox);
        }
#endif
#if defined(__WXMAC__)
        // see https://bugzilla.audacityteam.org/show_bug.cgi?id=2692
        S.StartPanel();
#endif
        {
            S.StartRadioButtonGroup(ImportExportPrefs::MusicFileImportSetting);
            {
                S.TieRadioButton();
                S.TieRadioButton();
                S.TieRadioButton();
            }
            S.EndRadioButtonGroup();
        }
#if defined(__WXMAC__)
        S.EndPanel();
#endif
    }
    S.EndStatic();

    S.EndScroller();
}

bool ImportExportPrefs::Commit()
{
    ShuttleGui S(this, eIsSavingToPrefs);
    PopulateOrExchange(S);

    return true;
}

namespace {
PrefsPanel::Registration sAttachment{ "ImportExport",
                                      [](wxWindow* parent, wxWindowID winid, AudacityProject*) -> PrefsPanel*
    {
        wxASSERT(parent); // to justify safenew
        if (ImportExportPrefs::RegisteredControls::Any()) {
            return safenew ImportExportPrefs(parent, winid);
        } else {
            return nullptr;
        }
    },
                                      false,
                                      // Register with an explicit ordering hint because this one might be
                                      // absent
                                      { "", { Registry::OrderingHint::After, "Tracks" } }
};
}

ImportExportPrefs::RegisteredControls::Init::Init()
{
    (void)PopulatorItem::Registry();
}
