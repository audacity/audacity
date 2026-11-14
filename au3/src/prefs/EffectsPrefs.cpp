/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectsPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class EffectsPrefs
\brief A PrefsPanel for general GUI preferences.

*//*******************************************************************/

#include "EffectsPrefs.h"

#include <wx/choice.h>
#include <wx/defs.h>
#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/scrolwin.h>

#include "PluginManager.h"
#include "PluginRegistrationDialog.h"
#include "MenuCreator.h"
#include "ModuleManager.h"
#include "Prefs.h"
#include "ShuttleGui.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

wxDEFINE_EVENT(EVT_PLUGIN_LOCATIONS_CHANGED, wxCommandEvent);

class EffectsLocationPanel final : public wxPanelWrapper
{
    //Helper container that stores paths during editing.
    //Workaround: wxTextCtrl::GetValue may return previous value if dialog
    //gets closed while text control is still has focus.
    std::vector<wxString> mPaths;
    //Container never shrinks, when row gets removed from the list
    //corresponding pointer is nulled.
    std::vector<wxTextCtrl*> mPathCtrls;
    //Sizer that holds rows (which are sizers too). Each row
    //consists of three controls: path text, browse button, remove button.
    //When created this controls are assigned with unique ids which could be
    //mapped to the indexes in mPathCtrls.
    wxSizer* mRows{};
    wxButton* mAddNewLocation{};

    //Returns the ID for the first element in the row that should be assigned to the
    //element stored at index in mPathCtrls.
    static int BaseRowIdForIndex(int index)
    {
        return wxID_HIGHEST + index * 3;
    }

    //Maps ID of any control in the row to the index in mPathCtrls.
    static int IndexForId(int id)
    {
        return (id - wxID_HIGHEST) / 3;
    }

public:

    template<typename ... Args>
    EffectsLocationPanel(Args&&... args)
        : wxPanelWrapper(std::forward<Args>(args)...)
    {
        InitUI();
    }

    void InitUI()
    {
        auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
        mainSizer->Add(mRows = safenew wxBoxSizer(wxVERTICAL), 0, wxEXPAND);
        mainSizer->Add(
            mAddNewLocation = safenew wxButton(this, wxID_ANY, _("Add new location")),
            0, wxALL, 3
            );
        SetSizer(mainSizer.release());

        mAddNewLocation->Bind(wxEVT_BUTTON, &EffectsLocationPanel::OnAddNewLocationClicked, this);
    }

    void AddLocation(const PluginPath& path, bool setFocus = false)
    {
        const auto baseId = BaseRowIdForIndex(mPathCtrls.size());

        auto rowSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
        wxTextCtrl* text;
        wxButton* remove;
        wxButton* browse;
        rowSizer->Add(
            text = safenew wxTextCtrl(this, baseId, path),
            1, wxEXPAND | wxALL, 3
            );
        rowSizer->Add(
            browse = safenew wxButton(this, baseId + 1, _("Browse...")),
            0, wxALL, 3
            );
        rowSizer->Add(
            remove = safenew wxButton(this, baseId + 2, _("Remove")),
            0, wxALL, 3
            );
        text->Bind(wxEVT_TEXT, [this](wxCommandEvent& evt)
        {
            const auto index = IndexForId(evt.GetId());
            mPaths[index] = evt.GetString();
            evt.Skip();
        });

        browse->Bind(wxEVT_BUTTON, &EffectsLocationPanel::OnBrowseClicked, this);
        remove->Bind(wxEVT_BUTTON, &EffectsLocationPanel::OnRemoveClicked, this);
        mPathCtrls.push_back(text);
        mPaths.push_back(path);

        mRows->Add(rowSizer.release(), 0, wxEXPAND);

        mAddNewLocation->MoveAfterInTabOrder(remove);

        if (setFocus) {
            text->SetFocus();
        }

        wxPostEvent(this, wxCommandEvent(EVT_PLUGIN_LOCATIONS_CHANGED));
    }

    void AddLocations(const PluginPaths& paths)
    {
        for (const auto& location : paths) {
            AddLocation(location);
        }
    }

    PluginPaths GetLocations() const
    {
        PluginPaths paths;
        for (const auto& text : mPaths) {
            if (!text.IsEmpty()) {
                paths.push_back(text);
            }
        }
        return paths;
    }

    void OnAddNewLocationClicked(wxCommandEvent&)
    {
        AddLocation("", true);
    }

    void OnRemoveClicked(wxCommandEvent& evt)
    {
        const auto index = IndexForId(evt.GetId());
        const auto baseId = BaseRowIdForIndex(index);
        //When item is removed from the sizer the tail gets shifted,
        //but items in mPathCtrls are not. We need to map index from
        //mPathCtrls to index in mRows
        mRows->Remove(
            std::count_if(
                mPathCtrls.begin(),
                mPathCtrls.begin() + index,
                [](const auto ptr) { return ptr != nullptr; }
                )
            );

        FindWindowById(baseId, this)->Destroy();
        FindWindowById(baseId + 1, this)->Destroy();
        FindWindowById(baseId + 2, this)->Destroy();
        mPathCtrls[index] = nullptr;
        mPaths[index] = wxString{};//Don't save path on commit

        wxPostEvent(this, wxCommandEvent(EVT_PLUGIN_LOCATIONS_CHANGED));
    }

    void OnBrowseClicked(wxCommandEvent& evt)
    {
        const auto index =  IndexForId(evt.GetId());

        wxDirDialogWrapper dirDialog(
            wxGetTopLevelParent(this),
            wxDirDialogWrapper::DefaultDialogPrompt,
            mPathCtrls[index]->GetValue()
            );
        if (dirDialog.ShowModal() == wxID_OK) {
            mPathCtrls[index]->SetValue(dirDialog.GetPath());//also invoke wxEVT_TEXT
        }
    }
};

EffectsPrefs::EffectsPrefs(wxWindow* parent, wxWindowID winid)
    :  PrefsPanel(parent, winid, XO("Effects"))
{
    Populate();
}

EffectsPrefs::~EffectsPrefs()
{
}

ComponentInterfaceSymbol EffectsPrefs::GetSymbol() const
{
    return EFFECTS_PREFS_PLUGIN_SYMBOL;
}

TranslatableString EffectsPrefs::GetDescription() const
{
    return XO("Preferences for Effects");
}

ManualPageID EffectsPrefs::HelpPageName()
{
    return "Effects_Preferences";
}

void EffectsPrefs::Populate()
{
    //------------------------- Main section --------------------
    // Now construct the GUI itself.
    // Use 'eIsCreatingFromPrefs' so that the GUI is
    // initialised with values from gPrefs.
    ShuttleGui S(this, eIsCreatingFromPrefs);
    PopulateOrExchange(S);
    // ----------------------- End of main section --------------
}

EnumValueSymbols EffectsGroupSymbols {
    ByColumns,
    {
        XO("Sort by effect name"),
        XO("Sort by publisher and effect name"),
        XO("Sort by type and effect name"),
        XO("Group by publisher"),
        XO("Group by type"),
        XO("Group by category"),
        XO("Group by type and publisher")
    },
    {
        wxT("sortby:name"),
        wxT("sortby:publisher:name"),
        wxT("sortby:type:name"),
        wxT("groupby:publisher"),
        wxT("groupby:type"),
        wxT("default"),
        wxT("groupby:type:publisher")
    }
};

BoolSetting SkipEffectsScanAtStartup {
    wxT("/Effects/SkipEffectsScanAtStartup"),
    false
};

ChoiceSetting EffectsGroupBy{
    wxT("/Effects/GroupBy"),
    EffectsGroupSymbols,
    5 // "default"
};

ChoiceSetting RealtimeEffectsGroupBy{
    wxT("/Effects/RealtimeGroupBy"),
    EffectsGroupSymbols,
    6 // "groupby:type:publisher"
};

void EffectsPrefs::PopulateOrExchange(ShuttleGui& S)
{
    const auto scroller = S.StartScroller();

    S.StartStatic(XO("Effect Options"));
    {
        S.StartMultiColumn(2);
        {
            S.SetBorder(3);
            S.MinSize()
            .TieChoice(XXO("Effect menu &organization:"), EffectsGroupBy);
            S.MinSize()
            .TieChoice(XXO("Realtime effect o&rganization:"), RealtimeEffectsGroupBy);
        }
        S.EndMultiColumn();
    }
    S.EndStatic();

    auto& pluginManager = PluginManager::Get();
    for (auto& [id, provider] : ModuleManager::Get().Providers()) {
        if (!provider->SupportsCustomModulePaths()) {
            continue;
        }

        /*i18n-hint: Title of the panel containing user-defined paths where plugins could be found
         * First argument is replaced with plugin type (e.g. "LV2 plugin locations")
         */
        const auto panelTitle = XO("%s plugin locations")
                                .Format(provider->GetOptionalFamilySymbol().Translation());
        S.StartStatic(panelTitle);
        {
            if (S.GetMode() == eIsCreating) {
                const auto panel = safenew EffectsLocationPanel(S.GetParent());
#if wxUSE_ACCESSIBILITY
                panel->SetName(panelTitle);
                safenew WindowAccessible(panel);
#endif

                panel->AddLocations(pluginManager.ReadCustomPaths(*provider.get()));
                S.Prop(1).AddWindow(panel, wxEXPAND);
                mLocations.emplace_back(provider.get(), panel);

                panel->Bind(EVT_PLUGIN_LOCATIONS_CHANGED, [wnd = wxWeakRef(scroller)](const auto&)
                {
                    if (!wnd) {
                        return;
                    }
                    wnd->Layout();
                    wnd->FitInside();
                });
            }
        }
        S.EndStatic();
    }

    S.TieCheckBox(XXO("&Skip effects scanning at startup"), SkipEffectsScanAtStartup);

    if (auto pButton = S.AddButton(XXO("Open Plugin &Manager"), wxALIGN_LEFT)) {
        pButton->Bind(wxEVT_BUTTON, [this](auto) {
            //Adding dependency on PluginRegistrationDialog, not good. Alternatively
            //that could be done with events, though event should be visible here too...
            PluginRegistrationDialog dialog(wxGetTopLevelParent(this));
            if (dialog.ShowModal() == wxID_OK) {
                MenuCreator::RebuildAllMenuBars();
            }
        });
    }

    S.EndScroller();
}

bool EffectsPrefs::Commit()
{
    auto& pluginManager = PluginManager::Get();

    for (auto [provider, panel] : mLocations) {
        pluginManager.StoreCustomPaths(*provider, panel->GetLocations());
    }

    ShuttleGui S(this, eIsSavingToPrefs);
    PopulateOrExchange(S);

    return true;
}

namespace {
PrefsPanel::Registration sAttachment{ "Effects",
                                      [](wxWindow* parent, wxWindowID winid, AudacityProject*)
    {
        wxASSERT(parent); // to justify safenew
        return safenew EffectsPrefs(parent, winid);
    }
};
}
