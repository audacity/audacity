/*!*********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginRegistrationDialog.cpp

  Paul Licameli split from PluginManager.cpp

**********************************************************************/
#include "PluginRegistrationDialog.h"

#include <numeric>
#include <unordered_map>

#include "AudacityMessageBox.h"
#include "EffectInterface.h"
#include "HelpSystem.h"
#include "IncompatiblePluginsDialog.h"
#include "ModuleManager.h"
#include "PluginManager.h"
#include "PluginStartupRegistration.h"
#include "ProgressDialog.h"
#include "ShuttleGui.h"

#include <set>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/app.h>
#include <wx/defs.h>
#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/wfstream.h>
#include <wx/utils.h>
#include <wx/dc.h>
#include <wx/sizer.h>
#include <wx/vlbox.h>
#include <wx/choice.h>

#include "PluginDataModel.h"
#include "PluginDataViewCtrl.h"

enum
{
    ID_FilterState = 10000,
    ID_FilterType,
    ID_FilterCategory,
    ID_List,
    ID_Rescan,
    ID_GetMoreEffects,
};

BEGIN_EVENT_TABLE(PluginRegistrationDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, PluginRegistrationDialog::OnOK)
EVT_BUTTON(wxID_CANCEL, PluginRegistrationDialog::OnCancel)
EVT_BUTTON(ID_Rescan, PluginRegistrationDialog::OnRescan)
EVT_BUTTON(ID_GetMoreEffects, PluginRegistrationDialog::OnGetMoreEffects)
EVT_CHOICE(ID_FilterState, PluginRegistrationDialog::OnStateFilterValueChanged)
EVT_CHOICE(ID_FilterType, PluginRegistrationDialog::OnTypeFilterValueChanged)
EVT_CHOICE(ID_FilterCategory, PluginRegistrationDialog::OnCategoryFilterValueChanged)
END_EVENT_TABLE()

static const TranslatableStrings ShowFilterValues {
    XO("All"),
    XO("Disabled"),
    XO("Enabled")
};

static const std::vector<std::pair<int, TranslatableString> > CategoryFilterValues {
    { -1, XO("All") },
    { EffectTypeGenerate, XO("Generator") },
    { EffectTypeProcess, XO("Effect") },
    { EffectTypeAnalyze, XO("Analyzer") },
    { EffectTypeTool, XO("Tool") }
};

PluginRegistrationDialog::PluginRegistrationDialog(wxWindow* parent, int defaultEffectCategory)
    :  wxDialogWrapper(parent,
                       wxID_ANY,
                       XO("Manage Plugins"),
                       wxDefaultPosition, wxDefaultSize,
                       wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
    mPluginsModel = safenew PluginDataModel(defaultEffectCategory);
    SetName();
    Populate();
}

void PluginRegistrationDialog::Populate()
{
    //------------------------- Main section --------------------
    ShuttleGui S(this, eIsCreating);
    PopulateOrExchange(S);
    // ----------------------- End of main section --------------
}

/// Defines the dialog and does data exchange with it.
void PluginRegistrationDialog::PopulateOrExchange(ShuttleGui& S)
{
    constexpr int Margin = 12;

    static const std::unordered_map<TranslatableString, TranslatableString> extraProviders = {
        { XO("Builtin Effects"), XO("Native Audacity") }
    };

    TranslatableStrings pluginProviderNames;
    pluginProviderNames.push_back(XO("All"));
    mPluginProviderIDs.clear();
    mPluginProviderIDs.push_back({});
    auto& moduleManager = ModuleManager::Get();
    for (auto& [name, provider] : moduleManager.Providers()) {
        //Use same name as in prefs
        auto familySymbol = provider->GetOptionalFamilySymbol();
        if (!familySymbol.empty()) {
            pluginProviderNames.push_back(familySymbol.Msgid());
        } else {
            auto it = extraProviders.find(provider->GetSymbol().Msgid());
            if (it != extraProviders.end()) {
                pluginProviderNames.push_back(it->second);
            } else {
                continue;
            }
        }
        mPluginProviderIDs.push_back(PluginManager::GetID(provider.get()));
    }

    S.Prop(1).StartPanel(wxEXPAND);
    {
        S.StartVerticalLay(true);
        {
            S.AddSpace(1, Margin);
            S.StartHorizontalLay(wxEXPAND, 0);
            {
                S.AddSpace(Margin, 1);
                S.StartHorizontalLay(wxALIGN_LEFT, 0);
                {
                    TranslatableStrings categoryFilterNames;
                    std::transform(
                        CategoryFilterValues.begin(),
                        CategoryFilterValues.end(),
                        std::back_inserter(categoryFilterNames),
                        [](const auto& p) { return p.second; });
                    const auto selectedCategory
                        =std::distance(
                              CategoryFilterValues.begin(),
                              std::find_if(
                                  CategoryFilterValues.begin(),
                                  CategoryFilterValues.end(),
                                  [category = mPluginsModel->GetFilterCategory()](const auto& p) {
                        return p.first == category;
                    }
                                  ));
                    S
                    .Id(ID_FilterState)
                    .AddChoice(XXO("&Show:"), ShowFilterValues, 0)
                    ->SetMinSize(wxSize(120, -1));
                    S
                    .Id(ID_FilterType)
                    .AddChoice(XXO("&Type:"), pluginProviderNames, 0)
                    ->SetMinSize(wxSize(120, -1));
                    S
                    .Id(ID_FilterCategory)
                    .AddChoice(XXO("C&ategory:"), categoryFilterNames, selectedCategory)
                    ->SetMinSize(wxSize(120, -1));
                }
                S.EndHorizontalLay();
                S.AddSpace(1, 1, 1);
                S.StartHorizontalLay(wxALIGN_CENTRE, 0);
                {
                    const auto searchCtrl = S
                                            .MinSize({ 240, -1 })
                                            .AddTextBox(XXO("Searc&h:"), wxEmptyString, 0);
                    if (searchCtrl != nullptr) {
                        searchCtrl->Bind(wxEVT_TEXT, &PluginRegistrationDialog::OnSearchTextChanged, this);
                    }
                }
                S.EndHorizontalLay();
                S.AddSpace(Margin, 1);
            }
            S.EndHorizontalLay();

            {
                const auto pluginsList = safenew PluginDataViewCtrl(
                    S.GetParent(), ID_List, wxDefaultPosition, wxDefaultSize,
                    wxDV_MULTIPLE | wxDV_HORIZ_RULES | wxDV_VERT_RULES,
                    wxDefaultValidator,
                    _("Plugin")
                    );
                mPluginList = pluginsList;
                mPluginList->AssociateModel(mPluginsModel.get());
                mPluginList->SetMinSize({ 728, 288 });
                mPluginList->GetColumn(PluginDataModel::ColumnName)->SetSortOrder(true);
                mPluginsModel->Resort();
            }

            S.SetBorder(Margin);
            S.Id(ID_List)
            .Prop(1)
            .AddWindow(mPluginList, wxEXPAND);
            S.SetBorder(2);

            S.AddSpace(1, Margin);

            S.StartHorizontalLay(wxALIGN_LEFT | wxEXPAND, 0);
            {
                S.AddSpace(Margin, 1);
                S.Id(ID_Rescan).AddButton(XXO("&Rescan"));
#if defined(__WXMSW__) || defined(__WXMAC__)
                S.Id(ID_GetMoreEffects).AddButton(XXO("&Get more effects..."));
#endif
                S.AddSpace(1, 1, 1);

                S.Id(wxID_OK).AddButton(XXO("&OK"));
                S.Id(wxID_CANCEL).AddButton(XXO("&Cancel"));
                S.AddSpace(Margin, 1);
            }
            S.EndHorizontalLay();
            S.AddSpace(1, Margin);
        }
        S.EndVerticalLay();
    }
    S.EndStatic();

    wxRect r = wxGetClientDisplayRect();

    Layout();
    Fit();

    wxSize sz = GetSize();
    sz.SetWidth(wxMin(sz.GetWidth(), r.GetWidth()));
    sz.SetHeight(wxMin(sz.GetHeight(), r.GetHeight()));
    SetMinSize(sz);

    mPluginList->GetColumn(PluginDataModel::ColumnName)->SetWidth(200);
    mPluginList->GetColumn(PluginDataModel::ColumnType)->SetWidth(80);
    mPluginList->GetColumn(PluginDataModel::ColumnPath)->SetWidth(350);

    // Parent window is usually not there yet, so centre on screen rather than on parent.
    CenterOnScreen();
}

void PluginRegistrationDialog::ReloadModel()
{
    mPluginsModel.reset(safenew PluginDataModel(
                            mPluginsModel->GetFilterCategory(),
                            mPluginsModel->GetFilterState(),
                            mPluginsModel->GetFilterType(),
                            mPluginsModel->GetFilterExpr()
                            ));
    mPluginList->AssociateModel(mPluginsModel.get());
}

void PluginRegistrationDialog::OnSearchTextChanged(wxCommandEvent& evt)
{
    mPluginsModel->SetFilterExpr(evt.GetString().Trim().Trim(true));
}

void PluginRegistrationDialog::OnStateFilterValueChanged(wxCommandEvent& evt)
{
    const auto index = evt.GetInt();

    mPluginsModel->SetFilterState(
        index == 2 ? 1 : (index == 1 ? 0 : -1)
        );
}

void PluginRegistrationDialog::OnTypeFilterValueChanged(wxCommandEvent& evt)
{
    const auto index = evt.GetInt();
    if (index >= 0 && index < mPluginProviderIDs.size()) {
        mPluginsModel->SetFilterType(mPluginProviderIDs[index]);
    }
}

void PluginRegistrationDialog::OnCategoryFilterValueChanged(wxCommandEvent& evt)
{
    const auto index = evt.GetInt();
    if (index >= 0 && index < CategoryFilterValues.size()) {
        mPluginsModel->SetFilterCategory(CategoryFilterValues[index].first);
    }
}

void PluginRegistrationDialog::OnRescan(wxCommandEvent& WXUNUSED(evt))
{
    wxTheApp->CallAfter([self = wxWeakRef(this)] {
        std::set<PluginPath> disabledPlugins;
        std::vector<wxString> failedPlugins;

        auto& pm = PluginManager::Get();

        // Record list of plugins that are currently disabled
        for (auto& plug : pm.AllPlugins()) {
            PluginType plugType = plug.GetPluginType();
            if (plugType != PluginTypeEffect && plugType != PluginTypeStub) {
                continue;
            }

            if (!plug.IsEnabled()) {
                disabledPlugins.insert(plug.GetPath());
            }
        }

        //PluginManager::ClearEffectPlugins() removes all effects
        //making all pointers cached in PluginDataModel invalid.
        //Reset model pointer before clearing effects so that
        //nothing attempts to access it.
        if (self) {
            self->mPluginList->AssociateModel(nullptr);
        }

        pm.ClearEffectPlugins();

        auto newPlugins = PluginManager::Get().CheckPluginUpdates();
        if (!newPlugins.empty()) {
            PluginStartupRegistration reg(newPlugins);
            reg.Run();

            failedPlugins = reg.GetFailedPluginsPaths();
        }

        // Disable all plugins which were previously disabled
        for (auto& plug : pm.AllPlugins()) {
            PluginType plugType = plug.GetPluginType();
            if (plugType != PluginTypeEffect && plugType != PluginTypeStub) {
                continue;
            }

            const auto& path = plug.GetPath();
            if (disabledPlugins.find(path) != disabledPlugins.end()) {
                plug.SetEnabled(false);
            }
        }

        pm.Save();
        pm.NotifyPluginsChanged();

        if (self) {
            self->ReloadModel();
            if (!failedPlugins.empty()) {
                auto dialog = safenew IncompatiblePluginsDialog(self.get(), wxID_ANY, ScanType::Manual, failedPlugins);
                dialog->ShowModal();
                self->Refresh();
            }
        }
    });
}

void PluginRegistrationDialog::OnGetMoreEffects(wxCommandEvent& WXUNUSED(evt))
{
    OpenInDefaultBrowser("https://www.musehub.com");
}

void PluginRegistrationDialog::OnOK(wxCommandEvent& WXUNUSED(evt))
{
    auto result = ProgressResult::Success;
    {
        // Make sure the progress dialog is deleted before we call EndModal() or
        // we will leave the project window in an unusable state on OSX.
        // See bug #1192.
        std::unique_ptr<ProgressDialog> dialog;
        wxArrayString last3;
        auto updateProgress = [&](int num, int denom, const wxString& msg)
        {
            last3.insert(last3.begin(), msg);
            if (last3.size() > 3) {
                last3.pop_back();
            }
            if (!dialog) {
                dialog = std::make_unique<ProgressDialog>(
                    Verbatim(GetTitle()),
                    TranslatableString {},
                    pdlgHideStopButton
                    );
                dialog->CenterOnParent();
            }
            result = dialog->Update(
                num,
                denom,
                TranslatableString(wxJoin(last3, '\n'), {})
                );
            return result == ProgressResult::Success;
        };
        auto onError = [](const TranslatableString& error) {
            AudacityMessageBox(error);
        };

        mPluginsModel->ApplyChanges(updateProgress, onError);
    }
    if (result == ProgressResult::Success) {
        EndModal(wxID_OK);
    } else {
        ReloadModel();
    }
}

void PluginRegistrationDialog::OnCancel(wxCommandEvent& WXUNUSED(evt))
{
    EndModal(wxID_CANCEL);
}
