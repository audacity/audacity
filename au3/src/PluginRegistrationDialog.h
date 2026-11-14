/**********************************************************************

  Audacity: A Digital Audio Editor

  PluginRegistrationDialog.h

  Paul Licameli split from PluginManager.cpp

**********************************************************************/
#ifndef __AUDACITY_PLUGIN_REGISTRATION_DIALOG__
#define __AUDACITY_PLUGIN_REGISTRATION_DIALOG__

#include "wxPanelWrapper.h" // to inherit
#include "PluginDataModel.h"

class ShuttleGui;
class wxDataViewCtrl;
class wxCommandEvent;

class PluginRegistrationDialog final : public wxDialogWrapper
{
public:
    ///@param defaultCategory - sets the default effect plugins filter.
    ///Could be one of EffectType or set to -1 to disable it.
    PluginRegistrationDialog(wxWindow* parent, int defaultEffectCategory = -1);

private:

    void Populate();
    void PopulateOrExchange(ShuttleGui& S);
    void ReloadModel();

    void OnSearchTextChanged(wxCommandEvent& evt);
    void OnStateFilterValueChanged(wxCommandEvent& evt);
    void OnTypeFilterValueChanged(wxCommandEvent& evt);
    void OnCategoryFilterValueChanged(wxCommandEvent& evt);
    void OnOK(wxCommandEvent& evt);
    void OnCancel(wxCommandEvent& evt);
    void OnRescan(wxCommandEvent& evt);
    void OnGetMoreEffects(wxCommandEvent& evt);

    wxArrayString mPluginProviderIDs;

    wxDataViewCtrl* mPluginList{};
    wxObjectDataPtr<PluginDataModel> mPluginsModel;

    DECLARE_EVENT_TABLE()
};

#endif
