#pragma once

#include "wxPanelWrapper.h"
#include <wx/choice.h>
#include <wx/listbox.h>

class EffectPlugin;

class EffectPresetsDialog final : public wxDialogWrapper
{
public:
    EffectPresetsDialog(wxWindow* parent, EffectPlugin* effect);
    virtual ~EffectPresetsDialog();

    wxString GetSelected() const;
    void SetSelected(const wxString& parms);

private:
    void SetPrefix(const TranslatableString& type, const wxString& prefix);
    void UpdateUI();

    void OnType(wxCommandEvent& evt);
    void OnOk(wxCommandEvent& evt);
    void OnCancel(wxCommandEvent& evt);

private:
    wxChoice* mType;
    wxListBox* mPresets;

    RegistryPaths mFactoryPresets;
    RegistryPaths mUserPresets;
    wxString mSelection;

    DECLARE_EVENT_TABLE()
};
