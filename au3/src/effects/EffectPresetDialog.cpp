#include "EffectPresetDialog.h"
#include "EffectManager.h" // GetUserPresets, ...
#include "EffectPlugin.h"
#include "ShuttleGui.h"

enum
{
    ID_Type = 10000
};

BEGIN_EVENT_TABLE(EffectPresetsDialog, wxDialogWrapper)
EVT_CHOICE(ID_Type, EffectPresetsDialog::OnType)
EVT_LISTBOX_DCLICK(wxID_ANY, EffectPresetsDialog::OnOk)
EVT_BUTTON(wxID_OK, EffectPresetsDialog::OnOk)
EVT_BUTTON(wxID_CANCEL, EffectPresetsDialog::OnCancel)
END_EVENT_TABLE()

EffectPresetsDialog::EffectPresetsDialog(wxWindow* parent, EffectPlugin* effect)
    : wxDialogWrapper(parent, wxID_ANY, XO("Select Preset"))
{
    ShuttleGui S(this, eIsCreating);
    S.StartVerticalLay();
    {
        S.StartTwoColumn();
        S.SetStretchyCol(1);
        {
            S.AddPrompt(XXO("Type:"));
            mType = S.Id(ID_Type).AddChoice({}, {}, 0);

            S.AddPrompt(XXO("&Preset:"));
            mPresets = S.Style(wxLB_SINGLE | wxLB_NEEDED_SB).AddListBox({});
        }
        S.EndTwoColumn();

        S.AddStandardButtons();
    }
    S.EndVerticalLay();

    mUserPresets = GetUserPresets(*effect);
    mFactoryPresets = effect->GetDefinition().GetFactoryPresets();

    if (mUserPresets.size() > 0) {
        mType->Append(_("User Presets"));
    }

    if (mFactoryPresets.size() > 0) {
        mType->Append(_("Factory Presets"));
    }

    if (HasCurrentSettings(*effect)) {
        mType->Append(_("Current Settings"));
    }

    if (HasFactoryDefaults(*effect)) {
        mType->Append(_("Factory Defaults"));
    }

    UpdateUI();
}

EffectPresetsDialog::~EffectPresetsDialog()
{
}

wxString EffectPresetsDialog::GetSelected() const
{
    return mSelection;
}

void EffectPresetsDialog::SetSelected(const wxString& parms)
{
    wxString preset = parms;
    if (preset.StartsWith(EffectPlugin::kUserPresetIdent)) {
        preset.Replace(EffectPlugin::kUserPresetIdent, wxEmptyString, false);
        SetPrefix(XO("User Presets"), preset);
    } else if (preset.StartsWith(EffectPlugin::kFactoryPresetIdent)) {
        preset.Replace(EffectPlugin::kFactoryPresetIdent, wxEmptyString, false);
        SetPrefix(XO("Factory Presets"), preset);
    } else if (preset.StartsWith(EffectPlugin::kCurrentSettingsIdent)) {
        SetPrefix(XO("Current Settings"), wxEmptyString);
    } else if (preset.StartsWith(EffectPlugin::kFactoryDefaultsIdent)) {
        SetPrefix(XO("Factory Defaults"), wxEmptyString);
    }
}

void EffectPresetsDialog::SetPrefix(
    const TranslatableString& type, const wxString& prefix)
{
    mType->SetStringSelection(type.Translation());

    if (type == XO("User Presets")) {
        mPresets->Clear();
        for (const auto& preset : mUserPresets) {
            mPresets->Append(preset);
        }
        mPresets->Enable(true);
        mPresets->SetStringSelection(prefix);
        if (mPresets->GetSelection() == wxNOT_FOUND) {
            mPresets->SetSelection(0);
        }
        mSelection
            =EffectPlugin::kUserPresetIdent + mPresets->GetStringSelection();
    } else if (type == XO("Factory Presets")) {
        mPresets->Clear();
        for (size_t i = 0, cnt = mFactoryPresets.size(); i < cnt; i++) {
            auto label = mFactoryPresets[i];
            if (label.empty()) {
                label = _("None");
            }
            mPresets->Append(label);
        }
        mPresets->Enable(true);
        mPresets->SetStringSelection(prefix);
        if (mPresets->GetSelection() == wxNOT_FOUND) {
            mPresets->SetSelection(0);
        }
        mSelection
            =EffectPlugin::kFactoryPresetIdent + mPresets->GetStringSelection();
    } else if (type == XO("Current Settings")) {
        mPresets->Clear();
        mPresets->Enable(false);
        mSelection = EffectPlugin::kCurrentSettingsIdent;
    } else if (type == XO("Factory Defaults")) {
        mPresets->Clear();
        mPresets->Enable(false);
        mSelection = EffectPlugin::kFactoryDefaultsIdent;
    }
}

void EffectPresetsDialog::UpdateUI()
{
    int selected = mType->GetSelection();
    if (selected == wxNOT_FOUND) {
        selected = 0;
        mType->SetSelection(selected);
    }
    wxString type = mType->GetString(selected);

    if (type == _("User Presets")) {
        selected = mPresets->GetSelection();
        if (selected == wxNOT_FOUND) {
            selected = 0;
        }

        mPresets->Clear();
        for (const auto& preset : mUserPresets) {
            mPresets->Append(preset);
        }
        mPresets->Enable(true);
        mPresets->SetSelection(selected);
        mSelection
            =EffectPlugin::kUserPresetIdent + mPresets->GetString(selected);
    } else if (type == _("Factory Presets")) {
        selected = mPresets->GetSelection();
        if (selected == wxNOT_FOUND) {
            selected = 0;
        }

        mPresets->Clear();
        for (size_t i = 0, cnt = mFactoryPresets.size(); i < cnt; i++) {
            auto label = mFactoryPresets[i];
            if (label.empty()) {
                label = _("None");
            }
            mPresets->Append(label);
        }
        mPresets->Enable(true);
        mPresets->SetSelection(selected);
        mSelection
            =EffectPlugin::kFactoryPresetIdent + mPresets->GetString(selected);
    } else if (type == _("Current Settings")) {
        mPresets->Clear();
        mPresets->Enable(false);
        mSelection = EffectPlugin::kCurrentSettingsIdent;
    } else if (type == _("Factory Defaults")) {
        mPresets->Clear();
        mPresets->Enable(false);
        mSelection = EffectPlugin::kFactoryDefaultsIdent;
    }
}

void EffectPresetsDialog::OnType(wxCommandEvent& WXUNUSED(evt))
{
    UpdateUI();
}

void EffectPresetsDialog::OnOk(wxCommandEvent& WXUNUSED(evt))
{
    UpdateUI();

    EndModal(true);
}

void EffectPresetsDialog::OnCancel(wxCommandEvent& WXUNUSED(evt))
{
    mSelection = wxEmptyString;

    EndModal(false);
}
