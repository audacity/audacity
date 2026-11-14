/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOptionsHandler.cpp

  Vitaly Sverchinsky

**********************************************************************/

#include "ExportOptionsHandler.h"

#include "ExportUtils.h"
#include "ExportOptionsUIServices.h"

#include "ShuttleGui.h"

#include <wx/spinctrl.h>
#include <wx/choice.h>
#include <wx/checkbox.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/wupdlock.h>

#ifdef wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

wxDEFINE_EVENT(AUDACITY_FILE_SUFFIX_EVENT, wxCommandEvent);

ExportOptionsHandler::~ExportOptionsHandler() = default;

ExportOptionsHandler::ExportOptionsHandler(ShuttleGui& S, const ExportPlugin& plugin, int format)
{
    mParent = S.GetParent();

    mEditor = plugin.CreateOptionsEditor(format, this);
    if (mEditor) {
        mEditor->Load(*gPrefs);
        if (auto uiServices = dynamic_cast<ExportOptionsUIServices*>(mEditor.get())) {
            uiServices->PopulateUI(S);
        } else {
            PopulateOptions(S);
        }
    } else {
        PopulateEmpty(S);
    }
}

bool ExportOptionsHandler::TransferDataFromEditor()
{
    if (mEditor) {
        if (auto uiServices = dynamic_cast<ExportOptionsUIServices*>(mEditor.get())) {
            if (!uiServices->TransferDataFromWindow()) {
                return false;
            }
        }
        mEditor->Store(*gPrefs);
    }
    return true;
}

ExportProcessor::Parameters ExportOptionsHandler::GetParameters() const
{
    if (mEditor) {
        return ExportUtils::ParametersFromEditor(*mEditor);
    }
    return {};
}

void ExportOptionsHandler::SetParameters(const ExportProcessor::Parameters& parameters)
{
    if (mEditor) {
        for (const auto& p : parameters) {
            mEditor->SetValue(std::get<0>(p), std::get<1>(p));
        }
    }
}

ExportOptionsEditor::SampleRateList ExportOptionsHandler::GetSampleRateList() const
{
    if (mEditor) {
        return mEditor->GetSampleRateList();
    }
    return {};
}

void ExportOptionsHandler::PopulateEmpty(ShuttleGui& S)
{
    S.StartHorizontalLay(wxCENTER);
    {
        S.StartHorizontalLay(wxCENTER, 0);
        {
            S.Prop(1).AddTitle(XO("No format specific options"));
        }
        S.EndHorizontalLay();
    }
    S.EndHorizontalLay();
}

void ExportOptionsHandler::PopulateOptions(ShuttleGui& S)
{
    {
        S.StartMultiColumn(2, wxALIGN_LEFT);
        {
            for (int i = 0; i < mEditor->GetOptionsCount(); ++i) {
                ExportOption option;
                if (!mEditor->GetOption(i, option)) {
                    continue;
                }

                ExportValue value;
                if (!mEditor->GetValue(option.id, value)) {
                    continue;
                }

                wxControl* control { nullptr };

                auto prompt = S.AddPrompt(option.title);
                prompt->SetMinSize({ 140, -1 });

                if ((option.flags & ExportOption::TypeMask) == ExportOption::TypeEnum) {
                    int selected = -1;
                    TranslatableStrings list;

                    int index { 0 };
                    std::unordered_map<int, ExportValue> indexValueMap;
                    indexValueMap.reserve(option.values.size());
                    for (auto& e : option.values) {
                        list.push_back(option.names[index]);
                        if (value == e) {
                            selected = index;
                        }
                        indexValueMap[index] = e;
                        ++index;
                    }
                    control = S.AddChoice({}, list, selected);
                    control->Bind(wxEVT_CHOICE, [this, id = option.id, indexValueMap](const wxCommandEvent& evt)
                    {
                        const auto it = indexValueMap.find(evt.GetInt());
                        if (it != indexValueMap.end()) {
                            mEditor->SetValue(id, it->second);
                        }
                    });
                } else if (auto selected = std::get_if<bool>(&value)) {
                    control = S.Name(option.title).
                              AddCheckBox({}, *selected);
#if wxUSE_ACCESSIBILITY
                    safenew WindowAccessible(control);
#endif
                    control->Bind(wxEVT_CHECKBOX, [this, id = option.id](const wxCommandEvent& evt)
                    {
                        const auto checked = evt.GetInt() != 0;
                        mEditor->SetValue(id, checked);
                    });
                } else if (auto num = std::get_if<int>(&value)) {
                    if ((option.flags & ExportOption::TypeMask) == ExportOption::TypeRange) {
                        const int min = *std::get_if<int>(&option.values[0]);
                        const int max = *std::get_if<int>(&option.values[1]);
                        if (max - min < 20) {
                            control = S.Name(option.title)
                                      .AddSlider({}, *num, max, min);
                            control->Bind(wxEVT_SLIDER, [this, id = option.id](const wxCommandEvent& evt)
                            {
                                mEditor->SetValue(id, evt.GetInt());
                            });
                            control->SetMinSize({ 180, -1 });
                        } else {
                            control = S.AddSpinCtrl({}, *num, max, min);
                            control->Bind(wxEVT_SPINCTRL, [this, id = option.id](const wxSpinEvent& evt)
                            {
                                mEditor->SetValue(id, evt.GetInt());
                            });
                        }
                    } else {
                        control = S.AddNumericTextBox({}, wxString::Format("%d", *num), 0);
                        control->Bind(wxEVT_TEXT, [this, id = option.id](const wxCommandEvent& evt)
                        {
                            long num;
                            if (evt.GetString().ToLong(&num)) {
                                mEditor->SetValue(id, static_cast<int>(num));
                            }
                        });
                    }
                } else if (auto str = std::get_if<std::string>(&value)) {
                    control = S.AddTextBox({}, wxString::FromUTF8(*str), 0);
                    control->Bind(wxEVT_TEXT, [this, id = option.id](const wxCommandEvent& evt)
                    {
                        mEditor->SetValue(id, evt.GetString().ToStdString());
                    });
                }
                mIDRowIndexMap[option.id] = static_cast<int>(mRows.size());
                mRows.emplace_back(prompt, control);

                if (option.flags & ExportOption::ReadOnly) {
                    control->Disable();
                }
                if (option.flags & ExportOption::Hidden) {
                    prompt->Hide();
                    control->Hide();
                }
            }
        }
        S.EndMultiColumn();
    }
}

void ExportOptionsHandler::OnExportOptionChangeBegin()
{
    mUpdateLocker = std::make_unique<wxWindowUpdateLocker>(mParent);
}

void ExportOptionsHandler::OnExportOptionChangeEnd()
{
    mParent->Layout();
    mUpdateLocker.reset();
}

void ExportOptionsHandler::OnExportOptionChange(const ExportOption& option)
{
    const auto it = mIDRowIndexMap.find(option.id);
    if (it == mIDRowIndexMap.end()) {
        return;
    }

    const auto index = it->second;
    const auto [prompt, control] = mRows[index];

    const auto visible = (option.flags & ExportOption::Hidden) == 0;

    prompt->Show(visible);
    control->Show(visible);

    const auto enabled = (option.flags & ExportOption::ReadOnly) == 0;
    control->Enable(enabled);
}

void ExportOptionsHandler::OnFormatInfoChange()
{
    Publish({ ExportOptionsHandlerEvent::FormatInfoChange });
}

void ExportOptionsHandler::OnSampleRateListChange()
{
    Publish({ ExportOptionsHandlerEvent::SampleRateListChange });
}
