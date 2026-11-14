/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.cpp

  Dominic Mazzoni

******************************************************************//**

\class NyquistBase
\brief An Effect that calls up a Nyquist (XLISP) plug-in, i.e. many possible
effects from this one class.

*//****************************************************************//**

\class NyquistOutputDialog
\brief Dialog used with NyquistBase

*//****************************************************************//**

\class NyqControl
\brief A control on a NyquistDialog.

*//*******************************************************************/

#include "Nyquist.h"

#include <wx/choice.h>
#include <wx/scrolwin.h>
#include <wx/sizer.h>
#include <wx/tokenzr.h>
#include <wx/utils.h> // wxYieldIfNeeded
#include <wx/valgen.h>

#include "../../commands/ScriptCommandRelay.h" // ExecFromMain
#include "../../tracks/playabletrack/wavetrack/ui/WaveChannelView.h"
#include "../../widgets/NumericTextCtrl.h"
#include "../../widgets/valnum.h"
#include "../EffectEditor.h"
#include "BasicUI.h"
#include "ShuttleGui.h"
#include "EffectManager.h"

#include <nyx.h>
#ifndef nyx_returns_start_and_end_time
#error You need to update lib-src/libnyquist
#endif

#include <cfloat>

enum
{
    ID_Editor = 10000,
    ID_Load,
    ID_Save,

    ID_Slider = 11000,
    ID_Text = 12000,
    ID_Choice = 13000,
    ID_Time = 14000,
    ID_FILE = 15000
};

BEGIN_EVENT_TABLE(NyquistEffect, wxEvtHandler)
EVT_BUTTON(ID_Load, NyquistEffect::OnLoad)
EVT_BUTTON(ID_Save, NyquistEffect::OnSave)

EVT_COMMAND_RANGE(ID_Slider, ID_Slider + 99,
                  wxEVT_COMMAND_SLIDER_UPDATED, NyquistEffect::OnSlider)
EVT_COMMAND_RANGE(ID_Text, ID_Text + 99,
                  wxEVT_COMMAND_TEXT_UPDATED, NyquistEffect::OnText)
EVT_COMMAND_RANGE(ID_Choice, ID_Choice + 99,
                  wxEVT_COMMAND_CHOICE_SELECTED, NyquistEffect::OnChoice)
EVT_COMMAND_RANGE(ID_Time, ID_Time + 99,
                  wxEVT_COMMAND_TEXT_UPDATED, NyquistEffect::OnTime)
EVT_COMMAND_RANGE(ID_FILE, ID_FILE + 99,
                  wxEVT_COMMAND_BUTTON_CLICKED, NyquistEffect::OnFileButton)
END_EVENT_TABLE()

int NyquistEffect::ShowHostInterface(EffectBase& plugin,
                                     wxWindow& parent, const EffectDialogFactory& factory,
                                     std::shared_ptr<EffectInstance>& pInstance, EffectSettingsAccess& access,
                                     bool forceModal)
{
    int res = wxID_APPLY;
    if (!(Effect::TestUIFlags(EffectManager::kRepeatNyquistPrompt) && mIsPrompt)) {
        // Show the normal (prompt or effect) interface
        res = EffectUIServices::ShowHostInterface(plugin,
                                                  parent, factory, pInstance, access, forceModal);
    }

    // Remember if the user clicked debug
    mDebug = (res == eDebugID);

    // We're done if the user clicked "Close", we are not the Nyquist Prompt,
    // or the program currently loaded into the prompt doesn't have a UI.
    if (!res || !mIsPrompt || mControls.size() == 0 || !pInstance) {
        return res;
    }

    // Nyquist prompt was OK, but gave us some magic ;control comments to
    // reinterpret into a second dialog

    NyquistEffect effect(NYQUIST_WORKER_ID);
    effect.SetCommand(mInputCmd);
    Finally Do{ [&]{
            // A second dialog will use effect as a pushed event handler.
            // wxWidgets delays window destruction until idle time.
            // Yield to destroy the dialog while effect is still in scope.
            BasicUI::Yield();
        } };

    // Must give effect its own settings to interpret, not those in access
    // Let's also give it its own instance
    auto newSettings = effect.MakeSettings();
    auto pNewInstance = effect.MakeInstance();
    auto newAccess = std::make_shared<SimpleEffectSettingsAccess>(newSettings);

    if (IsBatchProcessing()) {
        effect.SetBatchProcessing();

        CommandParameters cp;
        cp.SetParameters(mParameters);
        effect.LoadSettings(cp, newSettings);

        // Show the normal (prompt or effect) interface
        // Don't pass this as first argument, pass the worker to itself
        res = effect.ShowHostInterface(effect,
                                       parent, factory, pNewInstance, *newAccess, forceModal);
        if (res) {
            CommandParameters cp;
            effect.SaveSettings(newSettings, cp);
            cp.GetParameters(mParameters);
        }
    } else {
        if (!factory) {
            return 0;
        }
        // Don't pass this as first argument, pass the worker to itself
        res = effect.ShowHostInterface(effect,
                                       parent, factory, pNewInstance, *newAccess, false);
        if (!res) {
            return 0;
        }

        // Wrap the new settings in the old settings
        access.ModifySettings([&](EffectSettings& settings){
            auto& nyquistSettings = GetSettings(settings);
            nyquistSettings.proxySettings = std::move(newSettings);
            nyquistSettings.proxyDebug = this->mDebug;
            nyquistSettings.controls = move(effect.mControls);
            return nullptr;
        });
    }
    if (!pNewInstance) {
        // Propagate the failure from nested ShowHostInterface
        pInstance.reset();
    }
    return res;
}

std::unique_ptr<EffectEditor> NyquistEffect::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    if (mIsPrompt) {
        BuildPromptWindow(S);
    } else {
        BuildEffectWindow(S);
    }
    return nullptr;
}

bool NyquistEffect::TransferDataToWindow(const EffectSettings&)
{
    mUIParent->TransferDataToWindow();

    bool success;
    if (mIsPrompt) {
        success = TransferDataToPromptWindow();
    } else {
        success = TransferDataToEffectWindow();
    }

    if (success) {
        EffectEditor::EnablePreview(mUIParent, mEnablePreview);
    }

    return success;
}

bool NyquistEffect::TransferDataFromWindow(EffectSettings&)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    if (mIsPrompt) {
        return TransferDataFromPromptWindow();
    }
    return TransferDataFromEffectWindow();
}

bool NyquistEffect::TransferDataToPromptWindow()
{
    mCommandText->ChangeValue(mInputCmd);

    return true;
}

bool NyquistEffect::TransferDataToEffectWindow()
{
    for (size_t i = 0, cnt = mControls.size(); i < cnt; i++) {
        NyqControl& ctrl = mControls[i];

        if (ctrl.type == NYQ_CTRL_CHOICE) {
            const auto count = ctrl.choices.size();

            int val = (int)ctrl.val;
            if (val < 0 || val >= (int)count) {
                val = 0;
            }

            wxChoice* c = (wxChoice*)mUIParent->FindWindow(ID_Choice + i);
            c->SetSelection(val);
        } else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_FLOAT) {
            // wxTextCtrls are handled by the validators
            double range = ctrl.high - ctrl.low;
            int val = (int)(0.5 + ctrl.ticks * (ctrl.val - ctrl.low) / range);
            wxSlider* s = (wxSlider*)mUIParent->FindWindow(ID_Slider + i);
            s->SetValue(val);
        } else if (ctrl.type == NYQ_CTRL_TIME) {
            NumericTextCtrl* n = (NumericTextCtrl*)mUIParent->FindWindow(ID_Time + i);
            n->SetValue(ctrl.val);
        }
    }

    return true;
}

bool NyquistEffect::TransferDataFromPromptWindow()
{
    mInputCmd = mCommandText->GetValue();

    // Un-correct smart quoting, bothersomely applied in wxTextCtrl by
    // the native widget of MacOS 10.9 SDK
    const wxString left = wxT("\u201c"), right = wxT("\u201d"), dumb = '"';
    mInputCmd.Replace(left, dumb, true);
    mInputCmd.Replace(right, dumb, true);

    const wxString leftSingle = wxT("\u2018"), rightSingle = wxT("\u2019"),
                   dumbSingle = '\'';
    mInputCmd.Replace(leftSingle, dumbSingle, true);
    mInputCmd.Replace(rightSingle, dumbSingle, true);

    return ParseCommand(mInputCmd);
}

bool NyquistEffect::TransferDataFromEffectWindow()
{
    if (mControls.size() == 0) {
        return true;
    }

    for (unsigned int i = 0; i < mControls.size(); i++) {
        NyqControl* ctrl = &mControls[i];

        if (ctrl->type == NYQ_CTRL_STRING || ctrl->type == NYQ_CTRL_TEXT) {
            continue;
        }

        if (ctrl->val == UNINITIALIZED_CONTROL) {
            ctrl->val = GetCtrlValue(ctrl->valStr);
        }

        if (ctrl->type == NYQ_CTRL_CHOICE) {
            continue;
        }

        if (ctrl->type == NYQ_CTRL_FILE) {
            resolveFilePath(ctrl->valStr);

            wxString path;
            if (ctrl->valStr.StartsWith("\"", &path)) {
                // Validate if a list of quoted paths.
                if (path.EndsWith("\"", &path)) {
                    path.Replace("\"\"", "\"");
                    wxStringTokenizer tokenizer(path, "\"");
                    while (tokenizer.HasMoreTokens())
                    {
                        wxString token = tokenizer.GetNextToken();
                        if (!validatePath(token)) {
                            const auto message
                                =XO("\"%s\" is not a valid file path.").Format(token);
                            EffectUIServices::DoMessageBox(*this,
                                                           message,
                                                           wxOK | wxICON_EXCLAMATION | wxCENTRE,
                                                           XO("Error"));
                            return false;
                        }
                    }
                    continue;
                } else {
                    const auto message
                        =/* i18n-hint: Warning that there is one quotation mark rather than a pair.*/
                          XO("Mismatched quotes in\n%s").Format(ctrl->valStr);
                    EffectUIServices::DoMessageBox(*this,
                                                   message,
                                                   wxOK | wxICON_EXCLAMATION | wxCENTRE,
                                                   XO("Error"));
                    return false;
                }
            }
            // Validate a single path.
            else if (validatePath(ctrl->valStr)) {
                continue;
            }

            // Validation failed
            const auto message
                =XO("\"%s\" is not a valid file path.").Format(ctrl->valStr);
            EffectUIServices::DoMessageBox(*this,
                                           message,
                                           wxOK | wxICON_EXCLAMATION | wxCENTRE,
                                           XO("Error"));
            return false;
        }

        if (ctrl->type == NYQ_CTRL_TIME) {
            NumericTextCtrl* n = (NumericTextCtrl*)mUIParent->FindWindow(ID_Time + i);
            ctrl->val = n->GetValue();
        }

        if (ctrl->type == NYQ_CTRL_INT_TEXT && ctrl->lowStr.IsSameAs(wxT("nil"), false)) {
            ctrl->low = INT_MIN;
        } else if ((ctrl->type == NYQ_CTRL_FLOAT_TEXT || ctrl->type == NYQ_CTRL_TIME)
                   && ctrl->lowStr.IsSameAs(wxT("nil"), false)) {
            ctrl->low = -(FLT_MAX);
        } else {
            ctrl->low = GetCtrlValue(ctrl->lowStr);
        }

        if (ctrl->type == NYQ_CTRL_INT_TEXT && ctrl->highStr.IsSameAs(wxT("nil"), false)) {
            ctrl->high = INT_MAX;
        } else if ((ctrl->type == NYQ_CTRL_FLOAT_TEXT || ctrl->type == NYQ_CTRL_TIME)
                   && ctrl->highStr.IsSameAs(wxT("nil"), false)) {
            ctrl->high = FLT_MAX;
        } else {
            ctrl->high = GetCtrlValue(ctrl->highStr);
        }

        if (ctrl->high < ctrl->low) {
            ctrl->high = ctrl->low + 1;
        }

        if (ctrl->val < ctrl->low) {
            ctrl->val = ctrl->low;
        }

        if (ctrl->val > ctrl->high) {
            ctrl->val = ctrl->high;
        }

        ctrl->ticks = 1000;
        if (ctrl->type == NYQ_CTRL_INT
            && (ctrl->high - ctrl->low < ctrl->ticks)) {
            ctrl->ticks = (int)(ctrl->high - ctrl->low);
        }
    }

    return true;
}

void NyquistEffect::BuildPromptWindow(ShuttleGui& S)
{
    S.StartVerticalLay();
    {
        S.StartMultiColumn(3, wxEXPAND);
        {
            S.SetStretchyCol(1);

            S.AddVariableText(XO("Enter Nyquist Command: "));

            S.AddSpace(1, 1);
        }
        S.EndMultiColumn();

        S.StartHorizontalLay(wxEXPAND, 1);
        {
            mCommandText = S.Focus()
                           .MinSize({ 500, 200 })
                           .AddTextWindow(wxT(""));
        }
        S.EndHorizontalLay();

        S.StartHorizontalLay(wxALIGN_CENTER, 0);
        {
            S.Id(ID_Load).AddButton(XXO("&Load"));
            S.Id(ID_Save).AddButton(XXO("&Save"));
        }
        S.EndHorizontalLay();
    }
    S.EndVerticalLay();
}

void NyquistEffect::BuildEffectWindow(ShuttleGui& S)
{
    wxScrolledWindow* scroller = S.Style(wxVSCROLL | wxTAB_TRAVERSAL)
                                 .StartScroller(2);
    {
        S.StartMultiColumn(4);
        {
            for (size_t i = 0; i < mControls.size(); i++) {
                NyqControl& ctrl = mControls[i];

                if (ctrl.type == NYQ_CTRL_TEXT) {
                    S.EndMultiColumn();
                    S.StartHorizontalLay(wxALIGN_LEFT, 0);
                    {
                        S.AddSpace(0, 10);
                        S.AddFixedText(Verbatim(ctrl.label), false);
                    }
                    S.EndHorizontalLay();
                    S.StartMultiColumn(4);
                } else {
                    auto prompt = XXO("%s:").Format(ctrl.name);
                    S.AddPrompt(prompt);

                    if (ctrl.type == NYQ_CTRL_STRING) {
                        S.AddSpace(10, 10);

                        auto item = S.Id(ID_Text + i)
                                    .Validator<wxGenericValidator>(&ctrl.valStr)
                                    .Name(prompt)
                                    .AddTextBox({}, wxT(""), 50);
                    } else if (ctrl.type == NYQ_CTRL_CHOICE) {
                        S.AddSpace(10, 10);

                        S.Id(ID_Choice + i).AddChoice({},
                                                      Msgids(ctrl.choices.data(), ctrl.choices.size()));
                    } else if (ctrl.type == NYQ_CTRL_TIME) {
                        S.AddSpace(10, 10);

                        const auto options = NumericTextCtrl::Options{}
                        .AutoPos(true)
                        .MenuEnabled(true)
                        .ReadOnly(false);

                        NumericTextCtrl* time = safenew
                                       NumericTextCtrl(FormatterContext::SampleRateContext(mProjectRate),
                                                       S.GetParent(), (ID_Time + i),
                                                       NumericConverterType_TIME(),
                                                       GetSelectionFormat(),
                                                       ctrl.val,
                                                       options);
                        S
                        .Name(prompt)
                        .Position(wxALIGN_LEFT | wxALL)
                        .AddWindow(time);
                    } else if (ctrl.type == NYQ_CTRL_FILE) {
                        S.AddSpace(10, 10);

                        // Get default file extension if specified in wildcards
                        FileExtension defaultExtension;
                        if (!ctrl.fileTypes.empty()) {
                            const auto& type = ctrl.fileTypes[0];
                            if (!type.extensions.empty()) {
                                defaultExtension = type.extensions[0];
                            }
                        }
                        resolveFilePath(ctrl.valStr, defaultExtension);

                        wxTextCtrl* item = S.Id(ID_Text + i)
                                           .Name(prompt)
                                           .AddTextBox({}, wxT(""), 40);
                        item->SetValidator(wxGenericValidator(&ctrl.valStr));

                        if (ctrl.label.empty()) {
                            // We'd expect wxFileSelectorPromptStr to already be translated, but apparently not.
                            ctrl.label = wxGetTranslation(wxFileSelectorPromptStr);
                        }
                        S.Id(ID_FILE + i).AddButton(
                            Verbatim(ctrl.label), wxALIGN_LEFT);
                    } else {
                        // Integer or Real
                        if (ctrl.type == NYQ_CTRL_INT_TEXT || ctrl.type == NYQ_CTRL_FLOAT_TEXT) {
                            S.AddSpace(10, 10);
                        }

                        S.Id(ID_Text + i);
                        if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT) {
                            double range = ctrl.high - ctrl.low;
                            S.Validator<FloatingPointValidator<double> >(
                                // > 12 decimal places can cause rounding errors in display.
                                12, &ctrl.val,
                                // Set number of decimal places
                                (range < 10
                                 ? NumValidatorStyle::THREE_TRAILING_ZEROES
                                 : range < 100
                                 ? NumValidatorStyle::TWO_TRAILING_ZEROES
                                 : NumValidatorStyle::ONE_TRAILING_ZERO),
                                ctrl.low, ctrl.high
                                );
                        } else {
                            S.Validator<IntegerValidator<double> >(
                                &ctrl.val, NumValidatorStyle::DEFAULT,
                                (int)ctrl.low, (int)ctrl.high);
                        }
                        wxTextCtrl* item = S
                                           .Name(prompt)
                                           .AddTextBox({}, wxT(""),
                                                       (ctrl.type == NYQ_CTRL_INT_TEXT
                                                        || ctrl.type == NYQ_CTRL_FLOAT_TEXT) ? 25 : 12);

                        if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_FLOAT) {
                            S.Id(ID_Slider + i)
                            .Style(wxSL_HORIZONTAL)
                            .MinSize({ 150, -1 })
                            .AddSlider({}, 0, ctrl.ticks, 0);
                        }
                    }

                    if (ctrl.type != NYQ_CTRL_FILE) {
                        if (ctrl.type == NYQ_CTRL_CHOICE || ctrl.label.empty()) {
                            S.AddSpace(10, 10);
                        } else {
                            S.AddUnits(Verbatim(ctrl.label));
                        }
                    }
                }
            }
        }
        S.EndMultiColumn();
    }
    S.EndScroller();

    scroller->SetScrollRate(0, 20);

    // This fools NVDA into not saying "Panel" when the dialog gets focus
    scroller->SetName(wxT("\a"));
    scroller->SetLabel(wxT("\a"));
}

static const FileNames::FileType
/* i18n-hint: Nyquist is the name of a programming language */
    NyquistScripts = { XO("Nyquist scripts"), { wxT("ny") }, true }
/* i18n-hint: Lisp is the name of a programming language */
, LispScripts = { XO("Lisp scripts"), { wxT("lsp") }, true }
;

void NyquistEffect::OnLoad(wxCommandEvent& WXUNUSED(evt))
{
    if (mCommandText->IsModified()) {
        if (wxNO == EffectUIServices::DoMessageBox(*this,
                                                   XO("Current program has been modified.\nDiscard changes?"),
                                                   wxYES_NO)) {
            return;
        }
    }

    FileDialogWrapper dlog(
        mUIParent,
        XO("Load Nyquist script"),
        mFileName.GetPath(),
        wxEmptyString,
    {
        NyquistScripts,
        LispScripts,
        FileNames::TextFiles,
        FileNames::AllFiles
    },
        wxFD_OPEN | wxRESIZE_BORDER);

    if (dlog.ShowModal() != wxID_OK) {
        return;
    }

    mFileName = dlog.GetPath();

    if (!mCommandText->LoadFile(mFileName.GetFullPath())) {
        EffectUIServices::DoMessageBox(*this, XO("File could not be loaded"));
    }
}

void NyquistEffect::OnSave(wxCommandEvent& WXUNUSED(evt))
{
    FileDialogWrapper dlog(
        mUIParent,
        XO("Save Nyquist script"),
        mFileName.GetPath(),
        mFileName.GetFullName(),
    {
        NyquistScripts,
        LispScripts,
        FileNames::AllFiles
    },
        wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER);

    if (dlog.ShowModal() != wxID_OK) {
        return;
    }

    mFileName = dlog.GetPath();

    if (!mCommandText->SaveFile(mFileName.GetFullPath())) {
        EffectUIServices::DoMessageBox(*this, XO("File could not be saved"));
    }
}

void NyquistEffect::OnSlider(wxCommandEvent& evt)
{
    int i = evt.GetId() - ID_Slider;
    NyqControl& ctrl = mControls[i];

    int val = evt.GetInt();
    double range = ctrl.high - ctrl.low;
    double newVal = (val / (double)ctrl.ticks) * range + ctrl.low;

    // Determine precision for displayed number
    int precision = range < 1.0 ? 3
                    : range < 10.0 ? 2
                    : range < 100.0 ? 1
                    : 0;

    // If the value is at least one tick different from the current value
    // change it (this prevents changes from manually entered values unless
    // the slider actually moved)
    if (fabs(newVal - ctrl.val) >= (1 / (double)ctrl.ticks) * range
        && fabs(newVal - ctrl.val) >= pow(0.1, precision) / 2) {
        // First round to the appropriate precision
        newVal *= pow(10.0, precision);
        newVal = floor(newVal + 0.5);
        newVal /= pow(10.0, precision);

        ctrl.val = newVal;

        mUIParent->FindWindow(ID_Text + i)->GetValidator()->TransferToWindow();
    }
}

void NyquistEffect::OnChoice(wxCommandEvent& evt)
{
    mControls[evt.GetId() - ID_Choice].val = (double)evt.GetInt();
}

void NyquistEffect::OnTime(wxCommandEvent& evt)
{
    int i = evt.GetId() - ID_Time;
    static double value = 0.0;
    NyqControl& ctrl = mControls[i];

    NumericTextCtrl* n = (NumericTextCtrl*)mUIParent->FindWindow(ID_Time + i);
    double val = n->GetValue();

    // Observed that two events transmitted on each control change (Linux)
    // so skip if value has not changed.
    if (val != value) {
        if (val < ctrl.low || val > ctrl.high) {
            const auto message = XO("Value range:\n%s to %s")
                                 .Format(ToTimeFormat(ctrl.low), ToTimeFormat(ctrl.high));
            EffectUIServices::DoMessageBox(*this,
                                           message,
                                           wxOK | wxCENTRE,
                                           XO("Value Error"));
        }

        if (val < ctrl.low) {
            val = ctrl.low;
        } else if (val > ctrl.high) {
            val = ctrl.high;
        }

        n->SetValue(val);
        value = val;
    }
}

void NyquistEffect::OnFileButton(wxCommandEvent& evt)
{
    int i = evt.GetId() - ID_FILE;
    NyqControl& ctrl = mControls[i];

    // Get style flags:
    // Ensure legal combinations so that wxWidgets does not throw an assert error.
    unsigned int flags = 0;
    if (!ctrl.highStr.empty()) {
        wxStringTokenizer tokenizer(ctrl.highStr, ",");
        while (tokenizer.HasMoreTokens())
        {
            wxString token = tokenizer.GetNextToken().Trim(true).Trim(false);
            if (token.IsSameAs("open", false)) {
                flags |= wxFD_OPEN;
                flags &= ~wxFD_SAVE;
                flags &= ~wxFD_OVERWRITE_PROMPT;
            } else if (token.IsSameAs("save", false)) {
                flags |= wxFD_SAVE;
                flags &= ~wxFD_OPEN;
                flags &= ~wxFD_MULTIPLE;
                flags &= ~wxFD_FILE_MUST_EXIST;
            } else if (token.IsSameAs("overwrite", false) && !(flags & wxFD_OPEN)) {
                flags |= wxFD_OVERWRITE_PROMPT;
            } else if (token.IsSameAs("exists", false) && !(flags & wxFD_SAVE)) {
                flags |= wxFD_FILE_MUST_EXIST;
            } else if (token.IsSameAs("multiple", false) && !(flags & wxFD_SAVE)) {
                flags |= wxFD_MULTIPLE;
            }
        }
    }

    resolveFilePath(ctrl.valStr);

    wxFileName fname = ctrl.valStr;
    wxString defaultDir = fname.GetPath();
    wxString defaultFile = fname.GetName();
    auto message = XO("Select a file");

    if (flags & wxFD_MULTIPLE) {
        message = XO("Select one or more files");
    } else if (flags & wxFD_SAVE) {
        message = XO("Save file as");
    }

    FileDialogWrapper openFileDialog(mUIParent->FindWindow(ID_FILE + i),
                                     message,
                                     defaultDir,
                                     defaultFile,
                                     ctrl.fileTypes,
                                     flags); // styles

    if (openFileDialog.ShowModal() == wxID_CANCEL) {
        return;
    }

    wxString path;
    // When multiple files selected, return file paths as a list of quoted strings.
    if (flags & wxFD_MULTIPLE) {
        wxArrayString selectedFiles;
        openFileDialog.GetPaths(selectedFiles);

        for (size_t sf = 0; sf < selectedFiles.size(); sf++) {
            path += "\"";
            path += selectedFiles[sf];
            path += "\"";
        }
        ctrl.valStr = path;
    } else {
        ctrl.valStr = openFileDialog.GetPath();
    }

    mUIParent->FindWindow(ID_Text + i)->GetValidator()->TransferToWindow();
}

void NyquistEffect::OnText(wxCommandEvent& evt)
{
    int i = evt.GetId() - ID_Text;

    NyqControl& ctrl = mControls[i];

    if (wxDynamicCast(evt.GetEventObject(), wxWindow)->GetValidator()->TransferFromWindow()) {
        if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_INT) {
            int pos = (int)floor((ctrl.val - ctrl.low)
                                 / (ctrl.high - ctrl.low) * ctrl.ticks + 0.5);

            wxSlider* slider = (wxSlider*)mUIParent->FindWindow(ID_Slider + i);
            slider->SetValue(pos);
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
//
// NyquistOutputDialog
//
///////////////////////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(NyquistOutputDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, NyquistOutputDialog::OnOk)
END_EVENT_TABLE()

NyquistOutputDialog::NyquistOutputDialog(const TranslatableString& title,
                                         const TranslatableString& message)
    : wxDialogWrapper{nullptr, -1, title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER}
{
    SetName();

    ShuttleGui S{ this, eIsCreating };
    {
        S.SetBorder(10);

        S.AddVariableText(XO("Debug Output: "), false, wxALIGN_LEFT | wxLEFT | wxTOP | wxRIGHT);

        // TODO: use ShowInfoDialog() instead.
        // Beware this dialog MUST work with screen readers.
        S.Prop(1)
        .Position(wxEXPAND | wxALL)
        .MinSize({ 480, 250 })
        .Style(wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH)
        .AddTextWindow(message.Translation());

        S.SetBorder(5);

        S.StartHorizontalLay(wxALIGN_CENTRE | wxLEFT | wxBOTTOM | wxRIGHT, 0);
        {
            /* i18n-hint: In most languages OK is to be translated as OK.  It appears on a button.*/
            S.Id(wxID_OK).AddButton(XXO("OK"), wxALIGN_CENTRE, true);
        }
        S.EndHorizontalLay();
    }

    SetAutoLayout(true);
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
}

// ============================================================================
// NyquistOutputDialog implementation
// ============================================================================

void NyquistOutputDialog::OnOk(wxCommandEvent& /* event */)
{
    EndModal(wxID_OK);
}

static NyquistBase::GetEffectHook::Scope getEffectHookScope {
    [](const PluginID& path) {
        // Returned object must implement EffectUIServices for display of wxWidget
        // UI to be possible.
        return std::make_unique<NyquistEffect>(path);
    }
};

static NyquistBase::GetDisplaysHook::Scope getDisplaysHookScope {
    [](const WaveTrack* track) {
        auto pView = WaveChannelView::FindFirst(track);
        return pView ? pView->GetDisplays()
               : std::vector<WaveChannelSubView::Type> {};
    }
};

static NyquistBase::ShowDebugOutputHook::Scope showDebugOutputHookScope {
    [](const TranslatableString& title, const TranslatableString& message) {
        NyquistOutputDialog dialog { title, message };
        dialog.CentreOnParent();
        dialog.ShowModal();
    }
};

static NyquistBase::ExecFromMainHook::Scope execFromMainHookScope {
    [](wxString* pIn, wxString* pOut) { ExecFromMain(pIn, pOut); }
};

static NyquistBase::YieldIfNeededHook::Scope yieldIfNeededHookScope { []() {
// LLL:  STF figured out that yielding while the effect is being applied
//       produces an EXTREME slowdown.  It appears that yielding is not
//       really necessary on Linux and Windows.
//
//       However, on the Mac, the spinning cursor appears during longer
//       Nyquist processing and that may cause the user to think Audacity
//       has crashed or hung.  In addition, yielding or not on the Mac
//       doesn't seem to make much of a difference in execution time.
//
//       So, yielding on the Mac only...
#if defined(__WXMAC__)
        wxYieldIfNeeded();
#endif
    } };
