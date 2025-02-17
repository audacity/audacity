/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectUI.cpp

  Leland Lucius

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/
#include "EffectUI.h"
#include "Effect.h"
#include "StatefulEffectUIServices.h"
#include "EffectEditor.h"
#include "EffectPreview.h"

#include "AllThemeResources.h"
#include "widgets/BasicMenu.h"
#include "BasicUI.h"
#include "CommandManager.h"
#include "ConfigInterface.h"
#include "EffectManager.h"
#include "DoEffect.h"
#include "PluginManager.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "../ProjectWindowBase.h"
#include "../ProjectWindows.h"
#include "TrackFocus.h"
#include "RealtimeEffectList.h"
#include "RealtimeEffectManager.h"
#include "RealtimeEffectState.h"
#include "Theme.h"
#include "UndoManager.h"
#include "Viewport.h"
#include "wxWidgetsWindowPlacement.h"

static PluginID GetID(EffectPlugin& effect)
{
    return PluginManager::GetID(&effect.GetDefinition());
}

///////////////////////////////////////////////////////////////////////////////
//
// EffectPanel
//
///////////////////////////////////////////////////////////////////////////////

class EffectPanel final : public wxPanelWrapper
{
public:
    EffectPanel(wxWindow* parent)
        :  wxPanelWrapper(parent)
    {
        // This fools NVDA into not saying "Panel" when the dialog gets focus
        SetName(TranslatableString::Inaudible);
        SetLabel(TranslatableString::Inaudible);

        mAcceptsFocus = true;
    }

    virtual ~EffectPanel()
    {
    }

    // ============================================================================
    // wxWindow implementation
    // ============================================================================

    bool AcceptsFocus() const override
    {
        return mAcceptsFocus;
    }

    // So that wxPanel is not included in Tab traversal, when required - see wxWidgets bug 15581
    bool AcceptsFocusFromKeyboard() const override
    {
        return mAcceptsFocus;
    }

    // ============================================================================
    // EffectPanel implementation
    // ============================================================================
    void SetAccept(bool accept)
    {
        mAcceptsFocus = accept;
    }

private:
    bool mAcceptsFocus;
};

///////////////////////////////////////////////////////////////////////////////
//
// EffectUIHost
//
///////////////////////////////////////////////////////////////////////////////

#include "../../images/Effect.h"
#include "AudioIO.h"
#include "../CommonCommandFlags.h"
#include "../prefs/GUISettings.h" // for RTL_WORKAROUND
#include "Project.h"
#include "../ProjectAudioManager.h"
#include "ShuttleGui.h"
#include "ViewInfo.h"
#include "CommandContext.h"
#include "AudacityMessageBox.h"
#include "HelpSystem.h"
#include "../widgets/AButton.h"

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/menu.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>

static const int kDummyID = 20000;
static const int kSaveAsID = 20001;
static const int kImportID = 20002;
static const int kExportID = 20003;
static const int kDefaultsID = 20004;
static const int kOptionsID = 20005;
static const int kUserPresetsDummyID = 20006;
static const int kDeletePresetDummyID = 20007;
static const int kMenuID = 20100;
static const int kEnableID = 20101;
static const int kPlayID = EffectEditor::kPlayID;
static const int kPlaybackID = 20105;
static const int kCaptureID = 20106;
static const int kUserPresetsID = 21000;
static const int kDeletePresetID = 22000;
static const int kFactoryPresetsID = 23000;

BEGIN_EVENT_TABLE(EffectUIHost, wxDialogWrapper)
EVT_INIT_DIALOG(EffectUIHost::OnInitDialog)
EVT_ERASE_BACKGROUND(EffectUIHost::OnErase)
EVT_PAINT(EffectUIHost::OnPaint)
EVT_CLOSE(EffectUIHost::OnClose)
EVT_BUTTON(eDebugID, EffectUIHost::OnApply)
EVT_BUTTON(wxID_CANCEL, EffectUIHost::OnCancel)
EVT_BUTTON(wxID_APPLY, EffectUIHost::OnDebug)
EVT_BUTTON(kMenuID, EffectUIHost::OnMenu)
EVT_BUTTON(kEnableID, EffectUIHost::OnEnable)
EVT_BUTTON(kPlayID, EffectUIHost::OnPlay)
EVT_MENU(kSaveAsID, EffectUIHost::OnSaveAs)
EVT_MENU(kImportID, EffectUIHost::OnImport)
EVT_MENU(kExportID, EffectUIHost::OnExport)
EVT_MENU(kOptionsID, EffectUIHost::OnOptions)
EVT_MENU(kDefaultsID, EffectUIHost::OnDefaults)
EVT_MENU_RANGE(kUserPresetsID, kUserPresetsID + 999, EffectUIHost::OnUserPreset)
EVT_MENU_RANGE(kDeletePresetID, kDeletePresetID + 999, EffectUIHost::OnDeletePreset)
EVT_MENU_RANGE(kFactoryPresetsID, kFactoryPresetsID + 999, EffectUIHost::OnFactoryPreset)
EVT_IDLE(EffectUIHost::OnIdle)
EVT_CHAR_HOOK(EffectUIHost::OnCharHook)
END_EVENT_TABLE()

namespace {
//! Decorate an EffectSettingsAccess with a `Set` that replicates changes
//! into a second EffectSettingsAccess, while that one still exists
/*! Name inspired by `man 1 tee` */
class EffectSettingsAccessTee : public EffectSettingsAccess
{
public:
    EffectSettingsAccessTee(EffectSettingsAccess& main, const std::shared_ptr<EffectSettingsAccess>& pSide = {});
    const EffectSettings& Get() override;
    void Set(EffectSettings&& settings, std::unique_ptr<Message> pMessage) override;
    void Set(std::unique_ptr<Message> pMessage) override;
    void Flush() override;
    bool IsSameAs(const EffectSettingsAccess& other) const override;
private:
    //! @invariant not null
    const std::shared_ptr<EffectSettingsAccess> mpMain;
    const std::weak_ptr<EffectSettingsAccess> mwSide;
};
}

EffectSettingsAccessTee::EffectSettingsAccessTee(
    EffectSettingsAccess& main,
    const std::shared_ptr<EffectSettingsAccess>& pSide)
    : mpMain{main.shared_from_this()}  //! Guarantee lifetime of main
    , mwSide{pSide}  //! Do not control lifetime of side
{
}

const EffectSettings& EffectSettingsAccessTee::Get()
{
    return mpMain->Get();
}

void EffectSettingsAccessTee::Set(EffectSettings&& settings,
                                  std::unique_ptr<Message> pMessage)
{
    // Move copies of the given settings and message into the side
    if (auto pSide = mwSide.lock()) {
        pSide->Set(EffectSettings { settings },
                   pMessage ? pMessage->Clone() : nullptr);
    }
    // Move the given settings and message through
    mpMain->Set(std::move(settings), std::move(pMessage));
}

void EffectSettingsAccessTee::Set(std::unique_ptr<Message> pMessage)
{
    // Move copies of the given message into the side
    if (auto pSide = mwSide.lock()) {
        pSide->Set(pMessage ? pMessage->Clone() : nullptr);
    }
    // Move the given message through
    mpMain->Set(std::move(pMessage));
}

void EffectSettingsAccessTee::Flush()
{
    mpMain->Flush();
    if (auto pSide = mwSide.lock()) {
        pSide->Flush();
    }
}

bool EffectSettingsAccessTee::IsSameAs(
    const EffectSettingsAccess& other) const
{
    return mpMain->IsSameAs(other);
}

EffectUIHost::EffectUIHost(wxWindow* parent,
                           AudacityProject& project, EffectBase& effect,
                           EffectUIServices& client, std::shared_ptr<EffectInstance>& pInstance,
                           EffectSettingsAccess& access,
                           const std::shared_ptr<RealtimeEffectState>& pPriorState)
    :  wxDialogWrapper(parent, wxID_ANY, effect.GetDefinition().GetName(),
                       wxDefaultPosition, wxDefaultSize,
                       wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMINIMIZE_BOX | wxMAXIMIZE_BOX)
    , mEffectUIHost{effect}
    , mClient{client}
// Grab a pointer to the access object,
// extending its lifetime while this remains:
    , mpGivenAccess{access.shared_from_this()}
    , mpAccess{mpGivenAccess}
    , mwState{pPriorState}
    , mProject{project}
    , mParent{parent}
    , mSupportsRealtime{mEffectUIHost.GetDefinition().SupportsRealtime()}
    , mHadPriorState{(pPriorState != nullptr)}
    , mpInstance{InitializeInstance()}
    , mpOutputs{pPriorState ? pPriorState->GetOutputs() : nullptr}
{
    // Assign the out parameter
    pInstance = mpInstance;
#if defined(__WXMAC__)
    MacMakeWindowFloating(GetHandle());
#endif

    SetName(effect.GetDefinition().GetName());

    // This style causes Validate() and TransferDataFromWindow() to visit
    // sub-windows recursively, applying any wxValidators
    SetExtraStyle(GetExtraStyle() | wxWS_EX_VALIDATE_RECURSIVELY);
}

EffectUIHost::~EffectUIHost()
{
    if (mpEditor) {
        mpEditor->Disconnect();
    }
    DestroyChildren();
    wxASSERT(mClosed);
}

// ============================================================================
// wxWindow implementation
// ============================================================================

bool EffectUIHost::TransferDataToWindow()
{
    // Transfer-to takes const reference to settings
    const auto pServices
        =dynamic_cast<StatefulEffectUIServices*>(&mEffectUIHost);
    return (!pServices || pServices->TransferDataToWindow(mpAccess->Get()))
           &&//! Do other appearance updates
           mpEditor->UpdateUI()
           &&//! Do validators
           wxDialogWrapper::TransferDataToWindow();
}

bool EffectUIHost::TransferDataFromWindow()
{
    //! Do validations of any wxValidator objects
    if (!wxDialogWrapper::Validate()) {
        return false;
    }

    //! Do transfers of any wxValidator objects
    if (!wxDialogWrapper::TransferDataFromWindow()) {
        return false;
    }

    //! Do other custom validation and transfer actions
    if (!mpEditor->ValidateUI()) {
        return false;
    }

    // Transfer-from takes non-const reference to settings
    bool result = true;
    mpAccess->ModifySettings([&](EffectSettings& settings){
        const auto pServices
            =dynamic_cast<StatefulEffectUIServices*>(&mEffectUIHost);
        // Allow other transfers, and reassignment of settings
        result = (!pServices || pServices->TransferDataFromWindow(settings));
        if (result) {
            auto& definition = mEffectUIHost.GetDefinition();
            if (definition.GetType() == EffectTypeGenerate) {
                const auto seconds = settings.extra.GetDuration();
                // Updating of the last-used generator duration in the config
                SetConfig(definition, PluginSettings::Private,
                          CurrentSettingsGroup(), EffectSettingsExtra::DurationKey(),
                          seconds);
            }
        }
        return nullptr;
    });
    mpAccess->Flush();
    return result;
}

// ============================================================================
// wxDialog implementation
// ============================================================================

int EffectUIHost::ShowModal()
{
    if (mEffectUIHost.GetDefinition().GetType() == EffectTypeGenerate) {
        mApplyBtn->SetLabel(XXO("&Generate").Translation());
    }

    Layout();

    return wxDialogWrapper::ShowModal();
}

// ============================================================================
// EffectUIHost implementation
// ============================================================================

namespace {
AButton* MakeBitmapToggleButton(wxWindow* parent,
                                const wxImage& ImageOn, const wxImage& ImageOff)
{
    auto pBtn = safenew AButton(parent, kEnableID,
                                wxDefaultPosition, wxDefaultSize, true);
    pBtn->SetImages(ImageOff, ImageOff, ImageOn, ImageOn, ImageOff);
    return pBtn;
}

constexpr int InnerMargin = 3;
}

void EffectUIHost::BuildTopBar(ShuttleGui& S)
{
    S.StartPanel();
    {
        S.SetBorder(InnerMargin);

        S.StartHorizontalLay(wxEXPAND, 0);
        {
            if (IsOpenedFromEffectPanel()) {
                mEnableBtn = MakeBitmapToggleButton(S.GetParent(),
                                                    theTheme.Image(bmpEffectOn), theTheme.Image(bmpEffectOff));
                mEnableBtn->SetBackgroundColour(GetBackgroundColour());
                S
                .Position(wxALIGN_CENTER | wxTOP | wxBOTTOM)
                .Name(XO("Power"))
                .AddWindow(mEnableBtn);
            }

            mMenuBtn = S.Id(kMenuID)
                       .ToolTip(XO("Manage presets and options"))
                       .AddButton(XO("Presets && settings"), wxALIGN_CENTER | wxTOP | wxBOTTOM);

            S.AddSpace(1, 0, 1);

            if (mEffectUIHost.GetDefinition().EnablesDebug()) {
                mDebugBtn = S.Id(eDebugID)
                            .AddButton(XXO("Debu&g"),
                                       wxALIGN_CENTER | wxTOP | wxBOTTOM);
            }
        }
        S.EndHorizontalLay();
    }
    S.EndPanel();
}

bool EffectUIHost::Initialize()
{
    mEnabled = mpAccess->Get().extra.GetActive();

    // Build a "host" dialog, framing a panel that the client fills in.
    // The frame includes buttons to preview, apply, load and save presets, etc.
    EffectPanel* w {};
    ShuttleGui S{ this, eIsCreating };
    {
        BuildTopBar(S);

        // Make the panel for the client
        Destroy_ptr<EffectPanel> uw{ safenew EffectPanel(S.GetParent()) };
        RTL_WORKAROUND(uw.get());

        // Try to give the window a sensible default/minimum size
        uw->SetMinSize(wxSize(wxMax(600, mParent->GetSize().GetWidth() * 2 / 3),
                              mParent->GetSize().GetHeight() / 2));

        // Let the client add things to the panel
        ShuttleGui S1{ uw.get(), eIsCreating };
        mpEditor = mClient.PopulateUI(mEffectUIHost,
                                      S1, *mpInstance, *mpAccess, mpOutputs);
        if (!mpEditor) {
            return false;
        }

        mIsGUI = mpEditor->IsGraphicalUI();
        mIsBatch = mEffectUIHost.IsBatchProcessing();

        S.StartHorizontalLay(wxEXPAND);
        {
            S.Prop(1)
            .Position(wxEXPAND)
            .AddWindow((w = uw.release()));
        }
        S.EndHorizontalLay();

        if (!IsOpenedFromEffectPanel()) {
            S.StartPanel();
            {
                S.SetBorder(InnerMargin);
                S.StartHorizontalLay(wxEXPAND, 0);
                {
                    if (!mIsBatch) {
                        if (mEffectUIHost.GetDefinition().GetType() != EffectTypeAnalyze
                            && mEffectUIHost.GetDefinition().GetType() != EffectTypeTool) {
                            S.Id(kPlayID)
                            .ToolTip(XO("Preview effect"))
                            .AddButton(XXO("&Preview"),
                                       wxALIGN_CENTER | wxTOP | wxBOTTOM);
                        }
                    }

                    S.AddSpace(1, 1, 1);
                    S.Id(wxID_CANCEL)
                    .AddButton(XXO("&Cancel"));

                    mApplyBtn = S.Id(wxID_APPLY)
                                .AddButton(XXO("&Apply"));
                    mApplyBtn->SetDefault();
                }
                S.EndHorizontalLay();
            }
            S.EndPanel();
        }
    }

    Layout();
    Fit();
    Center();

    UpdateControls();

    w->SetAccept(!mIsGUI);

    if (!mIsGUI) {
        w->SetFocus();
    } else if (!IsOpenedFromEffectPanel()) {
        mApplyBtn->SetFocus();
    }

    LoadUserPresets();

    SetMinSize(GetSize());
    return true;
}

bool EffectUIHost::HandleCommandKeystrokes()
{
    return !IsModal();
}

void EffectUIHost::OnInitDialog(wxInitDialogEvent& evt)
{
    // Do default handling
    wxDialogWrapper::OnInitDialog(evt);

#if wxCHECK_VERSION(3, 0, 0)
    //#warning "check to see if this still needed in wx3"
#endif

    // Pure hackage coming down the pike...
    //
    // I have no idea why, but if a wxTextCtrl is the first control in the
    // panel, then its contents will not be automatically selected when the
    // dialog is displayed.
    //
    // So, we do the selection manually.
    wxTextCtrl* focused = wxDynamicCast(FindFocus(), wxTextCtrl);
    if (focused) {
        focused->SelectAll();
    }
}

void EffectUIHost::OnErase(wxEraseEvent& WXUNUSED(evt))
{
    // Ignore it
}

void EffectUIHost::OnPaint(wxPaintEvent& WXUNUSED(evt))
{
    wxPaintDC dc(this);

    dc.Clear();
}

void EffectUIHost::OnClose(wxCloseEvent& WXUNUSED(evt))
{
    DoCancel();
    CleanupRealtime();

    if (mpEditor) {
        mpEditor->OnClose();
    }

    Hide();
    Destroy();

#if wxDEBUG_LEVEL
    mClosed = true;
#endif
}

void EffectUIHost::OnApply(wxCommandEvent& evt)
{
    auto& project = mProject;

    // On wxGTK (wx2.8.12), the default action is still executed even if
    // the button is disabled.  This appears to affect all wxDialogs, not
    // just our Effects dialogs.  So, this is a only temporary workaround
    // for legacy effects that disable the OK button.  Hopefully this has
    // been corrected in wx3.
    if (!mApplyBtn->IsEnabled()) {
        return;
    }

    // Honor the "select all if none" preference...a little hackish, but whatcha gonna do...
    if (!mIsBatch
        && mEffectUIHost.GetDefinition().GetType() != EffectTypeGenerate
        && mEffectUIHost.GetDefinition().GetType() != EffectTypeTool
        && ViewInfo::Get(project).selectedRegion.isPoint()) {
        auto flags = AlwaysEnabledFlag;
        bool allowed
            =CommandManager::Get(project).ReportIfActionNotAllowed(
                  mEffectUIHost.GetDefinition().GetName(),
                  flags,
                  WaveTracksSelectedFlag() | TimeSelectedFlag());
        if (!allowed) {
            return;
        }
    }

    if (!TransferDataFromWindow()
        ||// This is the main place where there is a side-effect on the config
          // file to remember the last-used settings of an effect, just before
          // applying the effect destructively.
        !mEffectUIHost.GetDefinition()
        .SaveUserPreset(CurrentSettingsGroup(), mpAccess->Get())) {
        return;
    }

    if (IsModal()) {
        mDismissed = true;

        EndModal(evt.GetId());

        Close();

        return;
    }

    // Progress dialog no longer yields, so this "shouldn't" be necessary (yet to be proven
    // for sure), but it is a nice visual cue that something is going on.
    mApplyBtn->Disable();
    auto cleanup = finally([&] { mApplyBtn->Enable(); });

    CommandContext context(project);
    // This is absolute hackage...but easy and I can't think of another way just now.
    //
    // It should callback to the EffectManager to kick off the processing
    EffectUI::DoEffect(GetID(mEffectUIHost), context.project,
                       EffectManager::kConfigured);
}

void EffectUIHost::DoCancel()
{
    if (!mDismissed) {
        if (!mHadPriorState) {
            // For the destructive effect dialog only
            // Restore effect state from last updated preferences
            mpAccess->ModifySettings([&](EffectSettings& settings) {
                // ignore failure
                return mEffectUIHost.GetDefinition().LoadUserPreset(
                    CurrentSettingsGroup(), settings).value_or(nullptr);
            });
        }
        if (IsModal()) {
            EndModal(0);
        } else {
            Hide();
        }

        mDismissed = true;
    }
}

void EffectUIHost::OnCancel(wxCommandEvent& WXUNUSED(evt))
{
    DoCancel();
    Close();
}

void EffectUIHost::OnDebug(wxCommandEvent& evt)
{
    OnApply(evt);
}

namespace {
wxString GetVersionForDisplay(const EffectDefinitionInterface& definition)
{
    static const auto specialVersion = XO("n/a");
    auto result = definition.GetVersion();
    if (result == specialVersion.MSGID()) {
        result = specialVersion.Translation();
    }
    return result;
}
}

void EffectUIHost::OnMenu(wxCommandEvent& WXUNUSED(evt))
{
    wxMenu menu;
    menu.Bind(wxEVT_MENU, [](auto&){}, kUserPresetsDummyID);
    menu.Bind(wxEVT_MENU, [](auto&){}, kDeletePresetDummyID);
    LoadUserPresets();

    if (mUserPresets.size() == 0) {
        menu.Append(kUserPresetsDummyID, _("User Presets"))->Enable(false);
    } else {
        auto sub = std::make_unique<wxMenu>();
        for (size_t i = 0, cnt = mUserPresets.size(); i < cnt; i++) {
            sub->Append(kUserPresetsID + i, mUserPresets[i]);
        }
        menu.Append(0, _("User Presets"), sub.release());
    }

    menu.Append(kSaveAsID, _("Save Preset..."));

    if (mUserPresets.size() == 0) {
        menu.Append(kDeletePresetDummyID, _("Delete Preset"))->Enable(false);
    } else {
        auto sub = std::make_unique<wxMenu>();
        for (size_t i = 0, cnt = mUserPresets.size(); i < cnt; i++) {
            sub->Append(kDeletePresetID + i, mUserPresets[i]);
        }
        menu.Append(0, _("Delete Preset"), sub.release());
    }

    menu.AppendSeparator();

    auto factory = mEffectUIHost.GetDefinition().GetFactoryPresets();

    {
        auto sub = std::make_unique<wxMenu>();
        sub->Append(kDefaultsID, _("Defaults"));
        if (factory.size() > 0) {
            sub->AppendSeparator();
            for (size_t i = 0, cnt = factory.size(); i < cnt; i++) {
                auto label = factory[i];
                if (label.empty()) {
                    label = _("None");
                }

                sub->Append(kFactoryPresetsID + i, label);
            }
        }
        menu.Append(0, _("Factory Presets"), sub.release());
    }

    menu.AppendSeparator();
    menu.Append(kImportID, _("Import..."))
    ->Enable(mEffectUIHost.CanExportPresets());
    menu.Append(kExportID, _("Export..."))
    ->Enable(mEffectUIHost.CanExportPresets());
    menu.AppendSeparator();
    menu.Append(kOptionsID, _("Options..."))
    ->Enable(mEffectUIHost.HasOptions());
    menu.AppendSeparator();

    {
        auto sub = std::make_unique<wxMenu>();

        auto& definition = mEffectUIHost.GetDefinition();
        sub->Append(kDummyID, wxString::Format(_("Type: %s"),
                                               ::wxGetTranslation(definition.GetFamily().Translation())));
        sub->Append(kDummyID, wxString::Format(_("Name: %s"), definition.GetName().Translation()));
        sub->Append(kDummyID, wxString::Format(_("Version: %s"),
                                               GetVersionForDisplay(definition)));
        sub->Append(kDummyID, wxString::Format(_("Vendor: %s"), definition.GetVendor().Translation()));
        sub->Append(kDummyID, wxString::Format(_("Description: %s"), definition.GetDescription().Translation()));
        sub->Bind(wxEVT_MENU, [](auto&){}, kDummyID);

        menu.Append(0, _("About"), sub.release());
    }

    wxWindow* btn = FindWindow(kMenuID);
    wxRect r = btn->GetRect();
    BasicMenu::Handle{ &menu }.Popup(
        wxWidgetsWindowPlacement { btn },
        { r.GetLeft(), r.GetBottom() }
        );
}

void EffectUIHost::OnEnable(wxCommandEvent& WXUNUSED(evt))
{
    mEnabled = mEnableBtn->IsDown();

    auto mpState = mwState.lock();
    if (mpState) {
        mpState->SetActive(mEnabled);
        UndoManager::Get(mProject).MarkUnsaved();
    }

    UpdateControls();
}

void EffectUIHost::OnPlay(wxCommandEvent& WXUNUSED(evt))
{
    if (!TransferDataFromWindow()) {
        return;
    }

    auto updater = [this]{ TransferDataToWindow(); };
    EffectPreview(mEffectUIHost, *mpAccess, updater, false);
    // After restoration of settings and effect state:
    // In case any dialog control depends on mT1 or mDuration:
    updater();

    return;
}

void EffectUIHost::OnCapture(AudioIOEvent evt)
{
    if (evt.on) {
        if (evt.pProject == &mProject) {
            mCapturing = true;
        }
    } else {
        mCapturing = false;
    }
    UpdateControls();
}

void EffectUIHost::OnUserPreset(wxCommandEvent& evt)
{
    int preset = evt.GetId() - kUserPresetsID;

    mpAccess->ModifySettings([&](EffectSettings& settings){
        // ignore failure
        return mEffectUIHost.GetDefinition().LoadUserPreset(
            UserPresetsGroup(mUserPresets[preset]), settings).value_or(nullptr);
    });
    TransferDataToWindow();
    return;
}

void EffectUIHost::OnFactoryPreset(wxCommandEvent& evt)
{
    mpAccess->ModifySettings([&](EffectSettings& settings){
        //! ignore failure
        return mEffectUIHost.GetDefinition().LoadFactoryPreset(
            evt.GetId() - kFactoryPresetsID, settings).value_or(nullptr);
    });
    TransferDataToWindow();
    return;
}

void EffectUIHost::OnDeletePreset(wxCommandEvent& evt)
{
    auto preset = mUserPresets[evt.GetId() - kDeletePresetID];

    int res = AudacityMessageBox(
        XO("Are you sure you want to delete \"%s\"?").Format(preset),
        XO("Delete Preset"),
        wxICON_QUESTION | wxYES_NO);
    if (res == wxYES) {
        RemoveConfigSubgroup(mEffectUIHost.GetDefinition(),
                             PluginSettings::Private, UserPresetsGroup(preset));
    }

    LoadUserPresets();

    return;
}

void EffectUIHost::OnSaveAs(wxCommandEvent& WXUNUSED(evt))
{
    wxTextCtrl* text;
    wxString name;
    wxDialogWrapper dlg(this, wxID_ANY, XO("Save Preset"));

    ShuttleGui S(&dlg, eIsCreating);

    S.StartPanel();
    {
        S.StartVerticalLay(1);
        {
            S.StartHorizontalLay(wxALIGN_LEFT, 0);
            {
                text = S.AddTextBox(XXO("Preset name:"), name, 30);
            }
            S.EndHorizontalLay();
            S.SetBorder(10);
            S.AddStandardButtons();
        }
        S.EndVerticalLay();
    }
    S.EndPanel();

    dlg.SetSize(dlg.GetSizer()->GetMinSize());
    dlg.Center();
    dlg.Fit();

    while (true)
    {
        int rc = dlg.ShowModal();

        if (rc != wxID_OK) {
            break;
        }

        name = text->GetValue();
        if (name.empty()) {
            AudacityMessageDialog md(
                this,
                XO("You must specify a name"),
                XO("Save Preset"));
            md.Center();
            md.ShowModal();
            continue;
        }

        if (make_iterator_range(mUserPresets).contains(name)) {
            AudacityMessageDialog md(
                this,
                XO("Preset already exists.\n\nReplace?"),
                XO("Save Preset"),
                wxYES_NO | wxCANCEL | wxICON_EXCLAMATION);
            md.Center();
            int choice = md.ShowModal();
            if (choice == wxID_CANCEL) {
                break;
            }

            if (choice == wxID_NO) {
                continue;
            }
        }

        if (TransferDataFromWindow()) {
            mEffectUIHost.GetDefinition()
            .SaveUserPreset(UserPresetsGroup(name), mpAccess->Get());
        }
        LoadUserPresets();

        break;
    }

    return;
}

void EffectUIHost::OnImport(wxCommandEvent& WXUNUSED(evt))
{
    mpAccess->ModifySettings([&](EffectSettings& settings){
        // ignore failure
        return mClient.ImportPresets(mEffectUIHost, settings).value_or(nullptr);
    });
    TransferDataToWindow();
    LoadUserPresets();

    return;
}

void EffectUIHost::OnExport(wxCommandEvent& WXUNUSED(evt))
{
    // may throw
    // exceptions are handled in AudacityApp::OnExceptionInMainLoop
    if (TransferDataFromWindow()) {
        mClient.ExportPresets(mEffectUIHost, mpAccess->Get());
    }

    return;
}

void EffectUIHost::OnOptions(wxCommandEvent& WXUNUSED(evt))
{
    mClient.ShowOptions(mEffectUIHost);

    return;
}

void EffectUIHost::OnDefaults(wxCommandEvent& WXUNUSED(evt))
{
    mpAccess->ModifySettings([&](EffectSettings& settings){
        // ignore failure
        return mEffectUIHost.GetDefinition().LoadFactoryDefaults(settings)
               .value_or(nullptr);
    });
    TransferDataToWindow();
    return;
}

void EffectUIHost::OnIdle(wxIdleEvent& evt)
{
    evt.Skip();
    if (mpAccess) {
        mpAccess->Flush();
    }
}

void EffectUIHost::OnCharHook(wxKeyEvent& evt)
{
    if (!IsEscapeKey(evt)) {
        evt.Skip();
        return;
    }

    if (IsOpenedFromEffectPanel()) {
        Close();
    } else {
        wxCommandEvent cancelEvt { wxEVT_COMMAND_BUTTON_CLICKED, wxID_CANCEL };

        OnCancel(cancelEvt);
    }
}

bool EffectUIHost::IsOpenedFromEffectPanel() const
{
    return mpTempProjectState == nullptr && mSupportsRealtime;
}

wxBitmap EffectUIHost::CreateBitmap(const char* const xpm[], bool up, bool pusher)
{
    wxMemoryDC dc;
    wxBitmap pic(xpm);

    wxBitmap mod(pic.GetWidth() + 6, pic.GetHeight() + 6, 24);
    dc.SelectObject(mod);

#if defined(__WXGTK__)
    wxColour newColour = wxSystemSettings::GetColour(wxSYS_COLOUR_BACKGROUND);
#else
    wxColour newColour = wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE);
#endif

    dc.SetBackground(wxBrush(newColour));
    dc.Clear();

    int offset = 3;
    if (pusher) {
        if (!up) {
            offset += 1;
        }
    }

    dc.DrawBitmap(pic, offset, offset, true);

    dc.SelectObject(wxNullBitmap);

    return mod;
}

void EffectUIHost::UpdateControls()
{
    if (mIsBatch) {
        return;
    }

    if (IsOpenedFromEffectPanel()) {
        mEnabled ? mEnableBtn->PushDown() : mEnableBtn->PopUp();
        return;
    }

    mApplyBtn->Enable(!mCapturing);
}

void EffectUIHost::LoadUserPresets()
{
    mUserPresets.clear();

    GetConfigSubgroups(mEffectUIHost.GetDefinition(),
                       PluginSettings::Private, UserPresetsGroup(wxEmptyString), mUserPresets);

    std::sort(mUserPresets.begin(), mUserPresets.end());

    return;
}

std::shared_ptr<EffectInstance> EffectUIHost::InitializeInstance()
{
    // We are still constructing and the return initializes a const member
    std::shared_ptr<EffectInstance> result;

    auto mpState = mwState.lock();

    bool priorState = (mpState != nullptr);
    if (!priorState) {
        auto gAudioIO = AudioIO::Get();
        mCapturing = gAudioIO->IsStreamActive() && gAudioIO->GetNumCaptureChannels() > 0 && !gAudioIO->IsMonitoring();
    }

    if (mSupportsRealtime && !mInitialized) {
        if (!priorState) {
            mwState = mpState = mpTempProjectState
                                    =AudioIO::Get()->AddState(mProject, nullptr, GetID(mEffectUIHost));
        }
        if (mpState) {
            // Find the right instance to connect to the dialog
            if (!result) {
                result = mpState->GetInstance();
            }

            mpAccess2 = mpState->GetAccess();
            if (!(mpAccess2->IsSameAs(*mpAccess))) {
                // Decorate the given access object
                mpAccess = std::make_shared<EffectSettingsAccessTee>(
                    *mpAccess, mpAccess2);
            }

            mEffectStateSubscription = mpState->Subscribe([this](RealtimeEffectStateChange state) {
                mEnabled = (state == RealtimeEffectStateChange::EffectOn);
                UpdateControls();
            });
        }

        if (!priorState) {
            mAudioIOSubscription = AudioIO::Get()->Subscribe([this](AudioIOEvent event){
                switch (event.type) {
                    case AudioIOEvent::CAPTURE:
                        OnCapture(event);
                        break;
                    default:
                        break;
                }
            });
        }

        mInitialized = true;
    } else {
        result = EffectBase::FindInstance(mEffectUIHost).value_or(nullptr);
    }

    return result;
}

void EffectUIHost::CleanupRealtime()
{
    mAudioIOSubscription.Reset();

    if (mSupportsRealtime && mInitialized) {
        if (!IsOpenedFromEffectPanel()) {
            AudioIO::Get()->RemoveState(mProject, nullptr, mpTempProjectState);
            mEffectStateSubscription.Reset();
            mpTempProjectState.reset();
            /*
               ProjectHistory::Get(mProject).PushState(
                  XO("Removed %s effect").Format(mpState->GetEffect()->GetName()),
                  XO("Removed Effect"),
                  UndoPush::NONE
               );
             */
        }
        mInitialized = false;
    }
}

DialogFactoryResults EffectUI::DialogFactory(wxWindow& parent,
                                             EffectBase& host, EffectUIServices& client,
                                             EffectSettingsAccess& access)
{
    // Make sure there is an associated project, whose lifetime will
    // govern the lifetime of the dialog, even when the dialog is
    // non-modal, as for realtime effects
    auto project = FindProjectFromWindow(&parent);
    if (!project) {
        return {}
    }
    std::shared_ptr<EffectInstance> pInstance;
    Destroy_ptr<EffectUIHost> dlg{ safenew EffectUIHost{ &parent,
                                                         *project, host, client, pInstance, access } };
    if (!pInstance) {
        dlg->SetClosed();
        return {};
    }
    if (dlg->Initialize()) {
        auto pEditor = dlg->GetEditor();
        // release() is safe because parent will own it
        return { dlg.release(), pInstance, pEditor };
    }
    return {};
}

#include "PluginManager.h"
#include "ProjectRate.h"
#include "../SelectUtilities.h"
#include "WaveTrack.h"
#include "CommandManager.h"

///////////////////////////////////////////////////////////////////////////////
BEGIN_EVENT_TABLE(EffectDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, EffectDialog::OnOk)
END_EVENT_TABLE()

EffectDialog::EffectDialog(wxWindow* parent,
                           const TranslatableString& title,
                           int type,
                           int flags,
                           int additionalButtons)
    : wxDialogWrapper(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, flags)
{
    mType = type;
    mAdditionalButtons = additionalButtons;
}

void EffectDialog::Init()
{
    long buttons = eOkButton;
    if ((mType != EffectTypeAnalyze) && (mType != EffectTypeTool)) {
        buttons |= eCancelButton;
        if (mType == EffectTypeProcess) {
            buttons |= ePreviewButton;
        }
    }

    ShuttleGui S(this, eIsCreating);

    S.SetBorder(5);
    S.StartVerticalLay(true);
    {
        PopulateOrExchange(S);
        S.AddStandardButtons(buttons | mAdditionalButtons);
    }
    S.EndVerticalLay();

    Layout();
    Fit();
    SetMinSize(GetSize());
    Center();
}

/// This is a virtual function which will be overridden to
/// provide the actual parameters that we want for each
/// kind of dialog.
void EffectDialog::PopulateOrExchange(ShuttleGui& WXUNUSED(S))
{
    return;
}

bool EffectDialog::TransferDataToWindow()
{
    ShuttleGui S(this, eIsSettingToDialog);
    PopulateOrExchange(S);

    return true;
}

bool EffectDialog::TransferDataFromWindow()
{
    ShuttleGui S(this, eIsGettingFromDialog);
    PopulateOrExchange(S);

    return true;
}

bool EffectDialog::Validate()
{
    return true;
}

void EffectDialog::OnPreview(wxCommandEvent& WXUNUSED(evt))
{
    return;
}

void EffectDialog::OnOk(wxCommandEvent& WXUNUSED(evt))
{
    // On wxGTK (wx2.8.12), the default action is still executed even if
    // the button is disabled.  This appears to affect all wxDialogs, not
    // just our Effects dialogs.  So, this is a only temporary workaround
    // for legacy effects that disable the OK button.  Hopefully this has
    // been corrected in wx3.
    if (FindWindow(wxID_OK)->IsEnabled() && Validate() && TransferDataFromWindow()) {
        EndModal(wxID_OK);
    }

    return;
}

//! Inject a factory for realtime effects
#include "RealtimeEffectState.h"
static RealtimeEffectState::EffectFactory::Scope
    scope{ &EffectManager::GetInstanceFactory };
