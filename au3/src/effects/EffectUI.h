/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectUI.h

  Leland Lucius

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#ifndef __AUDACITY_EFFECTUI_H__
#define __AUDACITY_EFFECTUI_H__

#include <optional>

#include "Identifier.h"
#include "EffectUIServices.h" // for DialogFactoryResults
#include "EffectPlugin.h" // for its nested types
#include "Observer.h"
#include "PluginInterface.h"
#include "commands/CommandManagerWindowClasses.h"
#include "RealtimeEffectManager.h"

struct AudioIOEvent;

#include "EffectInterface.h"
#include "wxPanelWrapper.h" // to inherit

#include "SelectedRegion.h"

class AudacityCommand;
class AudacityProject;
class EffectBase;
class RealtimeEffectState;

class wxCheckBox;
class AButton;

//
class EffectUIHost final : public wxDialogWrapper, public TopLevelKeystrokeHandlingWindow
{
public:
    // constructors and destructors
    /*
     @param[out] pInstance may construct
     (and then must call Init() with success), or leave null for failure
     */
    EffectUIHost(wxWindow* parent, AudacityProject& project, EffectBase& effect, EffectUIServices& client,
                 std::shared_ptr<EffectInstance>& pInstance, EffectSettingsAccess& access,
                 const std::shared_ptr<RealtimeEffectState>& pPriorState = {});
    virtual ~EffectUIHost();

    bool TransferDataToWindow() override;
    bool TransferDataFromWindow() override;

    int ShowModal() override;

    bool Initialize();
    EffectEditor* GetEditor() const { return mpEditor.get(); }

    bool HandleCommandKeystrokes() override;

    void SetClosed()
    {
#if wxDEBUG_LEVEL
        mClosed = true;
#endif
    }

private:
    std::shared_ptr<EffectInstance> InitializeInstance();

    void BuildTopBar(ShuttleGui& S);

    void OnInitDialog(wxInitDialogEvent& evt);
    void OnErase(wxEraseEvent& evt);
    void OnPaint(wxPaintEvent& evt);
    void OnClose(wxCloseEvent& evt);
    void OnApply(wxCommandEvent& evt);
    void DoCancel();
    void OnCancel(wxCommandEvent& evt);
    void OnDebug(wxCommandEvent& evt);
    void OnMenu(wxCommandEvent& evt);
    void OnEnable(wxCommandEvent& evt);
    void OnPlay(wxCommandEvent& evt);
    void OnPlayback(AudioIOEvent);
    void OnCapture(AudioIOEvent);
    void OnUserPreset(wxCommandEvent& evt);
    void OnFactoryPreset(wxCommandEvent& evt);
    void OnDeletePreset(wxCommandEvent& evt);
    void OnSaveAs(wxCommandEvent& evt);
    void OnImport(wxCommandEvent& evt);
    void OnExport(wxCommandEvent& evt);
    void OnOptions(wxCommandEvent& evt);
    void OnDefaults(wxCommandEvent& evt);
    void OnIdle(wxIdleEvent& evt);
    void OnCharHook(wxKeyEvent& evt);

    bool IsOpenedFromEffectPanel() const;

    void UpdateControls();
    wxBitmap CreateBitmap(const char* const xpm[], bool up, bool pusher);
    void LoadUserPresets();

    void CleanupRealtime();

private:
    Observer::Subscription mAudioIOSubscription, mEffectStateSubscription;

    AudacityProject& mProject;
    wxWindow* const mParent;
    EffectBase& mEffectUIHost;
    EffectUIServices& mClient;
    //! @invariant not null
    const EffectPlugin::EffectSettingsAccessPtr mpGivenAccess;
    EffectPlugin::EffectSettingsAccessPtr mpAccess;
    EffectPlugin::EffectSettingsAccessPtr mpAccess2;
    std::weak_ptr<RealtimeEffectState> mwState{};
    // Temporary state used for destructive processing
    std::shared_ptr<RealtimeEffectState> mpTempProjectState {};

    RegistryPaths mUserPresets;
    bool mInitialized{ false };
    const bool mSupportsRealtime;
    bool mIsGUI{};
    bool mIsBatch{};

    wxButton* mApplyBtn{};
    wxButton* mMenuBtn{};
    AButton* mEnableBtn{};
    wxButton* mDebugBtn{};

    bool mEnabled{ true };

    bool mCapturing{};

    SelectedRegion mRegion;
    double mPlayPos{ 0.0 };

    bool mDismissed{};
    const bool mHadPriorState;

#if wxDEBUG_LEVEL
    // Used only in an assertion
    bool mClosed{ false };
#endif

    const std::shared_ptr<EffectInstance> mpInstance;
    const EffectOutputs* const mpOutputs;

    std::unique_ptr<EffectEditor> mpEditor;

    DECLARE_EVENT_TABLE()
};

class CommandContext;

namespace  EffectUI {
AUDACITY_DLL_API
DialogFactoryResults DialogFactory(wxWindow& parent, EffectBase& host, EffectUIServices& client, EffectSettingsAccess& access);
}

class ShuttleGui;

// Obsolescent dialog still used only in Noise Reduction/Removal
class AUDACITY_DLL_API EffectDialog /* not final */ : public wxDialogWrapper
{
public:
    // constructors and destructors
    EffectDialog(wxWindow* parent, const TranslatableString& title, int type = 0, int flags = wxDEFAULT_DIALOG_STYLE,
                 int additionalButtons = 0);

    void Init();

    bool TransferDataToWindow() override;
    bool TransferDataFromWindow() override;
    bool Validate() override;

    // NEW virtuals:
    virtual void PopulateOrExchange(ShuttleGui& S);
    virtual void OnPreview(wxCommandEvent& evt);
    virtual void OnOk(wxCommandEvent& evt);

private:
    int mType;
    int mAdditionalButtons;

    DECLARE_EVENT_TABLE()
    wxDECLARE_NO_COPY_CLASS(EffectDialog);
};

#if defined(__WXMAC__)
void MacMakeWindowFloating(NSView* handle);
#endif

#endif // __AUDACITY_EFFECTUI_H__
