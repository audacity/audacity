/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectUI.h

  Leland Lucius

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/

#ifndef __AUDACITY_EFFECTUI_H__
#define __AUDACITY_EFFECTUI_H__

#include <wx/bitmap.h> // member variables

#include "Identifier.h"
#include "PluginInterface.h"

#if defined(EXPERIMENTAL_EFFECTS_RACK)

#include <vector>

#include <wx/defs.h>
#include <wx/frame.h> // to inherit
#include <wx/timer.h> // member variable

class wxFlexGridSizer;
class wxPanel;
class wxStaticText;

class AudacityProject;

class Effect;
using EffectArray = std::vector<Effect*>;

class EffectRack final : public wxFrame
{
public:
   EffectRack( AudacityProject &project );
   virtual ~EffectRack();

   void Add(Effect *effect, bool active = false, bool favorite = false);

   static EffectRack &Get( AudacityProject &project );

private:

   wxBitmap CreateBitmap(const char *const xpm[], bool up, bool pusher);
   int GetEffectIndex(wxWindow *win);
   void MoveRowUp(int row);
   void UpdateActive();

   void OnClose(wxCloseEvent & evt);
   void OnTimer(wxTimerEvent & evt);
   void OnApply(wxCommandEvent & evt);
   void OnBypass(wxCommandEvent & evt);

   void OnPower(wxCommandEvent & evt);
   void OnEditor(wxCommandEvent & evt);
   void OnUp(wxCommandEvent & evt);
   void OnDown(wxCommandEvent & evt);
   void OnFav(wxCommandEvent & evt);
   void OnRemove(wxCommandEvent & evt);

private:
   AudacityProject &mProject;

   wxStaticText *mLatency;
   int mLastLatency;

   wxBitmap mPowerPushed;
   wxBitmap mPowerRaised;
   wxBitmap mSettingsPushed;
   wxBitmap mSettingsRaised;
   wxBitmap mUpPushed;
   wxBitmap mUpRaised;
   wxBitmap mUpDisabled;
   wxBitmap mDownPushed;
   wxBitmap mDownRaised;
   wxBitmap mDownDisabled;
   wxBitmap mFavPushed;
   wxBitmap mFavRaised;
   wxBitmap mRemovePushed;
   wxBitmap mRemoveRaised;

   std::vector<int> mPowerState;
   std::vector<int> mFavState;

   int mNumEffects;

   wxTimer mTimer;

   wxPanel *mPanel;
   wxFlexGridSizer *mMainSizer;

   EffectArray mEffects;
   EffectArray mActive;
   bool mBypassing;

   DECLARE_EVENT_TABLE()
};

#endif

#include "EffectInterface.h"
#include "widgets/wxPanelWrapper.h" // to inherit

#include "../SelectedRegion.h"

class AudacityCommand;
class AudacityProject;
class Effect;

class wxCheckBox;

//
class EffectUIHost final : public wxDialogWrapper,
                     public EffectUIHostInterface
{
public:
   // constructors and destructors
   EffectUIHost(wxWindow *parent,
                AudacityProject &project,
                Effect *effect,
                EffectUIClientInterface *client);
   EffectUIHost(wxWindow *parent,
                AudacityProject &project,
                AudacityCommand *command,
                EffectUIClientInterface *client);
   virtual ~EffectUIHost();

   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   int ShowModal() override;

   bool Initialize();

private:
   wxPanel *BuildButtonBar( wxWindow *parent );

   void OnInitDialog(wxInitDialogEvent & evt);
   void OnErase(wxEraseEvent & evt);
   void OnPaint(wxPaintEvent & evt);
   void OnClose(wxCloseEvent & evt);
   void OnApply(wxCommandEvent & evt);
   void DoCancel();
   void OnCancel(wxCommandEvent & evt);
   void OnHelp(wxCommandEvent & evt);
   void OnDebug(wxCommandEvent & evt);
   void OnMenu(wxCommandEvent & evt);
   void OnEnable(wxCommandEvent & evt);
   void OnPlay(wxCommandEvent & evt);
   void OnRewind(wxCommandEvent & evt);
   void OnFFwd(wxCommandEvent & evt);
   void OnPlayback(wxCommandEvent & evt);
   void OnCapture(wxCommandEvent & evt);
   void OnUserPreset(wxCommandEvent & evt);
   void OnFactoryPreset(wxCommandEvent & evt);
   void OnDeletePreset(wxCommandEvent & evt);
   void OnSaveAs(wxCommandEvent & evt);
   void OnImport(wxCommandEvent & evt);
   void OnExport(wxCommandEvent & evt);
   void OnOptions(wxCommandEvent & evt);
   void OnDefaults(wxCommandEvent & evt);

   void UpdateControls();
   wxBitmap CreateBitmap(const char * const xpm[], bool up, bool pusher);
   void LoadUserPresets();

   void InitializeRealtime();
   void CleanupRealtime();
   void Resume();

private:
   AudacityProject *mProject;
   wxWindow *mParent;
   Effect *mEffect;
   AudacityCommand * mCommand;
   EffectUIClientInterface *mClient;

   RegistryPaths mUserPresets;
   bool mInitialized;
   bool mSupportsRealtime;
   bool mIsGUI;
   bool mIsBatch;

   wxButton *mApplyBtn;
   wxButton *mCloseBtn;
   wxButton *mMenuBtn;
   wxButton *mPlayBtn;
   wxButton *mRewindBtn;
   wxButton *mFFwdBtn;
   wxCheckBox *mEnableCb;

   wxButton *mEnableToggleBtn;
   wxButton *mPlayToggleBtn;

   wxBitmap mPlayBM;
   wxBitmap mPlayDisabledBM;
   wxBitmap mStopBM;
   wxBitmap mStopDisabledBM;

   bool mEnabled;

   bool mDisableTransport;
   bool mPlaying;
   bool mCapturing;

   SelectedRegion mRegion;
   double mPlayPos;

   bool mDismissed{};
   bool mNeedsResume{};

   DECLARE_EVENT_TABLE()
};

class CommandContext;

namespace  EffectUI {

   AUDACITY_DLL_API
   wxDialog *DialogFactory( wxWindow &parent, EffectHostInterface *pHost,
      EffectUIClientInterface *client);

   /** Run an effect given the plugin ID */
   // Returns true on success.  Will only operate on tracks that
   // have the "selected" flag set to true, which is consistent with
   // Audacity's standard UI.
   AUDACITY_DLL_API bool DoEffect(
      const PluginID & ID, const CommandContext &context, unsigned flags );

}

class ShuttleGui;

// Obsolescent dialog still used only in Noise Reduction/Removal
class AUDACITY_DLL_API EffectDialog /* not final */ : public wxDialogWrapper
{
public:
   // constructors and destructors
   EffectDialog(wxWindow * parent,
                const TranslatableString & title,
                int type = 0,
                int flags = wxDEFAULT_DIALOG_STYLE,
                int additionalButtons = 0);

   void Init();

   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;
   bool Validate() override;

   // NEW virtuals:
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual void OnPreview(wxCommandEvent & evt);
   virtual void OnOk(wxCommandEvent & evt);

private:
   int mType;
   int mAdditionalButtons;

   DECLARE_EVENT_TABLE()
   wxDECLARE_NO_COPY_CLASS(EffectDialog);
};

#endif // __AUDACITY_EFFECTUI_H__
