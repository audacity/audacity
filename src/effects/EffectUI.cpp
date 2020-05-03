/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectUI.cpp

  Leland Lucius

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/


#include "EffectUI.h"

#include "AllThemeResources.h"
#include "widgets/BasicMenu.h"
#include "ConfigInterface.h"
#include "EffectManager.h"
#include "PluginManager.h"
#include "ProjectHistory.h"
#include "../ProjectWindowBase.h"
#include "../TrackPanelAx.h"
#include "../widgets/BasicMenu.h"
#include "../widgets/wxWidgetsWindowPlacement.h"
#include "RealtimeEffectList.h"
#include "RealtimeEffectManager.h"
#include "RealtimeEffectState.h"
#include "Theme.h"
#include "widgets/wxWidgetsWindowPlacement.h"

static PluginID GetID(EffectPlugin &effect)
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
   EffectPanel(wxWindow *parent)
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
#include "../AudioIO.h"
#include "../CommonCommandFlags.h"
#include "../Menus.h"
#include "../prefs/GUISettings.h" // for RTL_WORKAROUND
#include "Project.h"
#include "../ProjectAudioManager.h"
#include "../ShuttleGui.h"
#include "ViewInfo.h"
#include "../commands/AudacityCommand.h"
#include "../commands/CommandContext.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/HelpSystem.h"

#include <wx/bmpbuttn.h>
#include <wx/checkbox.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>

#if defined(__WXMAC__)
#include <Cocoa/Cocoa.h>
#endif

static const int kMenuID = 20100;
static const int kEnableID = 20101;
static const int kPlayID = 20102;
static const int kPlaybackID = 20105;
static const int kCaptureID = 20106;

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
EVT_IDLE(EffectUIHost::OnIdle)
EVT_CHAR_HOOK(EffectUIHost::OnCharHook)
END_EVENT_TABLE()

namespace {
//! Decorate an EffectSettingsAccess with a `Set` that replicates changes
//! into a second EffectSettingsAccess, while that one still exists
/*! Name inspired by `man 1 tee` */
class EffectSettingsAccessTee : public EffectSettingsAccess {
public:
   EffectSettingsAccessTee(EffectSettingsAccess &main,
      const std::shared_ptr<EffectSettingsAccess> &pSide = {});
   const EffectSettings &Get() override;
   void Set(EffectSettings &&settings,
      std::unique_ptr<Message> pMessage) override;
   void Set(std::unique_ptr<Message> pMessage) override;
   void Flush() override;
   bool IsSameAs(const EffectSettingsAccess &other) const override;
private:
   //! @invariant not null
   const std::shared_ptr<EffectSettingsAccess> mpMain;
   const std::weak_ptr<EffectSettingsAccess> mwSide;
};
}

EffectSettingsAccessTee::EffectSettingsAccessTee(
   EffectSettingsAccess &main,
   const std::shared_ptr<EffectSettingsAccess> &pSide
)  : mpMain{ main.shared_from_this() } //! Guarantee lifetime of main
   , mwSide{ pSide } //! Do not control lifetime of side
{
}

const EffectSettings &EffectSettingsAccessTee::Get() {
   return mpMain->Get();
}

void EffectSettingsAccessTee::Set(EffectSettings &&settings,
   std::unique_ptr<Message> pMessage)
{
   // Move copies of the given settings and message into the side
   if (auto pSide = mwSide.lock())
      pSide->Set(EffectSettings{ settings },
         pMessage ? pMessage->Clone() : nullptr);
   // Move the given settings and message through
   mpMain->Set(std::move(settings), std::move(pMessage));
}

void EffectSettingsAccessTee::Set(std::unique_ptr<Message> pMessage)
{
   // Move copies of the given message into the side
   if (auto pSide = mwSide.lock())
      pSide->Set(pMessage ? pMessage->Clone() : nullptr);
   // Move the given message through
   mpMain->Set(std::move(pMessage));
}

void EffectSettingsAccessTee::Flush()
{
   mpMain->Flush();
   if (auto pSide = mwSide.lock())
      pSide->Flush();
}

bool EffectSettingsAccessTee::IsSameAs(
   const EffectSettingsAccess &other) const
{
   return mpMain->IsSameAs(other);
}

EffectUIHost::EffectUIHost(wxWindow *parent,
   AudacityProject &project, EffectPlugin &effect,
   EffectUIClientInterface &client, std::shared_ptr<EffectInstance> &pInstance,
   EffectSettingsAccess &access,
   const std::shared_ptr<RealtimeEffectState> &pPriorState)
:  wxDialogWrapper(parent, wxID_ANY, effect.GetDefinition().GetName(),
                   wxDefaultPosition, wxDefaultSize,
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMINIMIZE_BOX | wxMAXIMIZE_BOX)
, mEffectUIHost{ effect }
, mClient{ client }
// Grab a pointer to the access object,
// extending its lifetime while this remains:
, mpGivenAccess{ access.shared_from_this() }
, mpAccess{ mpGivenAccess }
, mwState{ pPriorState }
, mProject{ project }
, mParent{ parent }
, mSupportsRealtime{ mEffectUIHost.GetDefinition().SupportsRealtime() }
, mHadPriorState{ (pPriorState != nullptr) }
, mpInstance{ InitializeInstance() }
, mpOutputs{ pPriorState ? pPriorState->GetOutputs() : nullptr }
{
   // Assign the out parameter
   pInstance = mpInstance;
#if defined(__WXMAC__)
   // Make sure the effect window actually floats above the main window
   [ [((NSView *)GetHandle()) window] setLevel:NSFloatingWindowLevel];
#endif
   
   SetName( effect.GetDefinition().GetName() );

   // This style causes Validate() and TransferDataFromWindow() to visit
   // sub-windows recursively, applying any wxValidators
   SetExtraStyle(GetExtraStyle() | wxWS_EX_VALIDATE_RECURSIVELY);
}

EffectUIHost::~EffectUIHost()
{
   if (mpValidator)
      mpValidator->Disconnect();
   DestroyChildren();
   wxASSERT(mClosed);
}

// ============================================================================
// wxWindow implementation
// ============================================================================

bool EffectUIHost::TransferDataToWindow()
{
   // Transfer-to takes const reference to settings
   return mEffectUIHost.TransferDataToWindow(mpAccess->Get()) &&
      //! Do other appearance updates
      mpValidator->UpdateUI() &&
      //! Do validators
      wxDialogWrapper::TransferDataToWindow();
}

bool EffectUIHost::TransferDataFromWindow()
{
   //! Do validations of any wxValidator objects
   if (!wxDialogWrapper::Validate())
      return false;

   //! Do transfers of any wxValidator objects
   if (!wxDialogWrapper::TransferDataFromWindow())
      return false;

   //! Do other custom validation and transfer actions
   if (!mpValidator->ValidateUI())
      return false;
   
   // Transfer-from takes non-const reference to settings
   bool result = true;
   mpAccess->ModifySettings([&](EffectSettings &settings){
      // Allow other transfers, and reassignment of settings
      result = mEffectUIHost.TransferDataFromWindow(settings);
      if (result) {
         auto &definition = mEffectUIHost.GetDefinition();
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
   if (mEffectUIHost.GetDefinition().GetType() == EffectTypeGenerate)
   {
      mApplyBtn->SetLabel(XXO("&Generate").Translation());
   }

   Layout();

   return wxDialogWrapper::ShowModal();
}

// ============================================================================
// EffectUIHost implementation
// ============================================================================

void EffectUIHost::BuildButtonBar(ShuttleGui &S, bool graphicalUI)
{
   mIsGUI = graphicalUI;
   mIsBatch = mEffectUIHost.IsBatchProcessing();

   constexpr int margin = 3;

   S.StartPanel();
   {
      S.SetBorder( margin );

      S.StartHorizontalLay(wxEXPAND, 0);
      {
         if (IsOpenedFromEffectPanel())
         {
            mEnableBtn = S.Id(kEnableID)
               .Position(wxALIGN_CENTER | wxTOP | wxBOTTOM)
               .Name(XO("Enable"))
               .AddBitmapButton(mEnabled ? mRealtimeEnabledBM : mRealtimeDisabledBM);
         }

         mMenuBtn = S.Id( kMenuID )
            .ToolTip(XO("Manage presets and options"))
            .AddButton( XO("Presets && settings"), wxALIGN_CENTER | wxTOP | wxBOTTOM );

         S.AddSpace(1, 0, 1);

         if (!mIsBatch)
         {
            if (mSupportsRealtime)
            {
               if (mpTempProjectState)
               {
                  mPlayToggleBtn = S.Id(kPlayID)
                     .ToolTip(XO("Start and stop preview"))
                     .AddButton( { },
                                 wxALIGN_CENTER | wxTOP | wxBOTTOM );
               }
            }
            else if (
               (mEffectUIHost.GetDefinition().GetType() != EffectTypeAnalyze) &&
               (mEffectUIHost.GetDefinition().GetType() != EffectTypeTool) )
            {
               mPlayToggleBtn = S.Id(kPlayID)
                  .ToolTip(XO("Preview effect"))
                  .AddButton( { },
                              wxALIGN_CENTER | wxTOP | wxBOTTOM );
            }
            if(mPlayToggleBtn != nullptr)
            {
               //wxButton does not implement GetSizeFromText
               //set button minimum size so that largest text fits
               mPlayToggleBtn->SetLabel(_("Stop &Preview"));
               auto a = mPlayToggleBtn->GetBestSize();
               mPlayToggleBtn->SetLabel(_("&Preview"));
               auto b = mPlayToggleBtn->GetBestSize();
               mPlayToggleBtn->SetMinSize(a.x > b.x ? a : b);
            }
         }

         if (!IsOpenedFromEffectPanel())
         {
            mApplyBtn = S.Id(wxID_APPLY)
               .AddButton( XXO("&Apply"),
                           wxALIGN_CENTER | wxTOP | wxBOTTOM );
            mApplyBtn->SetDefault();
         }

         if (mEffectUIHost.GetDefinition().EnablesDebug())
         {
            mDebugBtn = S.Id(eDebugID)
               .AddButton( XXO("Debu&g"),
                           wxALIGN_CENTER | wxTOP | wxBOTTOM );
         }
      }
      S.EndHorizontalLay();
   }
   S.EndPanel();
}

bool EffectUIHost::Initialize()
{
   mEnabled = mpAccess->Get().extra.GetActive();

   mRealtimeEnabledBM = theTheme.Bitmap(bmpEffectOn);
   mRealtimeDisabledBM = theTheme.Bitmap(bmpEffectOff);

   // Build a "host" dialog, framing a panel that the client fills in.
   // The frame includes buttons to preview, apply, load and save presets, etc.
   EffectPanel *w {};
   ShuttleGui S{ this, eIsCreating };
   {
      // Make the panel for the client
      Destroy_ptr<EffectPanel> uw{ safenew EffectPanel( S.GetParent() ) };
      RTL_WORKAROUND(uw.get());

      // Try to give the window a sensible default/minimum size
      uw->SetMinSize(wxSize(wxMax(600, mParent->GetSize().GetWidth() * 2 / 3),
         mParent->GetSize().GetHeight() / 2));

      // Let the client add things to the panel
      ShuttleGui S1{ uw.get(), eIsCreating };
      mpValidator = mClient.PopulateUI(S1, *mpInstance, *mpAccess, mpOutputs);
      if (!mpValidator)
         return false;

      BuildButtonBar(S, mpValidator->IsGraphicalUI());

      S.StartHorizontalLay( wxEXPAND );
      {
         S.Prop( 1 )
            .Position(wxEXPAND)
            .AddWindow((w = uw.release()));
      }
      S.EndHorizontalLay();
   }

   Layout();
   Fit();
   Center();

   UpdateControls();

   w->SetAccept(!mIsGUI);

   if (!mIsGUI)
   {
      w->SetFocus();
   }
   else if (!IsOpenedFromEffectPanel())
   {
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


void EffectUIHost::OnInitDialog(wxInitDialogEvent & evt)
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
   wxTextCtrl *focused = wxDynamicCast(FindFocus(), wxTextCtrl);
   if (focused)
   {
      focused->SelectAll();
   }
}

void EffectUIHost::OnErase(wxEraseEvent & WXUNUSED(evt))
{
   // Ignore it
}

void EffectUIHost::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   wxPaintDC dc(this);
   
   dc.Clear();
}

void EffectUIHost::OnClose(wxCloseEvent & WXUNUSED(evt))
{
   if (mPlaying)
      StopPlayback();

   DoCancel();
   CleanupRealtime();

   if (mpValidator)
      mpValidator->OnClose();
   
   Hide();
   Destroy();
   
#if wxDEBUG_LEVEL
   mClosed = true;
#endif
}

void EffectUIHost::OnApply(wxCommandEvent & evt)
{
   auto &project = mProject;

   // On wxGTK (wx2.8.12), the default action is still executed even if
   // the button is disabled.  This appears to affect all wxDialogs, not
   // just our Effects dialogs.  So, this is a only temporary workaround
   // for legacy effects that disable the OK button.  Hopefully this has
   // been corrected in wx3.
   if (!mApplyBtn->IsEnabled())
   {
      return;
   }

   if (mPlaying)
      StopPlayback();
   
   // Honor the "select all if none" preference...a little hackish, but whatcha gonna do...
   if (!mIsBatch &&
       mEffectUIHost.GetDefinition().GetType() != EffectTypeGenerate &&
       mEffectUIHost.GetDefinition().GetType() != EffectTypeTool &&
       ViewInfo::Get( project ).selectedRegion.isPoint())
   {
      auto flags = AlwaysEnabledFlag;
      bool allowed =
      MenuManager::Get( project ).ReportIfActionNotAllowed(
         mEffectUIHost.GetDefinition().GetName(),
         flags,
         WaveTracksSelectedFlag() | TimeSelectedFlag());
      if (!allowed)
         return;
   }
   
   if (!TransferDataFromWindow() ||
       // This is the main place where there is a side-effect on the config
       // file to remember the last-used settings of an effect, just before
       // applying the effect destructively.
       !mEffectUIHost.GetDefinition()
         .SaveUserPreset(CurrentSettingsGroup(), mpAccess->Get()))
      return;

   if (IsModal())
   {
      mDismissed = true;
      
      EndModal(evt.GetId());
      
      Close();
      
      return;
   }
   
   // Progress dialog no longer yields, so this "shouldn't" be necessary (yet to be proven
   // for sure), but it is a nice visual cue that something is going on.
   mApplyBtn->Disable();
   auto cleanup = finally( [&] { mApplyBtn->Enable(); } );

   CommandContext context( project );
   // This is absolute hackage...but easy and I can't think of another way just now.
   //
   // It should callback to the EffectManager to kick off the processing
   EffectUI::DoEffect(GetID(mEffectUIHost), context,
      EffectManager::kConfigured);
}

void EffectUIHost::DoCancel()
{
   if (!mDismissed) {
      if (!mHadPriorState) {
         // For the destructive effect dialog only
         // Restore effect state from last updated preferences
         mpAccess->ModifySettings([&](EffectSettings &settings) {
            // ignore failure
            return mEffectUIHost.GetDefinition().LoadUserPreset(
               CurrentSettingsGroup(), settings).value_or(nullptr);
         });
      }
      if (IsModal())
         EndModal(0);
      else
         Hide();
      
      mDismissed = true;
   }
}

void EffectUIHost::OnCancel(wxCommandEvent & WXUNUSED(evt))
{
   DoCancel();
   Close();
}

void EffectUIHost::OnDebug(wxCommandEvent & evt)
{
   OnApply(evt);
}

namespace {
wxString GetVersionForDisplay(const EffectDefinitionInterface &definition)
{
   static const auto specialVersion = XO("n/a");
   auto result = definition.GetVersion();
   if (result == specialVersion.MSGID())
      result = specialVersion.Translation();
   return result;
}
}

void EffectUIHost::OnMenu(wxCommandEvent & WXUNUSED(evt))
{
   BasicMenu::Handle menu{ BasicMenu::FreshMenu };
   LoadUserPresets();
   
   if (mUserPresets.size() == 0)
      menu.Append(XXO("User Presets"), {}, { false }); // disabled
   else {
      BasicMenu::Handle sub{ BasicMenu::FreshMenu };
      for (size_t i = 0, cnt = mUserPresets.size(); i < cnt; i++)
         sub.Append(Verbatim(mUserPresets[i]),
            [this, i]{ OnUserPreset(i); });
      menu.AppendSubMenu(std::move(sub), XXO("User Presets"));
   }
   
   menu.Append(XXO("Save Preset..."), [this]{ OnSaveAs(); }, {});
   
   if (mUserPresets.size() == 0)
      menu.Append(XXO("Delete Preset"), {}, { false }); // disabled
   else {
      BasicMenu::Handle sub{ BasicMenu::FreshMenu };
      for (size_t i = 0, cnt = mUserPresets.size(); i < cnt; i++)
         sub.Append( Verbatim( mUserPresets[i] ),
            [this, i]{ OnDeletePreset(i); } );
      menu.AppendSubMenu( std::move( sub ), XXO("Delete Preset") );
   }
   
   menu.AppendSeparator();
   
   auto factory = mEffectUIHost.GetDefinition().GetFactoryPresets();
   
   {
      BasicMenu::Handle sub{ BasicMenu::FreshMenu };
      sub.Append( XXO("Defaults"), [this]{ OnDefaults(); } );
      if (factory.size() > 0)
      {
         sub.AppendSeparator();
         for (size_t i = 0, cnt = factory.size(); i < cnt; i++)
         {
            auto label = Verbatim( factory[i] );
            if (label.empty())
            {
               label = XXO("None");
            }
            
            sub.Append( label, [this, i]{ OnFactoryPreset(i); } );
         }
      }
      menu.AppendSubMenu( std::move( sub ), XXO("Factory Presets") );
   }
   
   menu.AppendSeparator();
   menu.Append(
      XXO("Import..."), [this]{ OnImport(); },
         { mClient.CanExportPresets() } );
   menu.Append(
      XXO("Export..."), [this]{ OnExport(); },
         { mClient.CanExportPresets() } );
   menu.AppendSeparator();
   menu.Append(
      XXO("Options..."), [this]{ OnOptions(); },
         { mClient.HasOptions() } );
   menu.AppendSeparator();
   
   {
      BasicMenu::Handle sub{ BasicMenu::FreshMenu };
      
      auto &definition = mEffectUIHost.GetDefinition();
      sub.Append(XXO("Type: %s")
         .Format( definition.GetFamily().Translation() ) );
      sub.Append(XXO("Name: %s")
         .Format( definition.GetName().Translation() ) );
      sub.Append(XXO("Version: %s")
         .Format( GetVersionForDisplay(definition) ) );
      sub.Append(XXO("Vendor: %s")
         .Format( definition.GetVendor().Translation() ) );
      sub.Append(XXO("Description: %s")
         .Format( definition.GetDescription().Translation() ) );
      
      menu.AppendSubMenu( std::move( sub ), XXO("About") );
   }
   
   wxWindow *btn = FindWindow(kMenuID);
   wxRect r = btn->GetRect();
   menu.Popup(
      wxWidgetsWindowPlacement{ btn },
      { r.GetLeft(), r.GetBottom() }
   );
}

void EffectUIHost::OnEnable(wxCommandEvent & WXUNUSED(evt))
{
   mEnabled = !mEnabled;

   auto mpState = mwState.lock();
   if (mpState)
      mpState->SetActive(mEnabled);

   UpdateControls();
}

void EffectUIHost::OnPlay(wxCommandEvent & WXUNUSED(evt))
{
   if (!mSupportsRealtime)
   {
      if (!TransferDataFromWindow())
         return;
      
      mEffectUIHost.Preview(*mpAccess, false);
      
      return;
   }
   
   if (mPlaying)
   {
      StopPlayback();
   }
   else
   {
      auto &viewInfo = ViewInfo::Get( mProject );
      const auto &selectedRegion = viewInfo.selectedRegion;
      const auto &playRegion = viewInfo.playRegion;
      if ( playRegion.Active() )
      {
         mRegion.setTimes(playRegion.GetStart(), playRegion.GetEnd());
         mPlayPos = mRegion.t0();
      }
      else if (selectedRegion.t0() != mRegion.t0() ||
               selectedRegion.t1() != mRegion.t1())
      {
         mRegion = selectedRegion;
         mPlayPos = mRegion.t0();
      }
      
      if (mPlayPos > mRegion.t1())
      {
         mPlayPos = mRegion.t1();
      }
      
      auto &projectAudioManager = ProjectAudioManager::Get( mProject );
      projectAudioManager.PlayPlayRegion(
                                         SelectedRegion(mPlayPos, mRegion.t1()),
                                         DefaultPlayOptions( mProject ),
                                         PlayMode::normalPlay );
   }
}

void EffectUIHost::OnPlayback(AudioIOEvent evt)
{
   if (evt.on) {
      if (evt.pProject != &mProject)
         mDisableTransport = true;
      else
         mPlaying = true;
   }
   else {
      mDisableTransport = false;
      mPlaying = false;
   }
   
   if (mPlaying) {
      mRegion = ViewInfo::Get( mProject ).selectedRegion;
      mPlayPos = mRegion.t0();
   }
   UpdateControls();
}

void EffectUIHost::OnCapture(AudioIOEvent evt)
{
   if (evt.on) {
      if (evt.pProject != &mProject)
         mDisableTransport = true;
      else
         mCapturing = true;
   }
   else {
      mDisableTransport = false;
      mCapturing = false;
   }
   UpdateControls();
}

void EffectUIHost::OnUserPreset(size_t preset)
{
   mpAccess->ModifySettings([&](EffectSettings &settings){
      // ignore failure
      return mEffectUIHost.GetDefinition().LoadUserPreset(
         UserPresetsGroup(mUserPresets[preset]), settings).value_or(nullptr);
   });
   TransferDataToWindow();
}

void EffectUIHost::OnFactoryPreset(size_t preset)
{
   mpAccess->ModifySettings([&](EffectSettings &settings){
      //! ignore failure
      return mEffectUIHost.GetDefinition().LoadFactoryPreset(
         preset, settings).value_or(nullptr);
   });
   TransferDataToWindow();
}

void EffectUIHost::OnDeletePreset(size_t ii)
{
   auto preset = mUserPresets[ii];
   
   int res = AudacityMessageBox(
                                XO("Are you sure you want to delete \"%s\"?").Format( preset ),
                                XO("Delete Preset"),
                                wxICON_QUESTION | wxYES_NO);
   if (res == wxYES)
   {
      RemoveConfigSubgroup(mEffectUIHost.GetDefinition(),
         PluginSettings::Private, UserPresetsGroup(preset));
   }
   
   LoadUserPresets();
   
   return;
}

void EffectUIHost::OnSaveAs()
{
   wxTextCtrl *text;
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
      
      if (rc != wxID_OK)
      {
         break;
      }
      
      name = text->GetValue();
      if (name.empty())
      {
         AudacityMessageDialog md(
                                  this,
                                  XO("You must specify a name"),
                                  XO("Save Preset") );
         md.Center();
         md.ShowModal();
         continue;
      }
      
      if ( make_iterator_range( mUserPresets ).contains( name ) )
      {
         AudacityMessageDialog md(
                                  this,
                                  XO("Preset already exists.\n\nReplace?"),
                                  XO("Save Preset"),
                                  wxYES_NO | wxCANCEL | wxICON_EXCLAMATION );
         md.Center();
         int choice = md.ShowModal();
         if (choice == wxID_CANCEL)
         {
            break;
         }
         
         if (choice == wxID_NO)
         {
            continue;
         }
      }
      
      if (TransferDataFromWindow())
         mEffectUIHost.GetDefinition()
            .SaveUserPreset(UserPresetsGroup(name), mpAccess->Get());
      LoadUserPresets();
      
      break;
   }
   
   return;
}

void EffectUIHost::OnImport()
{
   mpAccess->ModifySettings([&](EffectSettings &settings){
      // ignore failure
      return mClient.ImportPresets(settings).value_or(nullptr);
   });
   TransferDataToWindow();
   LoadUserPresets();

   return;
}

void EffectUIHost::OnExport()
{
   // may throw
   // exceptions are handled in AudacityApp::OnExceptionInMainLoop
   if (TransferDataFromWindow())
     mClient.ExportPresets(mpAccess->Get());
   
   return;
}

void EffectUIHost::OnOptions()
{
   mClient.ShowOptions();
   
   return;
}

void EffectUIHost::OnDefaults()
{
   mpAccess->ModifySettings([&](EffectSettings &settings){
      // ignore failure
      return mEffectUIHost.GetDefinition().LoadFactoryDefaults(settings)
         .value_or(nullptr);
   });
   TransferDataToWindow();
   return;
}

void EffectUIHost::OnIdle(wxIdleEvent &evt)
{
   evt.Skip();
   if (mpAccess)
      mpAccess->Flush();
}

void EffectUIHost::OnCharHook(wxKeyEvent& evt)
{
   if (!IsEscapeKey(evt))
   {
      evt.Skip();
      return;
   }
   
   if (IsOpenedFromEffectPanel())
      Close();
   else
   {
      wxCommandEvent cancelEvt { wxEVT_COMMAND_BUTTON_CLICKED, wxID_CANCEL };

      OnCancel(cancelEvt);
   }
}

bool EffectUIHost::IsOpenedFromEffectPanel() const
{
   return (mpTempProjectState == nullptr && mSupportsRealtime);
}

wxBitmap EffectUIHost::CreateBitmap(const char * const xpm[], bool up, bool pusher)
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
   if (pusher)
   {
      if (!up)
      {
         offset += 1;
      }
   }
   
   dc.DrawBitmap(pic, offset, offset, true);
   
   dc.SelectObject(wxNullBitmap);
   
   return mod;
}

void EffectUIHost::UpdateControls()
{
   if (mIsBatch)
   {
      return;
   }

   if (IsOpenedFromEffectPanel())
   {
      mEnableBtn->SetBitmapLabel(mEnabled ? mRealtimeEnabledBM : mRealtimeDisabledBM);
      return;
   }

   mApplyBtn->Enable(!mCapturing);

   if (mSupportsRealtime)
   {
      mPlayToggleBtn->Enable(!(mCapturing || mDisableTransport));

      if (mPlaying)
      {
         /* i18n-hint: The access key "&P" should be the same in
          "Stop &Preview" and "Start &Preview" */
         mPlayToggleBtn->SetLabel(_("Stop &Preview"));
         mPlayToggleBtn->Refresh();
      }
      else
      {
         /* i18n-hint: The access key "&P" should be the same in
          "Stop &Preview" and "Start &Preview" */
         mPlayToggleBtn->SetLabel(_("&Preview"));
         mPlayToggleBtn->Refresh();
      }
   }
}

void EffectUIHost::LoadUserPresets()
{
   mUserPresets.clear();
   
   GetConfigSubgroups(mEffectUIHost.GetDefinition(),
      PluginSettings::Private, UserPresetsGroup(wxEmptyString), mUserPresets);
   
   std::sort( mUserPresets.begin(), mUserPresets.end() );
   
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
      mDisableTransport = !gAudioIO->IsAvailable(mProject);
      mPlaying = gAudioIO->IsStreamActive(); // not exactly right, but will suffice
      mCapturing = gAudioIO->IsStreamActive() && gAudioIO->GetNumCaptureChannels() > 0 && !gAudioIO->IsMonitoring();
   }

   if (mSupportsRealtime && !mInitialized) {
      if (!priorState)
         mwState = mpState = mpTempProjectState =
            AudioIO::Get()->AddState(mProject, nullptr, GetID(mEffectUIHost));
      if (mpState) {
         // Find the right instance to connect to the dialog
         if (!result) {
            result = mpState->GetInstance();
            if (result && !result->Init())
               result.reset();
         }

         mpAccess2 = mpState->GetAccess();
         if (!(mpAccess2->IsSameAs(*mpAccess)))
            // Decorate the given access object
            mpAccess = std::make_shared<EffectSettingsAccessTee>(
               *mpAccess, mpAccess2);

         mEffectStateSubscription = mpState->Subscribe([this](RealtimeEffectStateChange state) {
            mEnabled = (state == RealtimeEffectStateChange::EffectOn);
            mEnableBtn->SetBitmapLabel(mEnabled ? mRealtimeEnabledBM : mRealtimeDisabledBM);

            UpdateControls();
         });
      }

      if (!priorState) {
         mAudioIOSubscription = AudioIO::Get()->Subscribe([this](AudioIOEvent event){
            switch (event.type) {
            case AudioIOEvent::PLAYBACK:
               OnPlayback(event); break;
            case AudioIOEvent::CAPTURE:
               OnCapture(event); break;
            default:
               break;
            }
         });
      }
      
      mInitialized = true;
   }
   else {
      result = mEffectUIHost.MakeInstance();
      if (result && !result->Init())
         result.reset();
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

void EffectUIHost::StopPlayback()
{
   if (!mPlaying)
      return;
   
   auto gAudioIO = AudioIO::Get();
   mPlayPos = gAudioIO->GetStreamTime();
   auto& projectAudioManager = ProjectAudioManager::Get(mProject);
   projectAudioManager.Stop();
}

DialogFactoryResults EffectUI::DialogFactory(wxWindow &parent,
   EffectPlugin &host, EffectUIClientInterface &client,
   EffectSettingsAccess &access)
{
   // Make sure there is an associated project, whose lifetime will
   // govern the lifetime of the dialog, even when the dialog is
   // non-modal, as for realtime effects
   auto project = FindProjectFromWindow(&parent);
   if ( !project )
      return {};
   std::shared_ptr<EffectInstance> pInstance;
   Destroy_ptr<EffectUIHost> dlg{ safenew EffectUIHost{ &parent,
      *project, host, client, pInstance, access } };
   if (!pInstance) {
      dlg->SetClosed();
      return {};
   }
   if (dlg->Initialize()) {
      auto pValidator = dlg->GetValidator();
      // release() is safe because parent will own it
      return { dlg.release(), pInstance, pValidator };
   }
   return {};
}

#include "PluginManager.h"
#include "ProjectRate.h"
#include "../ProjectWindow.h"
#include "../SelectUtilities.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../commands/CommandManager.h"

/// DoEffect() takes a PluginID and executes the associated effect.
///
/// At the moment flags are used only to indicate whether to prompt for
//  parameters, whether to save the state to history and whether to allow
/// 'Repeat Last Effect'.

/* static */ bool EffectUI::DoEffect(
   const PluginID & ID, const CommandContext &context, unsigned flags )
{
   AudacityProject &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &trackFactory = WaveTrackFactory::Get( project );
   auto rate = ProjectRate::Get(project).GetRate();
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &commandManager = CommandManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
   if (!plug)
      return false;

   EffectType type = plug->GetEffectType();

   // Make sure there's no activity since the effect is about to be applied
   // to the project's tracks.  Mainly for Apply during RTP, but also used
   // for batch commands
   if (flags & EffectManager::kConfigured)
   {
      ProjectAudioManager::Get( project ).Stop();
      //Don't Select All if repeating Generator Effect
      if (!(flags & EffectManager::kConfigured)) {
         SelectUtilities::SelectAllIfNone(project);
      }
   }

   auto nTracksOriginally = tracks.size();
   wxWindow *focus = wxWindow::FindFocus();
   wxWindow *parent = nullptr;
   if (focus != nullptr) {
      parent = focus->GetParent();
   }

   bool success = false;
   auto cleanup = finally( [&] {

      if (!success) {
         // For now, we're limiting realtime preview to a single effect, so
         // make sure the menus reflect that fact that one may have just been
         // opened.
         MenuManager::Get(project).UpdateMenus( false );
      }

   } );

   int count = 0;
   bool clean = true;
   for (auto t : tracks.Selected< const WaveTrack >()) {
      if (t->GetEndTime() != 0.0)
         clean = false;
      count++;
   }

   EffectManager & em = EffectManager::Get();

   em.SetSkipStateFlag( false );
   success = false;
   if (auto effect = em.GetEffect(ID)) {
      if (const auto pSettings = em.GetDefaultSettings(ID)) {
         const auto pAccess =
            std::make_shared<SimpleEffectSettingsAccess>(*pSettings);
         pAccess->ModifySettings([&](EffectSettings &settings){
            success = effect->DoEffect(settings,
               rate,
               &tracks,
               &trackFactory,
               selectedRegion,
               flags,
               &window,
               (flags & EffectManager::kConfigured) == 0
                  ? DialogFactory
                  : nullptr,
               pAccess);
            return nullptr;
         });
      }
   }

   if (!success)
      return false;

   if (em.GetSkipStateFlag())
      flags = flags | EffectManager::kSkipState;

   if (!(flags & EffectManager::kSkipState))
   {
      auto shortDesc = em.GetCommandName(ID);
      auto longDesc = em.GetCommandDescription(ID);
      ProjectHistory::Get( project ).PushState(longDesc, shortDesc);
   }

   if (!(flags & EffectManager::kDontRepeatLast))
   {
      // Remember a successful generator, effect, analyzer, or tool Process
         auto shortDesc = em.GetCommandName(ID);
         /* i18n-hint: %s will be the name of the effect which will be
          * repeated if this menu item is chosen */
         auto lastEffectDesc = XO("Repeat %s").Format(shortDesc);
         auto& menuManager = MenuManager::Get(project);
         switch ( type ) {
         case EffectTypeGenerate:
            commandManager.Modify(wxT("RepeatLastGenerator"), lastEffectDesc);
            menuManager.mLastGenerator = ID;
            menuManager.mRepeatGeneratorFlags = EffectManager::kConfigured;
            break;
         case EffectTypeProcess:
            commandManager.Modify(wxT("RepeatLastEffect"), lastEffectDesc);
            menuManager.mLastEffect = ID;
            menuManager.mRepeatEffectFlags = EffectManager::kConfigured;
            break;
         case EffectTypeAnalyze:
            commandManager.Modify(wxT("RepeatLastAnalyzer"), lastEffectDesc);
            menuManager.mLastAnalyzer = ID;
            menuManager.mLastAnalyzerRegistration = MenuCreator::repeattypeplugin;
            menuManager.mRepeatAnalyzerFlags = EffectManager::kConfigured;
            break;
         case EffectTypeTool:
            commandManager.Modify(wxT("RepeatLastTool"), lastEffectDesc);
            menuManager.mLastTool = ID;
            menuManager.mLastToolRegistration = MenuCreator::repeattypeplugin;
            menuManager.mRepeatToolFlags = EffectManager::kConfigured;
            if (shortDesc == NYQUIST_PROMPT_NAME) {
               menuManager.mRepeatToolFlags = EffectManager::kRepeatNyquistPrompt;  //Nyquist Prompt is not configured
            }
            break;
      }
   }

   //STM:
   //The following automatically re-zooms after sound was generated.
   // IMO, it was disorienting, removing to try out without re-fitting
   //mchinen:12/14/08 reapplying for generate effects
   if (type == EffectTypeGenerate)
   {
      if (count == 0 || (clean && selectedRegion.t0() == 0.0))
         window.DoZoomFit();
         //  trackPanel->Refresh(false);
   }

   // PRL:  RedrawProject explicitly because sometimes history push is skipped
   window.RedrawProject();

   if (focus != nullptr && focus->GetParent()==parent) {
      focus->SetFocus();
   }

   // A fix for Bug 63
   // New tracks added?  Scroll them into view so that user sees them.
   // Don't care what track type.  An analyser might just have added a
   // Label track and we want to see it.
   if( tracks.size() > nTracksOriginally ){
      // 0.0 is min scroll position, 1.0 is max scroll position.
      trackPanel.VerticalScroll( 1.0 );
   }
   else {
      auto pTrack = *tracks.Selected().begin();
      if (!pTrack)
         pTrack = *tracks.Any().begin();
      if (pTrack) {
         TrackFocus::Get(project).Set(pTrack);
         pTrack->EnsureVisible();
      }
   }

   return true;
}

///////////////////////////////////////////////////////////////////////////////
BEGIN_EVENT_TABLE(EffectDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, EffectDialog::OnOk)
END_EVENT_TABLE()

EffectDialog::EffectDialog(wxWindow * parent,
                           const TranslatableString & title,
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
   if ((mType != EffectTypeAnalyze) && (mType != EffectTypeTool))
   {
      buttons |= eCancelButton;
      if (mType == EffectTypeProcess)
      {
         buttons |= ePreviewButton;
      }
   }

   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      PopulateOrExchange(S);
      S.AddStandardButtons(buttons|mAdditionalButtons);
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
void EffectDialog::PopulateOrExchange(ShuttleGui & WXUNUSED(S))
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

void EffectDialog::OnPreview(wxCommandEvent & WXUNUSED(evt))
{
   return;
}

void EffectDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   // On wxGTK (wx2.8.12), the default action is still executed even if
   // the button is disabled.  This appears to affect all wxDialogs, not
   // just our Effects dialogs.  So, this is a only temporary workaround
   // for legacy effects that disable the OK button.  Hopefully this has
   // been corrected in wx3.
   if (FindWindow(wxID_OK)->IsEnabled() && Validate() && TransferDataFromWindow())
   {
      EndModal(wxID_OK);
   }

   return;
}

//! Inject a factory for realtime effects
#include "RealtimeEffectState.h"
static RealtimeEffectState::EffectFactory::Scope
scope{ &EffectManager::GetInstanceFactory };

/* The following registration objects need a home at a higher level to avoid
 dependency either way between WaveTrack or RealtimeEffectList, which need to
 be in different libraries that do not depend either on the other.

 WaveTrack, like AudacityProject, has a registry for attachment of serializable
 data.  RealtimeEffectList exposes an interface for serialization.  This is
 where we connect them.
 */
#include "RealtimeEffectList.h"
static ProjectFileIORegistry::ObjectReaderEntry projectAccessor {
   RealtimeEffectList::XMLTag(),
   [](AudacityProject &project) { return &RealtimeEffectList::Get(project); }
};

static ProjectFileIORegistry::ObjectWriterEntry projectWriter {
[](const AudacityProject &project, XMLWriter &xmlFile){
   RealtimeEffectList::Get(project).WriteXML(xmlFile);
} };

static WaveTrackIORegistry::ObjectReaderEntry waveTrackAccessor {
   RealtimeEffectList::XMLTag(),
   [](WaveTrack &track) { return &RealtimeEffectList::Get(track); }
};

static WaveTrackIORegistry::ObjectWriterEntry waveTrackWriter {
[](const WaveTrack &track, auto &xmlFile) {
   if (track.IsLeader())
      RealtimeEffectList::Get(track).WriteXML(xmlFile);
} };
