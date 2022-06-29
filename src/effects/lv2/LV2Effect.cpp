/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/



#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "LV2Effect.h"
#include "LV2EffectMeter.h"
#include "LV2Wrapper.h"
#include "SampleCount.h"

#include <cmath>
#include <exception>
#include <functional>

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/log.h>
#include <wx/msgqueue.h>

#ifdef __WXMAC__
#include <wx/evtloop.h>
#endif

#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/tokenzr.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>

#include "AudacityException.h"
#include "ConfigInterface.h"
#include "../../widgets/valnum.h"
#include "../../widgets/AudacityMessageBox.h"
#include "../../widgets/NumericTextCtrl.h"

#include "lv2/instance-access/instance-access.h"

#if defined(__WXGTK__)
#include <gtk/gtk.h>
#endif

#if defined(__WXMSW__)
#include <wx/msw/wrapwin.h>
#endif

LV2Instance::LV2Instance(
   StatefulPerTrackEffect &effect, const LV2Ports &ports
)  : PerTrackEffect::Instance{ effect }
   , mPorts{ ports }
{
   LV2Preferences::GetUseLatency(effect, mUseLatency);

   int userBlockSize;
   LV2Preferences::GetBufferSize(effect, userBlockSize);
   mUserBlockSize = std::max(1, userBlockSize);
   mBlockSize = mUserBlockSize;
   lv2_atom_forge_init(&mForge, GetEffect().URIDMapFeature());
}

LV2Instance::~LV2Instance()
{
   // Some temporary ugliness until real statelessness
   const_cast<LV2Effect&>(static_cast<const LV2Effect&>(mProcessor))
      .mMaster.reset();
}

void LV2Instance::MakeWrapper(const EffectSettings &settings,
   double projectRate, bool useOutput)
{
   auto &effect = GetEffect();
   auto &pWrapper = effect.mMaster;
   if (pWrapper)
      // Already made so do nothing
      return;
   pWrapper = LV2Wrapper::Create(effect, mPorts,
      // TODO give instances independent port states
      effect.mPortStates,
      GetSettings(settings), projectRate, useOutput);
}

///////////////////////////////////////////////////////////////////////////////
//
// LV2Effect
//
///////////////////////////////////////////////////////////////////////////////

enum
{
   ID_Duration = 10000,
   ID_Triggers = 11000,
   ID_Toggles = 12000,
   ID_Sliders = 13000,
   ID_Choices = 14000,
   ID_Texts = 15000,
   ID_TIMER = 20000,
};

BEGIN_EVENT_TABLE(LV2Effect, wxEvtHandler)
   EVT_COMMAND_RANGE(ID_Triggers, ID_Triggers + 999, wxEVT_COMMAND_BUTTON_CLICKED, LV2Effect::OnTrigger)
   EVT_COMMAND_RANGE(ID_Toggles, ID_Toggles + 999, wxEVT_COMMAND_CHECKBOX_CLICKED, LV2Effect::OnToggle)
   EVT_COMMAND_RANGE(ID_Sliders, ID_Sliders + 999, wxEVT_COMMAND_SLIDER_UPDATED, LV2Effect::OnSlider)
   EVT_COMMAND_RANGE(ID_Choices, ID_Choices + 999, wxEVT_COMMAND_CHOICE_SELECTED, LV2Effect::OnChoice)
   EVT_COMMAND_RANGE(ID_Texts, ID_Texts + 999, wxEVT_COMMAND_TEXT_UPDATED, LV2Effect::OnText)

   EVT_TIMER(ID_TIMER, LV2Effect::OnTimer)
   EVT_IDLE(LV2Effect::OnIdle)
END_EVENT_TABLE()

LV2Effect::LV2Effect(const LilvPlugin &plug) : LV2FeaturesList{ plug }
{
}

LV2Effect::~LV2Effect()
{
}

// ============================================================================
// ComponentInterface Implementation
// ============================================================================

PluginPath LV2Effect::GetPath() const
{
   return LilvString(lilv_plugin_get_uri(&mPlug));
}

ComponentInterfaceSymbol LV2Effect::GetSymbol() const
{
   return LilvStringMove(lilv_plugin_get_name(&mPlug));
}

VendorSymbol LV2Effect::GetVendor() const
{
   wxString vendor = LilvStringMove(lilv_plugin_get_author_name(&mPlug));

   if (vendor.empty())
   {
      return XO("n/a");
   }

   return {vendor};
}

wxString LV2Effect::GetVersion() const
{
   return wxT("1.0");
}

TranslatableString LV2Effect::GetDescription() const
{
   return XO("n/a");
}

// ============================================================================
// EffectDefinitionInterface Implementation
// ============================================================================

EffectType LV2Effect::GetType() const
{
   if (GetAudioInCount() == 0 && GetAudioOutCount() == 0)
   {
      return EffectTypeTool;
   }

   if (GetAudioInCount() == 0)
   {
      return EffectTypeGenerate;
   }

   if (GetAudioOutCount() == 0)
   {
      return EffectTypeAnalyze;
   }

   return EffectTypeProcess;
}

EffectFamilySymbol LV2Effect::GetFamily() const
{
   return LV2EFFECTS_FAMILY;
}

bool LV2Effect::IsInteractive() const
{
   return mPorts.mControlPorts.size() != 0;
}

bool LV2Effect::IsDefault() const
{
   return false;
}

auto LV2Effect::RealtimeSupport() const -> RealtimeSince
{
   // TODO reenable after achieving statelessness
   return RealtimeSince::Never;
//   return GetType() == EffectTypeProcess
//      ? RealtimeSince::Always
//      : RealtimeSince::Never;
}

bool LV2Effect::SupportsAutomation() const
{
   return true;
}

bool LV2Effect::InitializePlugin()
{
   // To be set up later when making a dialog:
   mExtensionDataFeature = {};

   if (!LV2FeaturesList::InitializeOptions() ||
      !LV2FeaturesList::InitializeFeatures()
   )
      return false;

   AddFeature(LV2_UI__resize, &mUIResizeFeature);
   AddFeature(LV2_DATA_ACCESS_URI, &mExtensionDataFeature);
   AddFeature(LV2_EXTERNAL_UI__Host, &mExternalUIHost);
   AddFeature(LV2_EXTERNAL_UI_DEPRECATED_URI, &mExternalUIHost);
   // Two features must be filled in later
   mInstanceAccessFeature = mFeatures.size();
   AddFeature(LV2_INSTANCE_ACCESS_URI, nullptr);
   mParentFeature = mFeatures.size();
   if (!ValidateFeatures(lilv_plugin_get_uri(&mPlug)))
      return false;

   // Determine available extensions
   mWantsOptionsInterface = false;
   mWantsStateInterface = false;
   if (LilvNodesPtr extdata{ lilv_plugin_get_extension_data(&mPlug) }) {
      LILV_FOREACH(nodes, i, extdata.get()) {
         const auto node = lilv_nodes_get(extdata.get(), i);
         const auto uri = lilv_node_as_string(node);
         if (strcmp(uri, LV2_OPTIONS__interface) == 0)
            mWantsOptionsInterface = true;
         else if (strcmp(uri, LV2_STATE__interface) == 0)
            mWantsStateInterface = true;
      }
   }

   InitializeSettings(mPorts, mSettings);
   return true;
}

void LV2Effect::InitializeSettings(
   const LV2Ports &ports, LV2EffectSettings &settings)
{
   for (auto &controlPort : ports.mControlPorts) {
      auto &value = settings.values.emplace_back();
      value = controlPort->mDef;
   }
}

std::shared_ptr<EffectInstance> LV2Effect::MakeInstance() const
{
   return const_cast<LV2Effect*>(this)->DoMakeInstance();
}

std::shared_ptr<EffectInstance> LV2Effect::DoMakeInstance()
{
   LV2Preferences::GetUseGUI(*this, mUseGUI);
   return std::make_shared<LV2Instance>(*this, mPorts);
}

unsigned LV2Effect::GetAudioInCount() const
{
   return mPorts.mAudioIn;
}

unsigned LV2Effect::GetAudioOutCount() const
{
   return mPorts.mAudioOut;
}

int LV2Effect::GetMidiInCount() const
{
   return mPorts.mMidiIn;
}

int LV2Effect::GetMidiOutCount() const
{
   return mPorts.mMidiOut;
}

size_t LV2Instance::SetBlockSize(size_t maxBlockSize)
{
   auto pWrapper = GetWrapper();
   const auto &featuresList = GetEffect();
   mBlockSize = std::max(featuresList.mMinBlockSize,
      std::min({maxBlockSize, mUserBlockSize, featuresList.mMaxBlockSize}));
   if (pWrapper)
      pWrapper->SetBlockSize();
   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; ++i)
      mSlaves[i]->SetBlockSize();
   return mBlockSize;
}

size_t LV2Instance::GetBlockSize() const
{
   return mBlockSize;
}

sampleCount LV2Instance::GetLatency(const EffectSettings &, double)
{
   auto pWrapper = GetWrapper();
   if (pWrapper && mUseLatency && mPorts.mLatencyPort >= 0 && !mLatencyDone) {
      mLatencyDone = true;
      return sampleCount(pWrapper->GetLatency());
   }
   return 0;
}

// Start of destructive processing path
bool LV2Instance::ProcessInitialize(EffectSettings &settings,
   double sampleRate, sampleCount, ChannelNames chanMap)
{
   auto &mPortStates = GetEffect().mPortStates;
   if (!GetWrapper())
      MakeWrapper(settings, sampleRate, false);
   const auto pWrapper = GetWrapper();
   if (!pWrapper)
      return false;
   for (auto & state : mPortStates.mCVPortStates)
      state.mBuffer.reinit(mBlockSize, state.mpPort->mIsInput);
   pWrapper->Activate();
   mLatencyDone = false;
   return true;
}

size_t LV2Instance::ProcessBlock(EffectSettings &,
   const float *const *inbuf, float *const *outbuf, size_t size)
{
   using namespace LV2Symbols;
   auto pWrapper = GetWrapper();
   auto &mPortStates = GetEffect().mPortStates;

   assert(size <= mBlockSize);
   assert(pWrapper); // else ProcessInitialize() returned false, I'm not called
   const auto instance = &pWrapper->GetInstance();

   int i = 0;
   int o = 0;
   for (auto & port : mPorts.mAudioPorts)
      lilv_instance_connect_port(instance,
         port->mIndex,
         const_cast<float*>(port->mIsInput ? inbuf[i++] : outbuf[o++]));

   for (auto & state : mPortStates.mAtomPortStates)
      state->SendToInstance(mForge, mPositionFrame, mPositionSpeed);

   lilv_instance_run(instance, size);

   // Main thread consumes responses
   pWrapper->ConsumeResponses();

   for (auto & state : mPortStates.mAtomPortStates)
      state->ResetForInstanceOutput();

   return size;
}

bool LV2Instance::RealtimeInitialize(EffectSettings &, double)
{
   auto &mPortStates = GetEffect().mPortStates;
   for (auto & state : mPortStates.mCVPortStates)
      state.mBuffer.reinit(mBlockSize, state.mpPort->mIsInput);
   return true;
}

bool LV2Instance::RealtimeFinalize(EffectSettings &) noexcept
{
return GuardedCall<bool>([&]{
   auto &mPortStates = GetEffect().mPortStates;
   mSlaves.clear();
   for (auto & state : mPortStates.mCVPortStates)
      state.mBuffer.reset();
   return true;
});
}

bool LV2Instance::RealtimeAddProcessor(
   EffectSettings &settings, unsigned, float sampleRate)
{
   auto &featuresList = GetEffect();
   auto &mPortStates = GetEffect().mPortStates;
   auto pInstance = LV2Wrapper::Create(featuresList,
      mPorts, mPortStates, GetSettings(settings), sampleRate, false);
   if (!pInstance)
      return false;
   pInstance->Activate();
   mSlaves.push_back(move(pInstance));
   return true;
}

bool LV2Instance::RealtimeSuspend()
{
   mPositionSpeed = 0.0;
   mPositionFrame = 0;
   mRolling = false;

   return true;
}

bool LV2Instance::RealtimeResume()
{
   mPositionSpeed = 1.0;
   mPositionFrame = 0;
   mRolling = true;

   return true;
}

bool LV2Instance::RealtimeProcessStart(EffectSettings &)
{
   auto &mPortStates = GetEffect().mPortStates;
   mNumSamples = 0;
   for (auto & state : mPortStates.mAtomPortStates)
      state->SendToInstance(mForge, mPositionFrame, mPositionSpeed);
   return true;
}

size_t LV2Instance::RealtimeProcess(size_t group, EffectSettings &,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   auto &mPortStates = GetEffect().mPortStates;

   if (group >= mSlaves.size())
      return 0;
   wxASSERT(numSamples <= (size_t) mBlockSize);

   if (group < 0 || group >= (int) mSlaves.size())
   {
      return 0;
   }

   const auto slave = mSlaves[group].get();
   const auto instance = &slave->GetInstance();

   int i = 0;
   int o = 0;
   for (auto & port : mPorts.mAudioPorts)
      lilv_instance_connect_port(instance,
         port->mIndex,
         const_cast<float*>(port->mIsInput ? inbuf[i++] : outbuf[o++]));

   mNumSamples = std::max(numSamples, mNumSamples);

   if (mRolling)
      lilv_instance_run(instance, numSamples);
   else
      while (--i >= 0)
         for (decltype(numSamples) s = 0; s < numSamples; s++)
            outbuf[i][s] = inbuf[i][s];

   // Background thread consumes responses from yet another worker thread
   slave->ConsumeResponses();

   for (auto & state : mPortStates.mAtomPortStates)
      state->ResetForInstanceOutput();

   if (group == 0)
      mPositionFrame += numSamples;

   return numSamples;
}

bool LV2Instance::RealtimeProcessEnd(EffectSettings &) noexcept
{
return GuardedCall<bool>([&]{
   auto &mPortStates = GetEffect().mPortStates;
   // Nothing to do if we did process any samples
   if (mNumSamples == 0)
   {
      return true;
   }

   // Why is this not also done on the destructive processing path?
   // Because it is soon dimissing the modal dialog anyway.
   for (auto & state : mPortStates.mAtomPortStates)
      state->ReceiveFromInstance();

   mNumSamples = 0;

   return true;
});
}

int LV2Effect::ShowClientInterface(
   wxWindow &parent, wxDialog &dialog, bool forceModal)
{
   // Remember the dialog with a weak pointer, but don't control its lifetime
   mDialog = &dialog;

   // Try to give the window a sensible default/minimum size
   mDialog->Layout();
   mDialog->Fit();
   mDialog->SetMinSize(mDialog->GetSize());
   if (mNoResize)
   {
      mDialog->SetMaxSize(mDialog->GetSize());
   }

   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal)
   {
      mDialog->Show();
      return 0;
   }

   return mDialog->ShowModal();
}

bool LV2Effect::SaveSettings(
   const EffectSettings &settings, CommandParameters & parms) const
{
   auto &values = GetSettings(settings).values;
   size_t index = 0;
   for (auto & port : mPorts.mControlPorts) {
      if (port->mIsInput)
         if (!parms.Write(port->mName, values[index]))
            return false;
      ++index;
   }
   return true;
}

bool LV2Effect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   // First pass validates values
   for (auto & port : mPorts.mControlPorts) {
      if (port->mIsInput) {
         double d = 0.0;
         if (!parms.Read(port->mName, &d))
            return false;
         // Use unscaled range here
         if (d < port->mMin || d > port->mMax)
            return false;
      }
   }

   // Second pass actually sets the values
   auto &values = GetSettings(settings).values;
   size_t index = 0;
   for (auto & port : mPorts.mControlPorts) {
      if (port->mIsInput) {
         double d = 0.0;
         if (!parms.Read(port->mName, &d))
            return false;
         values[index] = d;
      }
      ++index;
   }

   return true;
}

// ============================================================================
// EffectUIClientInterface Implementation
// ============================================================================

// May come here before destructive processing
// Or maybe not (if you "Repeat Last Effect")
std::unique_ptr<EffectUIValidator> LV2Effect::PopulateUI(ShuttleGui &S,
   EffectInstance &instance, EffectSettingsAccess &access)
{
   auto &settings = access.Get();
   auto parent = S.GetParent();
   mParent = parent;

   mParent->PushEventHandler(this);

   mSuilHost.reset();
   mSuilInstance.reset();

   auto &myInstance = dynamic_cast<LV2Instance &>(instance);
   myInstance.MakeWrapper(settings, mProjectRate, true);
   if (!myInstance.GetWrapper()) {
      AudacityMessageBox( XO("Couldn't instantiate effect") );
      return nullptr;
   }

   // Determine if the GUI editor is supposed to be used or not
   LV2Preferences::GetUseGUI(*this, mUseGUI);

   // Until I figure out where to put the "Duration" control in the
   // graphical editor, force usage of plain editor.
   if (GetType() == EffectTypeGenerate)
      mUseGUI = false;

   if (mUseGUI)
      mUseGUI = BuildFancy(
         myInstance.GetWrapper()->GetInstance(), settings);

   if (!mUseGUI) {
      if (!BuildPlain(access))
         return nullptr;
   }

   return std::make_unique<DefaultEffectUIValidator>(*this, access);
}

bool LV2Effect::IsGraphicalUI()
{
   return mUseGUI;
}

bool LV2Effect::ValidateUI(EffectSettings &settings)
{
   if (GetType() == EffectTypeGenerate)
      settings.extra.SetDuration(mDuration->GetValue());

   return true;
}

bool LV2Effect::CloseUI()
{
#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(false);
#endif
#endif

   mParent->RemoveEventHandler(this);
   if (mSuilInstance) {
      if (mNativeWin) {
         mNativeWin->Destroy();
         mNativeWin = nullptr;
      }
      mUIIdleInterface = nullptr;
      mUIShowInterface = nullptr;
      mExternalWidget = nullptr;
      mSuilInstance.reset();
   }
   mSuilHost.reset();
   mParent = nullptr;
   mDialog = nullptr;

   // Restore initial state
   mExtensionDataFeature = {};
   mPlainUIControls.clear();
   return true;
}

bool LV2Effect::LoadUserPreset(
   const RegistryPath &name, EffectSettings &settings) const
{
   return LoadParameters(name, settings);
}

bool LV2Effect::SaveUserPreset(
   const RegistryPath &name, const EffectSettings &settings) const
{
   return SaveParameters(name, settings);
}

RegistryPaths LV2Effect::GetFactoryPresets() const
{
   using namespace LV2Symbols;
   if (mFactoryPresetsLoaded)
      return mFactoryPresetNames;

   if (LilvNodesPtr presets{ lilv_plugin_get_related(&mPlug, node_Preset) }) {
      LILV_FOREACH(nodes, i, presets.get()) {
         const auto preset = lilv_nodes_get(presets.get(), i);

         mFactoryPresetUris.push_back(LilvString(preset));

         lilv_world_load_resource(gWorld, preset);

         if (LilvNodesPtr labels{ lilv_world_find_nodes(gWorld, preset,
            node_Label, nullptr) }) {
            const auto label = lilv_nodes_get_first(labels.get());
            mFactoryPresetNames.push_back(LilvString(label));
         }
         else
            mFactoryPresetNames.push_back(
               LilvString(preset).AfterLast(wxT('#')));
      }
   }

   mFactoryPresetsLoaded = true;

   return mFactoryPresetNames;
}

bool LV2Effect::LoadFactoryPreset(int id, EffectSettings &settings) const
{
   using namespace LV2Symbols;
   if (id < 0 || id >= (int) mFactoryPresetUris.size())
      return false;

   LilvNodePtr preset{ lilv_new_uri(gWorld, mFactoryPresetUris[id].ToUTF8()) };
   if (!preset)
      return false;

   using LilvStatePtr = Lilv_ptr<LilvState, lilv_state_free>;
   if (LilvStatePtr state{
      lilv_state_new_from_world(gWorld, URIDMapFeature(), preset.get())
   }){
      auto &mySettings = GetSettings(settings);
      mPorts.EmitPortValues(*state, mySettings);
      // Save the state, for whatever might not be contained in port values
      mySettings.mpState = move(state);
      return true;
   }
   else
      return false;
}

bool LV2Effect::CanExportPresets()
{
   return false;
}

void LV2Effect::ExportPresets(const EffectSettings &) const
{
}

void LV2Effect::ImportPresets(EffectSettings &)
{
}

bool LV2Effect::HasOptions()
{
   return true;
}

void LV2Effect::ShowOptions()
{
   LV2Preferences::Dialog{ mParent, *this }.ShowModal();
}

// ============================================================================
// LV2Effect Implementation
// ============================================================================

bool LV2Effect::LoadParameters(
   const RegistryPath &group, EffectSettings &settings) const
{
   wxString parms;
   if (!GetConfig(*this,
      PluginSettings::Private, group, wxT("Parameters"), parms, wxEmptyString))
      return false;
   CommandParameters eap;
   if (!eap.SetParameters(parms))
      return false;
   return LoadSettings(eap, settings);
}

bool LV2Effect::SaveParameters(
   const RegistryPath &group, const EffectSettings &settings) const
{
   // PRL: This function just dumps the several control port values to the
   // config files.  Should it be reimplemented with
   // lilv_state_new_from_instance to capture -- I don't know what -- other
   // important state?

   CommandParameters eap;
   if (!SaveSettings(settings, eap))
      return false;

   wxString parms;
   if (!eap.GetParameters(parms))
      return false;

   return SetConfig(*this,
      PluginSettings::Private, group, wxT("Parameters"), parms);
}

bool LV2Effect::BuildFancy(
   LilvInstance &instance, const EffectSettings &settings)
{
   using namespace LV2Symbols;
   // Set the native UI type
   const char *nativeType =
#if defined(__WXGTK3__)
      LV2_UI__Gtk3UI;
#elif defined(__WXGTK__)
      LV2_UI__GtkUI;
#elif defined(__WXMSW__)
      LV2_UI__WindowsUI;
#elif defined(__WXMAC__)
      LV2_UI__CocoaUI;
#endif

   // Determine if the plugin has a supported UI
   const LilvUI *ui = nullptr;
   const LilvNode *uiType = nullptr;
   LilvUIsPtr uis{ lilv_plugin_get_uis(&mPlug) };
   if (uis) {
      if (LilvNodePtr containerType{ lilv_new_uri(gWorld, nativeType) }) {
         LILV_FOREACH(uis, iter, uis.get()) {
            ui = lilv_uis_get(uis.get(), iter);
            if (lilv_ui_is_supported(ui,
               suil_ui_supported, containerType.get(), &uiType))
               break;
            if (lilv_ui_is_a(ui, node_Gtk) || lilv_ui_is_a(ui, node_Gtk3)) {
               uiType = node_Gtk;
               break;
            }
            ui = nullptr;
         }
      }
   }

   // Check for other supported UIs
   if (!ui && uis) {
      LILV_FOREACH(uis, iter, uis.get()) {
         ui = lilv_uis_get(uis.get(), iter);
         if (lilv_ui_is_a(ui, node_ExternalUI) || lilv_ui_is_a(ui, node_ExternalUIOld))
         {
            uiType = node_ExternalUI;
            break;
         }
         ui = NULL;
      }
   }

   // No usable UI found
   if (ui == NULL)
      return false;

   const LilvNode *uinode = lilv_ui_get_uri(ui);
   lilv_world_load_resource(gWorld, uinode);
   if (!ValidateFeatures(uinode))
      return false;

   const char *containerType;

   if (uiType == node_ExternalUI)
   {
      containerType = LV2_EXTERNAL_UI__Widget;
   }
   else
   {
      containerType = nativeType;
      mFeatures[mParentFeature].data = mParent->GetHandle();

#if defined(__WXGTK__)
      // Make sure the parent has a window
      if (!gtk_widget_get_window(GTK_WIDGET(mParent->m_wxwindow)))
      {
         gtk_widget_realize(GTK_WIDGET(mParent->m_wxwindow));
      }
#endif
   }

   mFeatures[mInstanceAccessFeature].data = lilv_instance_get_handle(&instance);
   mExtensionDataFeature =
      { lilv_instance_get_descriptor(&instance)->extension_data };

   // Set before creating the UI instance so the initial size (if any) can be captured
   mNativeWinInitialSize = wxDefaultSize;
   mNativeWinLastSize = wxDefaultSize;

   // Create the suil host
   mSuilHost.reset(suil_host_new(LV2Effect::suil_port_write_func,
      LV2Effect::suil_port_index_func, nullptr, nullptr));
   if (!mSuilHost)
      return false;

#if defined(__WXMSW__)
   // Plugins may have dependencies that need to be loaded from the same path
   // as the main DLL, so add this plugin's path to the DLL search order.
   LilvCharsPtr libPath{
      lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_binary_uri(ui)),
      nullptr)
   };
   const auto path = wxPathOnly(libPath.get());
   SetDllDirectory(path.c_str());
#endif

   LilvCharsPtr bundlePath{
      lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_bundle_uri(ui)), nullptr)
   };
   LilvCharsPtr binaryPath{
      lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_binary_uri(ui)), nullptr)
   };

   // Reassign the sample rate, which is pointed to by options, which are
   // pointed to by features, before we tell the library the features
   mSampleRate = mProjectRate;
   mSuilInstance.reset(suil_instance_new(mSuilHost.get(),
      // The void* that the instance passes back to our write and index
      // callback functions, which were given to suil_host_new:
      this,
      containerType,
      lilv_node_as_uri(lilv_plugin_get_uri(&mPlug)),
      lilv_node_as_uri(lilv_ui_get_uri(ui)), lilv_node_as_uri(uiType),
      bundlePath.get(), binaryPath.get(), GetFeaturePointers().data()));

   // Bail if the instance (no compatible UI) couldn't be created
   if (!mSuilInstance)
   {
#if defined(__WXMSW__)
      SetDllDirectory(NULL);
#endif

      mSuilHost.reset();

      return false;
   }

   if (uiType == node_ExternalUI)
   {
      mParent->SetMinSize(wxDefaultSize);

      mExternalWidget = static_cast<LV2_External_UI_Widget *>(
         suil_instance_get_widget(mSuilInstance.get()));
      mTimer.SetOwner(this, ID_TIMER);
      mTimer.Start(20);

      LV2_EXTERNAL_UI_SHOW(mExternalWidget);
   }
   else
   {
      const auto widget = static_cast<WXWidget>(
         suil_instance_get_widget(mSuilInstance.get()));

#if defined(__WXGTK__)
      // Needed by some plugins (e.g., Invada) to ensure the display is fully
      // populated.
      gtk_widget_show_all(widget);

      // See note at size_request()
      g_signal_connect(widget, "size-request", G_CALLBACK(LV2Effect::size_request), this);
#endif

      Destroy_ptr< NativeWindow > uNativeWin{ safenew NativeWindow() };
      if ( !uNativeWin->Create(mParent, widget) )
         return false;
      mNativeWin = uNativeWin.release();

      mNativeWin->Bind(wxEVT_SIZE, &LV2Effect::OnSize, this);

      // The plugin called the LV2UI_Resize::ui_resize function to set the size before
      // the native window was created, so set the size now.
      if (mNativeWinInitialSize != wxDefaultSize)
      {
         mNativeWin->SetMinSize(mNativeWinInitialSize);
      }

      wxSizerItem *si = NULL;
      auto vs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      if (vs)
      {
         auto hs = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
         if (hs)
         {
            if (mNoResize)
            {
               si = hs->Add(mNativeWin, 0, wxCENTER);
               vs->Add(hs.release(), 1, wxCENTER);
            }
            else
            {
               si = hs->Add(mNativeWin, 1, wxEXPAND);
               vs->Add(hs.release(), 1, wxEXPAND);
            }
         }
      }

      if (!si)
         return false;

      mParent->SetSizerAndFit(vs.release());
   }

   mUIIdleInterface = static_cast<const LV2UI_Idle_Interface *>(
      suil_instance_extension_data(mSuilInstance.get(), LV2_UI__idleInterface));

   mUIShowInterface = static_cast<const LV2UI_Show_Interface *>(
      suil_instance_extension_data(mSuilInstance.get(), LV2_UI__showInterface));

   if (mUIShowInterface)
   {
//      mUIShowInterface->show(suil_instance_get_handle(mSuilInstance));
   }

   TransferDataToWindow(settings);

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(true);
#endif
#endif

#if defined(__WXMSW__)
   SetDllDirectory(NULL);
#endif

   return true;
}

bool LV2Effect::BuildPlain(EffectSettingsAccess &access)
{
   auto &settings = access.Get();
   auto &values = GetSettings(settings).values;
   mPlainUIControls.resize(mPorts.mControlPorts.size());

   int numCols = 5;
   wxSizer *innerSizer;

   assert(mParent); // To justify safenew
   const auto w = safenew wxScrolledWindow(mParent,
      wxID_ANY, wxDefaultPosition, wxDefaultSize, wxVSCROLL | wxTAB_TRAVERSAL);

   {
      auto outerSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      w->SetScrollRate(0, 20);
      // This fools NVDA into not saying "Panel" when the dialog gets focus
      w->SetName(wxT("\a"));
      w->SetLabel(wxT("\a"));
      outerSizer->Add(w, 1, wxEXPAND);

      auto uInnerSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      innerSizer = uInnerSizer.get();

      // Add the duration control, if a generator
      if (GetType() == EffectTypeGenerate) {
         auto sizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
         auto item = safenew wxStaticText(w, 0, _("&Duration:"));
         sizer->Add(item, 0, wxALIGN_CENTER | wxALL, 5);
         auto &extra = settings.extra;
         mDuration = safenew NumericTextCtrl(w, ID_Duration,
            NumericConverter::TIME, extra.GetDurationFormat(),
            extra.GetDuration(), mProjectRate,
            NumericTextCtrl::Options{}.AutoPos(true));
         mDuration->SetName( XO("Duration") );
         sizer->Add(mDuration, 0, wxALIGN_CENTER | wxALL, 5);
         auto groupSizer =
            std::make_unique<wxStaticBoxSizer>(wxVERTICAL, w, _("Generator"));
         groupSizer->Add(sizer.release(), 0, wxALIGN_CENTER | wxALL, 5);
         innerSizer->Add(groupSizer.release(), 0, wxEXPAND | wxALL, 5);
      }

      // Make other controls, grouped into static boxes that are named
      // according to certain control port metadata
      auto groups = mPorts.mGroups; // mutable copy
      std::sort(groups.begin(), groups.end(), TranslationLess);
      for (auto &label: groups) {
         auto gridSizer = std::make_unique<wxFlexGridSizer>(numCols, 5, 5);
         gridSizer->AddGrowableCol(3);
         for (auto & p : mPorts.mGroupMap.at(label)) /* won't throw */ {
            auto &state = mPortUIStates.mControlPortStates[p];
            auto &port = state.mpPort;
            auto &ctrl = mPlainUIControls[p];
            const auto &value = values[p];
            auto labelText = port->mName;
            if (!port->mUnits.empty())
               labelText += XO("(%s)").Format(port->mUnits).Translation();

            // A "trigger" port gets a row with just a pushbutton
            if (port->mTrigger) {
               gridSizer->Add(1, 1, 0);

               assert(w); // To justify safenew
               auto b = safenew wxButton(w, ID_Triggers + p, labelText);
               gridSizer->Add(b, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
               ctrl.button = b;

               gridSizer->Add(1, 1, 0);
               gridSizer->Add(1, 1, 0);
               gridSizer->Add(1, 1, 0);
               continue;
            }

            // Any other kind of port gets a name text...
            auto item = safenew wxStaticText(w, wxID_ANY, labelText + wxT(":"),
               wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT);
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT);

            // ... then appropriate controls and static texts in other columns
            if (port->mToggle) {
               // Toggle port gets a checkbox
               auto c = safenew wxCheckBox(w, ID_Toggles + p, wxT(""));
               c->SetName(labelText);
               c->SetValue(value > 0);
               gridSizer->Add(c, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
               ctrl.checkbox = c;

               gridSizer->Add(1, 1, 0);
               gridSizer->Add(1, 1, 0);
               gridSizer->Add(1, 1, 0);
            }
            else if (port->mEnumeration) {
               // Enumeration port gets a choice control
               // Discretize the value (all ports hold a float value) to
               // determine the intial selection
               auto s = port->Discretize(value);
               auto c = safenew wxChoice(w, ID_Choices + p);
               c->SetName(labelText);
               c->Append(port->mScaleLabels);
               c->SetSelection(s);
               gridSizer->Add(c, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
               ctrl.choice = c;

               gridSizer->Add(1, 1, 0);
               gridSizer->Add(1, 1, 0);
               gridSizer->Add(1, 1, 0);
            }
            else if (!port->mIsInput) {
               // Real-valued output gets a meter control
               gridSizer->Add(1, 1, 0);
               gridSizer->Add(1, 1, 0);

               //! Captures a const reference to value!
               auto m = safenew LV2EffectMeter(w, port, value);
               gridSizer->Add(m, 0, wxALIGN_CENTER_VERTICAL | wxEXPAND);
               ctrl.meter = m;

               gridSizer->Add(1, 1, 0);
            }
            else {
               // Numerical input gets a text input, with a validator...
               auto t = safenew wxTextCtrl(w, ID_Texts + p, wxT(""));
               t->SetName(labelText);
               gridSizer->Add(t, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
               ctrl.mText = t;
               auto rate = port->mSampleRate ? mProjectRate : 1.0f;
               state.mLo = port->mMin * rate;
               state.mHi = port->mMax * rate;
               state.mTmp = value * rate;
               if (port->mInteger) {
                  IntegerValidator<float> vld(&state.mTmp);
                  vld.SetRange(state.mLo, state.mHi);
                  t->SetValidator(vld);
               }
               else {
                  FloatingPointValidator<float> vld(6, &state.mTmp);
                  vld.SetRange(state.mLo, state.mHi);

                  // Set number of decimal places
                  float range = state.mHi - state.mLo;
                  auto style = range < 10
                     ? NumValidatorStyle::THREE_TRAILING_ZEROES
                     : range < 100
                        ? NumValidatorStyle::TWO_TRAILING_ZEROES
                        : NumValidatorStyle::ONE_TRAILING_ZERO;
                  vld.SetStyle(style);
                  t->SetValidator(vld);
               }

               // ... optional lower-bound static text ...
               if (port->mHasLo) {
                  wxString str;
                  if (port->mInteger || port->mSampleRate)
                     str.Printf(wxT("%d"), (int) lrintf(state.mLo));
                  else
                     str = Internat::ToDisplayString(state.mLo);
                  item = safenew wxStaticText(w, wxID_ANY, str);
                  gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT);
               }
               else
                  gridSizer->Add(1, 1, 0);

               // ... a slider ...
               auto s = safenew wxSliderWrapper(w, ID_Sliders + p,
                  0, 0, 1000, wxDefaultPosition, { 150, -1 });
               s->SetName(labelText);
               gridSizer->Add(s, 0, wxALIGN_CENTER_VERTICAL | wxEXPAND);
               ctrl.slider = s;

               // ... and optional upper-bound static text
               if (port->mHasHi) {
                  wxString str;
                  if (port->mInteger || port->mSampleRate)
                     str.Printf(wxT("%d"), (int) lrintf(state.mHi));
                  else
                     str = Internat::ToDisplayString(state.mHi);
                  item = safenew wxStaticText(w, wxID_ANY, str);
                  gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
               }
               else
                  gridSizer->Add(1, 1, 0);
            }
         }

         auto groupSizer = std::make_unique<wxStaticBoxSizer>(
            wxVERTICAL, w, label.Translation());
         groupSizer->Add(gridSizer.release(), 1, wxEXPAND | wxALL, 5);
         innerSizer->Add(groupSizer.release(), 0, wxEXPAND | wxALL, 5);
      }

      innerSizer->Layout();

      //! Function to revisit the controls just added above
      auto VisitCells = [&, cnt = innerSizer->GetChildren().GetCount()](auto f){
         for (size_t i = (GetType() == EffectTypeGenerate); i < cnt; ++i) {
            // For each group (skipping duration) visit the grid sizer
            auto groupSizer = innerSizer->GetItem(i)->GetSizer();
            auto gridSizer = static_cast<wxFlexGridSizer *>(
               groupSizer->GetItem(size_t{0})->GetSizer());
            auto items = gridSizer->GetChildren().GetCount();
            size_t cols = gridSizer->GetCols();
            for (size_t j = 0; j < items; ++j) {
               // For each grid item
               auto item = gridSizer->GetItem(j);
               f(item, j, cols);
            }
         }
      };

      // Calculate the maximum width of all columns (bypass Generator sizer)
      std::vector<int> widths(numCols);
      VisitCells([&](wxSizerItem *item, size_t j, size_t cols){
         auto &width = widths[j % cols];
         width = std::max(width, item->GetSize().GetWidth());
      });

      // Set each column in all of the groups to the same width.
      VisitCells([&](wxSizerItem *item, size_t j, size_t cols){
         int flags = item->GetFlag();
         if (flags & wxEXPAND)
            return;
         if (flags & wxALIGN_RIGHT)
            flags = (flags & ~wxALL) | wxLEFT;
         else
            flags = (flags & ~wxALL) | wxRIGHT;
         item->SetFlag(flags);
         item->SetBorder(widths[j % cols] - item->GetMinSize().GetWidth());
      });

      w->SetSizer(uInnerSizer.release());

      mParent->SetSizer(outerSizer.release());
   } // scope of unique_ptrs of sizers

   // Try to give the window a sensible default/minimum size
   wxSize sz1 = innerSizer->GetMinSize();
   wxSize sz2 = mParent->GetMinSize();
   w->SetMinSize( { -1, std::min(sz1.y, sz2.y) } );

   // And let the parent reduce to the NEW minimum if possible
   mParent->SetMinSize(w->GetMinSize());

   TransferDataToWindow(settings);
   return true;
}

bool LV2Effect::TransferDataToWindow(const EffectSettings &settings)
{
   auto &mySettings = GetSettings(settings);

   if (mMaster && mySettings.mpState) {
      // Maybe there are other important side effects on the instance besides
      // changes of port values
      lilv_state_restore(mySettings.mpState.get(), &mMaster->GetInstance(),
         nullptr, nullptr, 0, nullptr);
      // Destroy the short lived carrier of preset state
      mySettings.mpState.reset();
   }

   auto &values = mySettings.values;
   {
      size_t index = 0; for (auto & state : mPortUIStates.mControlPortStates) {
         auto &port = state.mpPort;
         if (port->mIsInput)
            state.mTmp =
               values[index] * (port->mSampleRate ? mProjectRate : 1.0);
         ++index;
      }
   }

   if (mUseGUI) {
      // fancy UI
      if (mSuilInstance) {
         size_t index = 0;
         for (auto & port : mPorts.mControlPorts) {
            if (port->mIsInput)
               suil_instance_port_event(mSuilInstance.get(),
                  port->mIndex, sizeof(float),
                  /* Means this event sends a float: */ 0,
                  &values[index]);
            ++index;
         }
      }
      return true;
   }

   // else plain UI
   // Visiting controls by groups
   for (auto & group : mPorts.mGroups) {
      const auto & params = mPorts.mGroupMap.at(group); /* won't throw */
      for (auto & param : params) {
         auto &state = mPortUIStates.mControlPortStates[param];
         auto &port = state.mpPort;
         auto &ctrl = mPlainUIControls[param];
         auto &value = values[param];
         if (port->mTrigger)
            continue;
         else if (port->mToggle)
            ctrl.checkbox->SetValue(value > 0);
         else if (port->mEnumeration)      // Check before integer
            ctrl.choice->SetSelection(port->Discretize(value));
         else if (port->mIsInput) {
            state.mTmp = value * (port->mSampleRate ? mProjectRate : 1.0f);
            SetSlider(state, ctrl);
         }
      }
   }
   if (mParent && !mParent->TransferDataToWindow())
      return false;
   return true;
}

void LV2Effect::SetSlider(
   const LV2ControlPortState &state, const PlainUIControl &ctrl)
{
   float lo = state.mLo;
   float hi = state.mHi;
   float val = state.mTmp;
   if (state.mpPort->mLogarithmic) {
      lo = logf(lo);
      hi = logf(hi);
      val = logf(val);
   }
   ctrl.slider->SetValue(lrintf((val - lo) / (hi - lo) * 1000.0));
}

void LV2Effect::OnTrigger(wxCommandEvent &evt)
{
   size_t idx = evt.GetId() - ID_Triggers;
   auto & port = mPorts.mControlPorts[idx];
   mSettings.values[idx] = port->mDef;
}

void LV2Effect::OnToggle(wxCommandEvent &evt)
{
   size_t idx = evt.GetId() - ID_Toggles;
   auto & port = mPorts.mControlPorts[idx];
   mSettings.values[idx] = evt.GetInt() ? 1.0 : 0.0;
}

void LV2Effect::OnChoice(wxCommandEvent &evt)
{
   size_t idx = evt.GetId() - ID_Choices;
   auto & port = mPorts.mControlPorts[idx];
   mSettings.values[idx] = port->mScaleValues[evt.GetInt()];
}

void LV2Effect::OnText(wxCommandEvent &evt)
{
   size_t idx = evt.GetId() - ID_Texts;
   auto &state = mPortUIStates.mControlPortStates[idx];
   auto &port = state.mpPort;
   auto &ctrl = mPlainUIControls[idx];
   if (ctrl.mText->GetValidator()->TransferFromWindow()) {
      mSettings.values[idx] =
         port->mSampleRate ? state.mTmp / mProjectRate : state.mTmp;
      SetSlider(state, ctrl);
   }
}

void LV2Effect::OnSlider(wxCommandEvent &evt)
{
   size_t idx = evt.GetId() - ID_Sliders;
   auto &state = mPortUIStates.mControlPortStates[idx];
   auto &port = state.mpPort;
   float lo = state.mLo;
   float hi = state.mHi;
   if (port->mLogarithmic) {
      lo = logf(lo);
      hi = logf(hi);
   }
   state.mTmp = (((float) evt.GetInt()) / 1000.0) * (hi - lo) + lo;
   state.mTmp = std::clamp(state.mTmp, lo, hi);
   state.mTmp = port->mLogarithmic ? expf(state.mTmp) : state.mTmp;
   mSettings.values[idx] =
      port->mSampleRate ? state.mTmp / mProjectRate : state.mTmp;
   mPlainUIControls[idx].mText->GetValidator()->TransferToWindow();
}

void LV2Effect::OnTimer(wxTimerEvent &evt)
{
   evt.Skip();

   if (mExternalWidget)
   {
      LV2_EXTERNAL_UI_RUN(mExternalWidget);
   }
}

void LV2Effect::OnIdle(wxIdleEvent &evt)
{
   evt.Skip();
   if (!mSuilInstance)
      return;

   if (mExternalUIClosed) {
      mExternalUIClosed = false;
      mDialog->Close();
      return;
   }

   if (mUIIdleInterface) {
      const auto handle = suil_instance_get_handle(mSuilInstance.get());
      if (mUIIdleInterface->idle(handle)) {
         if (mUIShowInterface)
            mUIShowInterface->hide(handle);
         mDialog->Close();
         return;
      }
   }

   if (auto &atomState = mPortUIStates.mControlOut) {
      atomState->SendToDialog([&](const LV2_Atom *atom, uint32_t size){
         suil_instance_port_event(mSuilInstance.get(),
            atomState->mpPort->mIndex, size,
            // Means this event sends some structured data:
            LV2Symbols::urid_EventTransfer, atom);
      });
   }

   // Is this idle time polling for changes of input redundant with
   // TransferDataToWindow or is it really needed?  Probably harmless.
   // In case of output control port values though, it is needed for metering.
   size_t index = 0;
   for (auto &state : mPortUIStates.mControlPortStates) {
      auto &port = state.mpPort;
      const auto &value = mSettings.values[index];
      // Let UI know that a port's value has changed
      if (value != state.mLst) {
         suil_instance_port_event(mSuilInstance.get(),
            port->mIndex, sizeof(value),
            /* Means this event sends a float: */ 0,
            &value);
         state.mLst = value;
      }
      ++index;
   }
}

void LV2Effect::OnSize(wxSizeEvent & evt)
{
   evt.Skip();

   // Don't do anything here if we're recursing
   if (mResizing)
   {
      return;
   }

   // Indicate resizing is occurring
   mResizing = true;

   // Can only resize AFTER the dialog has been completely created and
   // there's no need to resize if we're already at the desired size.
   if (mDialog && evt.GetSize() != mNativeWinLastSize)
   {
      // Save the desired size and set the native window to match
      mNativeWinLastSize = evt.GetSize();
      mNativeWin->SetMinSize(mNativeWinLastSize);

      // Clear the minimum size of the parent window to allow the following
      // Fit() to make proper adjustments
      mParent->SetMinSize(wxDefaultSize);

#if defined(__WXGTK__)
      // If the user resized the native window, then we need to also
      // clear the dialogs minimum size.  If this isn't done, the dialog
      // will not resize properly when going from a larger size to a smaller
      // size (due to the minimum size constraint).
      //
      // In this case, mResized has been set by the "size_request()" function
      // to indicate that this is a plugin generated resize request.
      if (mResized)
      {
        mDialog->SetMinSize(wxDefaultSize);
      }

      // Resize dialog
      mDialog->Fit();

      // Reestablish the minimum (and maximum) now that the dialog
      // has is desired size.
      if (mResized)
      {
         mDialog->SetMinSize(mDialog->GetSize());
         if (mNoResize)
         {
            mDialog->SetMaxSize(mDialog->GetSize());
         }
      }

      // Tell size_request() that the native window was just resized.
      mResized = true;
#else
      // Resize the dialog to fit its content.
      mDialog->Fit();
#endif
   }

   // No longer resizing
   mResizing = false;
}

// ============================================================================
// Feature handlers
// ============================================================================

// static callback
int LV2Effect::ui_resize(LV2UI_Feature_Handle handle, int width, int height)
{
   return static_cast<LV2Effect *>(handle)->UIResize(width, height);
}

int LV2Effect::UIResize(int width, int height)
{
   // Queue a wxSizeEvent to resize the plugins UI
   if (mNativeWin) {
      wxSizeEvent sw{ wxSize{ width, height } };
      sw.SetEventObject(mNativeWin);
      mNativeWin->GetEventHandler()->AddPendingEvent(sw);
   }
   else
      // The window hasn't been created yet, so record the desired size
      mNativeWinInitialSize = { width, height };
   return 0;
}

// static callback
void LV2Effect::ui_closed(LV2UI_Controller controller)
{
   return static_cast<LV2Effect *>(controller)->UIClosed();
}

void LV2Effect::UIClosed()
{
   mExternalUIClosed = true;
}

// static callback
// Foreign UI code wants to send a value or event to me, the host
void LV2Effect::suil_port_write_func(SuilController controller,
   uint32_t port_index, uint32_t buffer_size, uint32_t protocol,
   const void *buffer)
{
   static_cast<LV2Effect *>(controller)
      ->SuilPortWrite(port_index, buffer_size, protocol, buffer);
}

void LV2Effect::SuilPortWrite(uint32_t port_index,
   uint32_t buffer_size, uint32_t protocol, const void *buffer)
{
   // Handle implicit floats
   if (protocol == 0 && buffer_size == sizeof(float)) {
      if (auto it = mPorts.mControlPortMap.find(port_index)
         ; it != mPorts.mControlPortMap.end()
      )
         mSettings.values[it->second] = *static_cast<const float *>(buffer);
   }
   // Handle event transfers
   else if (protocol == LV2Symbols::urid_EventTransfer) {
      auto &atomPortState = mPortUIStates.mControlIn;
      if (atomPortState && port_index == atomPortState->mpPort->mIndex)
         atomPortState->ReceiveFromDialog(buffer, buffer_size);
   }
}

// static callback
uint32_t LV2Effect::suil_port_index_func(
   SuilController controller, const char *port_symbol)
{
   return static_cast<LV2Effect *>(controller)->SuilPortIndex(port_symbol);
}

uint32_t LV2Effect::SuilPortIndex(const char *port_symbol)
{
   for (size_t i = 0, cnt = lilv_plugin_get_num_ports(&mPlug); i < cnt; ++i) {
      const auto port = lilv_plugin_get_port_by_index(&mPlug, i);
      if (strcmp(port_symbol,
            lilv_node_as_string(lilv_port_get_symbol(&mPlug, port))) == 0)
         return lilv_port_get_index(&mPlug, port);
   }
   return LV2UI_INVALID_PORT_INDEX;
}

#if defined(__WXGTK__)
// static callback
//
// Need to queue a wxSizeEvent when the native window gets resized outside of
// WX control.  Many of the x42 LV2 plugins can resize themselves when changing
// the scale factor. (e.g., open "x42-dpl" effect and right click to change scaling)
void LV2Effect::size_request(GtkWidget *widget, GtkRequisition *requisition, LV2Effect *effect)
{
   effect->SizeRequest(widget, requisition);
}

void LV2Effect::SizeRequest(GtkWidget *widget, GtkRequisition *requisition)
{
   // Don't do anything if the OnSize() method is active
   if (!mResizing)
   {
      // If the OnSize() routine has processed an event, mResized will be true,
      // so just set the widgets size.
      if (mResized)
      {
         gtk_widget_set_size_request(widget, mNativeWinLastSize.x, mNativeWinLastSize.y);
         mResized = false;
      }
      // Otherwise, the plugin has resized the widget and we need to let WX know
      // about it.
      else if (mNativeWin)
      {
         mResized = true;
         wxSizeEvent se(wxSize(requisition->width, requisition->height));
         se.SetEventObject(mNativeWin);
         mNativeWin->GetEventHandler()->AddPendingEvent(se);
      }
   }
}
#endif

#endif
