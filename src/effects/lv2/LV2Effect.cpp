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
#include "LV2Instance.h"
#include "LV2Validator.h"
#include "LV2Wrapper.h"
#include "SampleCount.h"

#include <cmath>
#include <exception>
#include <functional>


#ifdef __WXMAC__
#include <wx/evtloop.h>
#endif

#include "ConfigInterface.h"
#include "../../widgets/AudacityMessageBox.h"

#if defined(__WXGTK__)
#include <gtk/gtk.h>
#endif

#if defined(__WXMSW__)
#include <wx/msw/wrapwin.h>
#endif

///////////////////////////////////////////////////////////////////////////////
//
// LV2Effect
//
///////////////////////////////////////////////////////////////////////////////

LV2Effect::LV2Effect(const LilvPlugin &plug) : mPlug{ plug }
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
   return LV2FeaturesList::GetPluginSymbol(mPlug);
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
   if (mPorts.mAudioIn == 0 && mPorts.mAudioOut == 0)
   {
      return EffectTypeTool;
   }

   if (mPorts.mAudioIn == 0)
   {
      return EffectTypeGenerate;
   }

   if (mPorts.mAudioOut == 0)
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
   return GetType() == EffectTypeProcess
      ? RealtimeSince::Always
      : RealtimeSince::Never;
}

bool LV2Effect::SupportsAutomation() const
{
   return true;
}

bool LV2Effect::InitializePlugin()
{
   if (!mFeatures.mOk)
      return false;

   // Do a check only on temporary feature list objects
   auto instanceFeatures = LV2InstanceFeaturesList{ mFeatures };
   if (!instanceFeatures.mOk)
      return false;
   if (!LV2UIFeaturesList{ LV2WrapperFeaturesList{instanceFeatures},
      nullptr, lilv_plugin_get_uri(&mPlug)
   }.mOk)
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

   return true;
}

EffectSettings LV2Effect::MakeSettings() const
{
   auto result = EffectSettings::Make<LV2EffectSettings>();
   auto &settings = GetSettings(result);
   // This may waste a bit of space on output ports, but not likely much
   settings.values.reserve(mPorts.mControlPorts.size());
   for (auto &controlPort : mPorts.mControlPorts) {
      auto &value = settings.values.emplace_back();
      value = controlPort->mDef;
   }
   return result;
}

bool LV2Effect::CopySettingsContents(
   const EffectSettings &src, EffectSettings &dst) const
{
   auto &srcControls = GetSettings(src).values;
   auto &dstControls = GetSettings(dst).values;

   const auto &controlPorts = mPorts.mControlPorts;
   const auto portsCount = controlPorts.size();
   // Do not use the copy constructor of std::vector.  Do an in-place rewrite
   // of the destination vector, which will not allocate memory if dstControls
   // began with sufficient capacity.
   // And that will be true if dstControls originated with MakeSettings() or a
   // copy of it, because the set of control ports does not vary after
   // initialization of the plug-in.
   assert(srcControls.size() == portsCount);
   assert(dstControls.size() == portsCount);
   // But let's be sure
   const auto portValuesCount =
      std::min(srcControls.size(), dstControls.size());

   if (portValuesCount != portsCount)
      return false;

   size_t portIndex {};

   for (auto& port : controlPorts)
   {
      if (port->mIsInput)
         dstControls[portIndex] = srcControls[portIndex];

      ++portIndex;
   }

   // Ignore mpState

   return true;
}

auto LV2Effect::MakeOutputs() const -> std::unique_ptr<EffectOutputs>
{
   auto result = std::make_unique<LV2EffectOutputs>();
   auto &values = result->values;
   // This may waste a bit of space on input ports, but not likely much
   values.resize(mPorts.mControlPorts.size());
   return result;
}

std::shared_ptr<EffectInstance> LV2Effect::MakeInstance() const
{
   auto result = std::make_shared<LV2Instance>(*this, mFeatures, mPorts);
   if (result->IsOk())
      return result;
   return nullptr;
}

int LV2Effect::ShowClientInterface(wxWindow &parent, wxDialog &dialog,
   EffectUIValidator *pValidator, bool forceModal)
{
   if (pValidator)
      // Remember the dialog with a weak pointer, but don't control its lifetime
      static_cast<LV2Validator*>(pValidator)->mDialog = &dialog;
   // Try to give the window a sensible default/minimum size
   dialog.Layout();
   dialog.Fit();
   dialog.SetMinSize(dialog.GetSize());
   if (mFeatures.mNoResize)
      dialog.SetMaxSize(dialog.GetSize());
   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal) {
      dialog.Show();
      return 0;
   }
   return dialog.ShowModal();
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
   EffectInstance &instance, EffectSettingsAccess &access,
   const EffectOutputs *pOutputs)
{
   auto &settings = access.Get();
   auto parent = S.GetParent();
   mParent = parent;

   auto &myInstance = dynamic_cast<LV2Instance &>(instance);
   auto pWrapper =
      // Output port connection isn't needed for fancy UI wrapper.  Its
      // features are needed to make the suil_instance
      myInstance.MakeWrapper(settings, mProjectRate, nullptr);
   if (!pWrapper) {
      AudacityMessageBox( XO("Couldn't instantiate effect") );
      return nullptr;
   }

   // Determine if the GUI editor is supposed to be used or not
   bool useGUI = false;
   LV2Preferences::GetUseGUI(*this, useGUI);

   // Until I figure out where to put the "Duration" control in the
   // graphical editor, force usage of plain editor.
   if (GetType() == EffectTypeGenerate)
      useGUI = false;

   auto result = std::make_unique<LV2Validator>(*this, mPlug,
      dynamic_cast<LV2Instance&>(instance),
      access, pOutputs, mProjectRate, mFeatures, mPorts, parent, useGUI);

   if (result->mUseGUI)
      result->mUseGUI = result->BuildFancy(move(pWrapper), settings);
   if (!result->mUseGUI && !result->BuildPlain(access))
      return nullptr;
   result->UpdateUI();

   return result;
}

bool LV2Effect::CloseUI()
{
#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(false);
#endif
#endif

   mParent = nullptr;
   return true;
}

OptionalMessage LV2Effect::LoadUserPreset(
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

OptionalMessage
LV2Effect::LoadFactoryPreset(int id, EffectSettings &settings) const
{
   using namespace LV2Symbols;
   if (id < 0 || id >= (int) mFactoryPresetUris.size())
      return {};

   LilvNodePtr preset{ lilv_new_uri(gWorld, mFactoryPresetUris[id].ToUTF8()) };
   if (!preset)
      return {};

   using LilvStatePtr = Lilv_ptr<LilvState, lilv_state_free>;
   LilvStatePtr state{
      lilv_state_new_from_world(gWorld,
         mFeatures.URIDMapFeature(), preset.get())
   };
   if (!state)
      return {};

   auto &mySettings = GetSettings(settings);
   mPorts.EmitPortValues(*state, mySettings);
   // Save the state, for whatever might not be contained in port values
   mySettings.mpState = move(state);
   return { nullptr };
}

bool LV2Effect::CanExportPresets()
{
   return false;
}

void LV2Effect::ExportPresets(const EffectSettings &) const
{
}

OptionalMessage LV2Effect::ImportPresets(EffectSettings &)
{
   return { nullptr };
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

OptionalMessage LV2Effect::LoadParameters(
   const RegistryPath &group, EffectSettings &settings) const
{
   wxString parms;
   if (!GetConfig(*this,
      PluginSettings::Private, group, wxT("Parameters"), parms, wxEmptyString))
      return {};
   CommandParameters eap;
   if (!eap.SetParameters(parms))
      return {};
   if (!LoadSettings(eap, settings))
      return {};
   return { nullptr };
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
#endif
