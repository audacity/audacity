/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.cpp

  Dominic Mazzoni

  This class implements a LADSPA Plug-in effect.

*******************************************************************//**

\class LadspaEffect
\brief An Effect that calls up a LADSPA plug in, i.e. many possible
effects from this one class.

*//****************************************************************//**

\class LadspaEffectDialog
\brief Dialog used with Effect

*//*******************************************************************/
#include "LadspaEffect.h"       // This class's header file
#include "LadspaEditor.h"
#include "LadspaEffectOptionsDialog.h"
#include "ConfigInterface.h"

#include <wx/log.h>
#include "ShuttleGui.h"

// Don't use the template-generated MakeSettings(), which default-constructs
// the structure.  Instead allocate a number of values chosen by the plug-in
EffectSettings LadspaEffect::MakeSettings() const
{
   auto result = EffectSettings::Make<LadspaEffectSettings>( mData->PortCount );
   InitializeControls(GetSettings(result));
   return result;
}

bool LadspaEffect::CopySettingsContents(
   const EffectSettings &src, EffectSettings &dst) const
{
   // Do not use the copy constructor of std::vector.  Do an in-place rewrite
   // of the destination vector, which will not allocate memory if dstControls
   // began with sufficient capacity.
   const auto portCount = mData->PortCount;

   auto &srcControls = GetSettings(src).controls;
   auto &dstControls = GetSettings(dst).controls;

   assert(srcControls.size() == portCount);
   assert(dstControls.size() == portCount);

   const auto portValuesCount =
      std::min(srcControls.size(), dstControls.size());

   if (portValuesCount != portCount)
      return false;

   for (unsigned long p = 0; p < portCount; ++p)
   {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];

      if (!(LADSPA_IS_PORT_CONTROL(d)))
         continue;

      if (LADSPA_IS_PORT_INPUT(d))
         dstControls[p] = srcControls[p];
   }

   return true;
}

auto LadspaEffect::MakeOutputs() const -> std::unique_ptr<EffectOutputs>
{
   auto result = std::make_unique<LadspaEffectOutputs>();
   result->controls.resize(mData->PortCount);
   return result;
}

BEGIN_EVENT_TABLE(LadspaEffectOptionsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, LadspaEffectOptionsDialog::OnOk)
END_EVENT_TABLE()

LadspaEffectOptionsDialog::LadspaEffectOptionsDialog(
   const EffectDefinitionInterface &effect
)  : wxDialogWrapper{ nullptr, wxID_ANY, XO("LADSPA Effect Options") }
   , mEffect{ effect }
   , mUseLatency{ LadspaInstance::LoadUseLatency(mEffect) }
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

LadspaEffectOptionsDialog::~LadspaEffectOptionsDialog()
{
}

void LadspaEffectOptionsDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(false);
      {
         S.StartStatic(XO("Latency Compensation"));
         {
            S.AddVariableText( XO(
"As part of their processing, some LADSPA effects must delay returning "
"audio to Audacity. When not compensating for this delay, you will "
"notice that small silences have been inserted into the audio. "
"Enabling this option will provide that compensation, but it may "
"not work for all LADSPA effects."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(XXO("Enable &compensation"),
                             mUseLatency);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();
      }
      S.EndVerticalLay();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   Center();
}

void LadspaEffectOptionsDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   if (!Validate())
   {
      return;
   }

   ShuttleGui S(this, eIsGettingFromDialog);
   // Note this call re-visits the controls, not to create them but to fetch
   // the values, in this case mUseLatency
   PopulateOrExchange(S);

   LadspaInstance::SaveUseLatency(mEffect, mUseLatency);

   EndModal(wxID_OK);
}

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffect
//
///////////////////////////////////////////////////////////////////////////////

LadspaEffect::LadspaEffect(const wxString & path, int index)
   : mPath{ path }
   , mIndex{ index }
{
}

LadspaEffect::~LadspaEffect()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath LadspaEffect::GetPath() const
{
   return wxString::Format(wxT("%s;%d"), mPath, mIndex);
}

ComponentInterfaceSymbol LadspaEffect::GetSymbol() const
{
   return LAT1CTOWX(mData->Name);
}

VendorSymbol LadspaEffect::GetVendor() const
{
   return { LAT1CTOWX(mData->Maker) };
}

wxString LadspaEffect::GetVersion() const
{
   return "n/a";
}

TranslatableString LadspaEffect::GetDescription() const
{
   return Verbatim( LAT1CTOWX(mData->Copyright) );
}

// ============================================================================
// EffectDefinitionInterface implementation
// ============================================================================

EffectType LadspaEffect::GetType() const
{
   if (mAudioIns == 0 && mAudioOuts == 0)
   {
      return EffectTypeTool;
   }

   if (mAudioIns == 0)
   {
      return EffectTypeGenerate;
   }

   if (mAudioOuts == 0)
   {
      return EffectTypeAnalyze;
   }

   return EffectTypeProcess;
}

EffectFamilySymbol LadspaEffect::GetFamily() const
{
   return LADSPAEFFECTS_FAMILY;
}

bool LadspaEffect::IsInteractive() const
{
   return mInteractive;
}

bool LadspaEffect::IsDefault() const
{
   return false;
}

auto LadspaEffect::RealtimeSupport() const -> RealtimeSince
{
   return GetType() == EffectTypeProcess
      ? RealtimeSince::After_3_1
      : RealtimeSince::Never;
}

bool LadspaEffect::SupportsAutomation() const
{
   return mNumInputControls > 0;
}

namespace {
std::pair<float, float>
InputControlPortBounds(const LADSPA_PortRangeHint &hint, double sampleRate)
{
   // Find lower and upper bound values for ths hint
   const auto multiplier =
      LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor) ? sampleRate : 1.0;
   return { hint.LowerBound * multiplier, hint.UpperBound * multiplier };
}
float ClampInputControlValue(
   const LADSPA_PortRangeHint &hint, float val, float lower, float upper)
{
   if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor) && val < lower)
      val = lower;
   if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor) && val > upper)
      val = upper;
   return val;
}
float InputControlPortDefaultValue(
   const LADSPA_PortRangeHint &hint, double sampleRate)
{
   // See comments in library header ladspa.h about interpretation of macros
   const auto bounds = InputControlPortBounds(hint, sampleRate);

   // Function to find low, middle, or high default values
   const auto combine = [bounds,
      logarithmic = LADSPA_IS_HINT_LOGARITHMIC(hint.HintDescriptor)
   ](float lowWeight, float highWeight){
      auto [lower, upper] = bounds;
      return logarithmic
         ? exp(log(lower) * lowWeight + log(upper) * highWeight)
         : lower * lowWeight + upper * highWeight;
   };

   auto [lower, upper] = bounds;
   auto val = 1.0f;
   // Four bits of the descriptor describe mutually exclusive cases
   switch (hint.HintDescriptor & LADSPA_HINT_DEFAULT_MASK) {
   case LADSPA_HINT_DEFAULT_NONE:
   default:
      break;
   case LADSPA_HINT_DEFAULT_MINIMUM:
      val = lower; break;
   case LADSPA_HINT_DEFAULT_LOW:
      val = combine(0.75, 0.25); break;
   case LADSPA_HINT_DEFAULT_MIDDLE:
      val = combine(0.5, 0.5); break;
   case LADSPA_HINT_DEFAULT_HIGH:
      val = combine(0.25, 0.75); break;
   case LADSPA_HINT_DEFAULT_MAXIMUM:
      val = upper; break;
   case LADSPA_HINT_DEFAULT_0:
      val = 0.0f; break;
   case LADSPA_HINT_DEFAULT_1:
      val = 1.0f; break;
   case LADSPA_HINT_DEFAULT_100:
      val = 100.0f; break;
   case LADSPA_HINT_DEFAULT_440:
      val = 440.0f; break;
   }

   return ClampInputControlValue(hint, val, lower, upper);
}
}

bool LadspaEffect::InitializePlugin()
{
   if (!Load())
      return false;

   mInputPorts.reinit( mData->PortCount );
   mOutputPorts.reinit( mData->PortCount );
   for (unsigned long p = 0; p < mData->PortCount; p++) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];

      // Collect the audio ports
      if (LADSPA_IS_PORT_AUDIO(d)) {
         if (LADSPA_IS_PORT_INPUT(d))
            mInputPorts[mAudioIns++] = p;
         else if (LADSPA_IS_PORT_OUTPUT(d))
            mOutputPorts[mAudioOuts++] = p;
      }
      // Count control ports
      else if (LADSPA_IS_PORT_CONTROL(d)) {
         if (LADSPA_IS_PORT_INPUT(d)) {
            mInteractive = true;
            ++mNumInputControls;
         }
         else if (LADSPA_IS_PORT_OUTPUT(d)) {
            // LADSPA effects have a convention of providing latency on an output
            // control port whose name is "latency".
            if (strcmp(mData->PortNames[p], "latency") == 0)
               mLatencyPort = p;
            else {
               mInteractive = true;
               ++mNumOutputControls;
            }
         }
      }
   }
   return true;
}

bool LadspaEffect::InitializeControls(LadspaEffectSettings &settings) const
{
   auto &controls = settings.controls;
   // (Re-)initialize with right-sized vector
   std::vector<float>(mData->PortCount).swap(controls);

   for (unsigned long p = 0; p < mData->PortCount; ++p) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_CONTROL(d) && LADSPA_IS_PORT_INPUT(d))
         // Determine the port's default value
         controls[p] = InputControlPortDefaultValue(
            mData->PortRangeHints[p], mProjectRate);
      else
         controls[p] = 0;
   }
   return true;
}

std::shared_ptr<EffectInstance> LadspaEffect::MakeInstance() const
{
   return std::make_shared<LadspaInstance>(*this, mData,
      mInputPorts, mOutputPorts, mAudioIns, mAudioOuts,
      mLatencyPort);
}

int LadspaEffect::ShowClientInterface(const EffectPlugin &,
   wxWindow &parent, wxDialog &dialog,
   EffectEditor *, bool forceModal) const
{
   dialog.Layout();
   dialog.Fit();
   dialog.SetMinSize(dialog.GetSize());

   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal)
   {
      dialog.Show();
      return 0;
   }

   return dialog.ShowModal();
}

bool LadspaEffect::SaveSettings(
   const EffectSettings &settings, CommandParameters & parms) const
{
   const auto &controls = GetSettings(settings).controls;
   for (unsigned long p = 0; p < mData->PortCount; p++) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_CONTROL(d) && LADSPA_IS_PORT_INPUT(d))
         if (!parms.Write(LAT1CTOWX(mData->PortNames[p]), controls[p]))
            return false;
   }
   return true;
}

bool LadspaEffect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   auto &controls = GetSettings(settings).controls;
   for (unsigned long p = 0; p < mData->PortCount; p++) {
      LADSPA_PortDescriptor descriptor = mData->PortDescriptors[p];

      if (LADSPA_IS_PORT_CONTROL(descriptor) &&
          LADSPA_IS_PORT_INPUT(descriptor)) {
         wxString labelText = LAT1CTOWX(mData->PortNames[p]);
         double d = 0.0;
         if (!parms.Read(labelText, &d))
            return false;
         controls[p] = d;
      }
   }
   return true;
}

OptionalMessage LadspaEffect::LoadUserPreset(
   const RegistryPath & name, EffectSettings &settings) const
{
   return LoadParameters(name, settings);
}

bool LadspaEffect::SaveUserPreset(
   const RegistryPath & name, const EffectSettings &settings) const
{
   return SaveParameters(name, settings);
}

RegistryPaths LadspaEffect::GetFactoryPresets() const
{
   return {};
}

OptionalMessage LadspaEffect::LoadFactoryPreset(int, EffectSettings &) const
{
   return { nullptr };
}

std::unique_ptr<EffectEditor> LadspaEffect::MakeEditor(ShuttleGui & S,
   EffectInstance &instance, EffectSettingsAccess &access,
   const EffectOutputs *pOutputs) const
{
   auto pValues = static_cast<const LadspaEffectOutputs *>(pOutputs);
   auto result = std::make_unique<LadspaEditor>(*this,
      dynamic_cast<LadspaInstance&>(instance),
      mNumInputControls, mNumOutputControls, access, mProjectRate,
      GetType(), pValues);
   result->PopulateUI(S);
   return result;
}

bool LadspaEffect::CanExportPresets() const
{
   return false;
}

void LadspaEffect::ExportPresets(
   const EffectPlugin &, const EffectSettings &) const
{
}

OptionalMessage LadspaEffect::ImportPresets(
   const EffectPlugin &, EffectSettings &) const
{
   return { nullptr };
}

bool LadspaEffect::HasOptions() const
{
   return true;
}

void LadspaEffect::ShowOptions(const EffectPlugin &) const
{
   LadspaEffectOptionsDialog{ *this }.ShowModal();
}

// ============================================================================
// LadspaEffect Implementation
// ============================================================================

bool LadspaEffect::Load()
{
   if (mLib.IsLoaded())
   {
      return true;
   }

   wxFileName ff = mPath;
   wxString envpath;
   bool hadpath = wxGetEnv(wxT("PATH"), &envpath);
   wxSetEnv(wxT("PATH"), ff.GetPath() + wxFILE_SEP_PATH + envpath);
   wxString saveOldCWD = ff.GetCwd();
   ff.SetCwd();

   LADSPA_Descriptor_Function mainFn = NULL;

   if (mLib.Load(mPath, wxDL_NOW))
   {
      wxLogNull logNo;

      mainFn = (LADSPA_Descriptor_Function) mLib.GetSymbol(wxT("ladspa_descriptor"));
      if (mainFn)
      {
         mData = mainFn(mIndex);
         return true;
      }
   }

   if (mLib.IsLoaded())
   {
      mLib.Unload();
   }

   wxSetWorkingDirectory(saveOldCWD);
   hadpath ? wxSetEnv(wxT("PATH"), envpath) : wxUnsetEnv(wxT("PATH"));

   return false;
}

void LadspaEffect::Unload()
{
   if (mLib.IsLoaded())
   {
      mLib.Unload();
   }
}

OptionalMessage LadspaEffect::LoadParameters(
   const RegistryPath & group, EffectSettings &settings) const
{
   wxString parms;
   if (!GetConfig(*this, PluginSettings::Private, group, wxT("Parameters"),
      parms, wxEmptyString))
   {
      return {};
   }

   CommandParameters eap;
   if (!eap.SetParameters(parms))
   {
      return {};
   }

   if (!LoadSettings(eap, settings))
      return {};
   return { nullptr };
}

bool LadspaEffect::SaveParameters(
   const RegistryPath & group, const EffectSettings &settings) const
{
   CommandParameters eap;
   if (!SaveSettings(settings, eap))
   {
      return false;
   }

   wxString parms;
   if (!eap.GetParameters(parms))
   {
      return false;
   }

   return SetConfig(*this, PluginSettings::Private,
      group, wxT("Parameters"), parms);
}
