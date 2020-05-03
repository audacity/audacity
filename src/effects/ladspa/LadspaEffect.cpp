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
#include "SampleCount.h"
#include "ConfigInterface.h"

#include <float.h>
#include <thread>

#if !defined(__WXMSW__)
#include <dlfcn.h>

#ifndef RTLD_DEEPBIND
#define RTLD_DEEPBIND 0
#endif
#endif

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/wxprec.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/dcclient.h>
#include <wx/filename.h>
#include <wx/log.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>
#include <wx/version.h>

#include "AudacityException.h"
#include "FileNames.h"
#include "../../ShuttleGui.h"
#include "../../widgets/NumericTextCtrl.h"
#include "../../widgets/valnum.h"
#include "../../widgets/wxPanelWrapper.h"
#include "ModuleManager.h"

#if wxUSE_ACCESSIBILITY
#include "../../widgets/WindowAccessible.h"
#endif

// ============================================================================
// List of effects that ship with Audacity.  These will be autoregistered.
// ============================================================================
const static wxChar *kShippedEffects[] =
{
   wxT("sc4_1882.dll"),
};

// ============================================================================
// Tolerance to be used when comparing control values.
constexpr float ControlValueTolerance = 1.0e-5f;
// ============================================================================
 
// ============================================================================
// Module registration entry point
//
// This is the symbol that Audacity looks for when the module is built as a
// dynamic library.
//
// When the module is builtin to Audacity, we use the same function, but it is
// declared static so as not to clash with other builtin modules.
// ============================================================================
DECLARE_PROVIDER_ENTRY(AudacityModule)
{
   // Create and register the importer
   // Trust the module manager not to leak this
   return std::make_unique<LadspaEffectsModule>();
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_PROVIDER(LadspaBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

LadspaEffectsModule::LadspaEffectsModule()
{
}

LadspaEffectsModule::~LadspaEffectsModule()
{
}

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

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath LadspaEffectsModule::GetPath() const
{
   return {};
}

ComponentInterfaceSymbol LadspaEffectsModule::GetSymbol() const
{
   /* i18n-hint: abbreviates "Linux Audio Developer's Simple Plugin API"
      (Application programming interface)
    */
   return XO("LADSPA Effects");
}

VendorSymbol LadspaEffectsModule::GetVendor() const
{
   return XO("The Audacity Team");
}

wxString LadspaEffectsModule::GetVersion() const
{
   // This "may" be different if this were to be maintained as a separate DLL
   return LADSPAEFFECTS_VERSION;
}

TranslatableString LadspaEffectsModule::GetDescription() const
{
   return XO("Provides LADSPA Effects");
}

// ============================================================================
// PluginProvider implementation
// ============================================================================

bool LadspaEffectsModule::Initialize()
{
   // Nothing to do here
   return true;
}

void LadspaEffectsModule::Terminate()
{
   // Nothing to do here
   return;
}

EffectFamilySymbol LadspaEffectsModule::GetOptionalFamilySymbol()
{
#if USE_LADSPA
   return LADSPAEFFECTS_FAMILY;
#else
   return {};
#endif
}

const FileExtensions &LadspaEffectsModule::GetFileExtensions()
{
   static FileExtensions result{{

#ifdef __WXMSW__

      _T("dll")

#else

      _T("so")

   #ifdef __WXMAC__
   // Is it correct that these are candidate plug-in files too for macOs?
      , _T("dylib")
   #endif

#endif

   }};
   return result;
}

FilePath LadspaEffectsModule::InstallPath()
{
   // To do: better choice
   return FileNames::PlugInDir();
}

void LadspaEffectsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   // Autoregister effects that we "think" are ones that have been shipped with
   // Audacity.  A little simplistic, but it should suffice for now.
   auto pathList = GetSearchPaths();
   FilePaths files;
   TranslatableString ignoredErrMsg;

   for (int i = 0; i < (int)WXSIZEOF(kShippedEffects); i++)
   {
      files.clear();
      pm.FindFilesInPathList(kShippedEffects[i], pathList, files);
      for (size_t j = 0, cnt = files.size(); j < cnt; j++)
      {
         if (!pm.IsPluginRegistered(files[j]))
         {
            // No checking for error ?
            DiscoverPluginsAtPath(files[j], ignoredErrMsg,
               PluginManagerInterface::DefaultRegistrationCallback);
         }
      }
   }
}

PluginPaths LadspaEffectsModule::FindModulePaths(PluginManagerInterface & pm)
{
   auto pathList = GetSearchPaths();
   FilePaths files;

#if defined(__WXMAC__)

   // Recursively scan for all shared objects
   pm.FindFilesInPathList(wxT("*.so"), pathList, files, true);

#elif defined(__WXMSW__)

   // Recursively scan for all DLLs
   pm.FindFilesInPathList(wxT("*.dll"), pathList, files, true);

#else

   // Recursively scan for all shared objects
   pm.FindFilesInPathList(wxT("*.so"), pathList, files, true);

#endif

   return { files.begin(), files.end() };
}

unsigned LadspaEffectsModule::DiscoverPluginsAtPath(
   const PluginPath & path, TranslatableString &errMsg,
   const RegistrationCallback &callback)
{
   errMsg = {};
   // Since we now have builtin VST support, ignore the VST bridge as it
   // causes duplicate menu entries to appear.
   wxFileName ff(path);
   if (ff.GetName().CmpNoCase(wxT("vst-bridge")) == 0) {
      errMsg = XO("Audacity no longer uses vst-bridge");
      return 0;
   }

   // As a courtesy to some plug-ins that might be bridges to
   // open other plug-ins, we set the current working
   // directory to be the plug-in's directory.
   wxString envpath;
   bool hadpath = wxGetEnv(wxT("PATH"), &envpath);
   wxSetEnv(wxT("PATH"), ff.GetPath() + wxFILE_SEP_PATH + envpath);
   wxString saveOldCWD = ff.GetCwd();
   ff.SetCwd();

   int index = 0;
   int nLoaded = 0;
   LADSPA_Descriptor_Function mainFn = NULL;

#if defined(__WXMSW__)
   wxDynamicLibrary lib;
   if (lib.Load(path, wxDL_NOW))
#else
   void *lib = dlopen((const char *)path.ToUTF8(), RTLD_NOW | RTLD_LOCAL | RTLD_DEEPBIND);
   if (lib)
#endif
   {

#if defined(__WXMSW__)
      wxLogNull logNo;

      mainFn = (LADSPA_Descriptor_Function) lib.GetSymbol(wxT("ladspa_descriptor"));
#else
      mainFn = (LADSPA_Descriptor_Function) dlsym(lib, "ladspa_descriptor");
#endif

      if (mainFn) {
         const LADSPA_Descriptor *data;

         for (data = mainFn(index); data; data = mainFn(++index)) {
            LadspaEffect effect(path, index);
            if (effect.InitializePlugin()) {
               ++nLoaded;
               if (callback)
                  callback( this, &effect );
            }
            else
               errMsg = XO("Could not load the library");
         }
      }
   }
   else
      errMsg = XO("Could not load the library");

#if defined(__WXMSW__)
   if (lib.IsLoaded()) {
      // PRL:  I suspect Bug1257 -- Crash when enabling Amplio2 -- is the fault of a timing-
      // dependent multi-threading bug in the Amplio2 library itself, in case the unload of the .dll
      // comes too soon after the load.  I saw the bug in Release builds but not Debug.
      // A sleep of even 1 ms was enough to fix the problem for me, but let's be even more generous.
      using namespace std::chrono;
      std::this_thread::sleep_for(10ms);
      lib.Unload();
   }
#else
   if (lib) {
      dlclose(lib);
   }
#endif

   wxSetWorkingDirectory(saveOldCWD);
   hadpath ? wxSetEnv(wxT("PATH"), envpath) : wxUnsetEnv(wxT("PATH"));

   return nLoaded;
}

std::unique_ptr<ComponentInterface>
LadspaEffectsModule::LoadPlugin(const PluginPath & path)
{
   // Acquires a resource for the application.
   // For us, the path is two words.
   // 1)  The library's path
   // 2)  The LADSPA descriptor index
   long index;
   wxString realPath = path.BeforeFirst(wxT(';'));
   path.AfterFirst(wxT(';')).ToLong(&index);
   auto result = std::make_unique<LadspaEffect>(realPath, (int)index);
   result->FullyInitializePlugin();
   return result;
}

bool LadspaEffectsModule::CheckPluginExist(const PluginPath& path) const
{
   const auto realPath = path.BeforeFirst(wxT(';'));
   return wxFileName::FileExists(realPath);
}

FilePaths LadspaEffectsModule::GetSearchPaths()
{
   FilePaths pathList;
   wxString pathVar;

   // Check for the LADSPA_PATH environment variable
   pathVar = wxString::FromUTF8(getenv("LADSPA_PATH"));
   if (!pathVar.empty())
   {
      wxStringTokenizer tok(pathVar, wxPATH_SEP);
      while (tok.HasMoreTokens())
      {
         pathList.push_back(tok.GetNextToken());
      }
   }

#if defined(__WXMAC__)
#define LADSPAPATH wxT("/Library/Audio/Plug-Ins/LADSPA")

   // Look in ~/Library/Audio/Plug-Ins/LADSPA and /Library/Audio/Plug-Ins/LADSPA
   pathList.push_back(wxGetHomeDir() + wxFILE_SEP_PATH + LADSPAPATH);
   pathList.push_back(LADSPAPATH);

#elif defined(__WXMSW__)

   // No special paths...probably should look in %CommonProgramFiles%\LADSPA

#else

   pathList.push_back(wxGetHomeDir() + wxFILE_SEP_PATH + wxT(".ladspa"));
#if defined(__LP64__)
   pathList.push_back(wxT("/usr/local/lib64/ladspa"));
   pathList.push_back(wxT("/usr/lib64/ladspa"));
#endif
   pathList.push_back(wxT("/usr/local/lib/ladspa"));
   pathList.push_back(wxT("/usr/lib/ladspa"));
   pathList.push_back(wxT(LIBDIR) wxT("/ladspa"));

#endif

   return pathList;
}

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffectOptionsDialog
//
///////////////////////////////////////////////////////////////////////////////

class LadspaEffectOptionsDialog final : public wxDialogWrapper
{
public:
   LadspaEffectOptionsDialog(
      wxWindow * parent, EffectDefinitionInterface &effect, bool &var);
   virtual ~LadspaEffectOptionsDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   EffectDefinitionInterface &mEffect;
   bool &mUseLatency;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(LadspaEffectOptionsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, LadspaEffectOptionsDialog::OnOk)
END_EVENT_TABLE()

LadspaEffectOptionsDialog::LadspaEffectOptionsDialog(
   wxWindow * parent, EffectDefinitionInterface &effect, bool &var)
: wxDialogWrapper(parent, wxID_ANY, XO("LADSPA Effect Options"))
, mEffect{ effect }
, mUseLatency{ var }
{
   mUseLatency = LadspaEffect::LoadUseLatency(effect);

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

LadspaEffectOptionsDialog::~LadspaEffectOptionsDialog()
{
}

static const wchar_t *OptionsKey = L"Options";
static const wchar_t *UseLatencyKey = L"UseLatency";

bool LadspaEffect::LoadUseLatency(const EffectDefinitionInterface &effect)
{
   bool result{};
   GetConfig(effect, PluginSettings::Shared,
      OptionsKey, UseLatencyKey, result, true /* default value */);
   return result;
}

bool LadspaEffect::SaveUseLatency(
   const EffectDefinitionInterface &effect, bool value)
{
   return SetConfig(
      effect, PluginSettings::Shared, OptionsKey, UseLatencyKey, value);
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

   LadspaEffect::SaveUseLatency(mEffect, mUseLatency);

   EndModal(wxID_OK);
}

enum
{
   ID_Duration = 20000,
   ID_Toggles = 21000,
   ID_Sliders = 22000,
   ID_Texts = 23000,
};

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffectMeter
//
///////////////////////////////////////////////////////////////////////////////

class LadspaEffectMeter final : public wxWindow
{
public:
   LadspaEffectMeter(wxWindow *parent, const float & val, float min, float max);

   void Disconnect()
   {
      // Stop using mVal, it might be dangling now
      mConnected = false;
   }

   virtual ~LadspaEffectMeter();

private:
   void OnErase(wxEraseEvent & evt);
   void OnPaint(wxPaintEvent & evt);
   void OnIdle(wxIdleEvent & evt);
   void OnSize(wxSizeEvent & evt);

private:
   bool mConnected{ true };
   const float & mVal;
   float mMin;
   float mMax;
   float mLastValue;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(LadspaEffectMeter, wxWindow)
   EVT_IDLE(LadspaEffectMeter::OnIdle)
   EVT_ERASE_BACKGROUND(LadspaEffectMeter::OnErase)
   EVT_PAINT(LadspaEffectMeter::OnPaint)
   EVT_SIZE(LadspaEffectMeter::OnSize)
END_EVENT_TABLE()

LadspaEffectMeter::LadspaEffectMeter(wxWindow *parent, const float & val, float min, float max)
:  wxWindow{ parent, wxID_ANY, wxDefaultPosition, wxDefaultSize,
      wxSIMPLE_BORDER },
   mVal(val)
{
   mMin = min;
   mMax = max;
   mLastValue = -mVal;
   SetBackgroundColour(*wxWHITE);
   SetMinSize({ 20, 20 });
}

LadspaEffectMeter::~LadspaEffectMeter()
{
}

void LadspaEffectMeter::OnIdle(wxIdleEvent &evt)
{
   evt.Skip();
   if (!mConnected)
      return;
   if (mLastValue != mVal)
      Refresh(false);
}

void LadspaEffectMeter::OnErase(wxEraseEvent & WXUNUSED(evt))
{
   // Just ignore it to prevent flashing
}

void LadspaEffectMeter::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   if (!mConnected)
      return;

   wxPaintDC dc(this);

   // Cache some metrics
   wxRect r = GetClientRect();
   wxCoord x = r.GetLeft();
   wxCoord y = r.GetTop();
   wxCoord w = r.GetWidth();
   wxCoord h = r.GetHeight();

   // These use unscaled value, min, and max
   float val = mVal;
   if (val > mMax)
      val = mMax;
   if (val < mMin)
      val = mMin;
   val -= mMin;

   // Setup for erasing the background
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(wxColour(100, 100, 220));
   dc.Clear();
   dc.DrawRectangle(x, y, (w * (val / fabs(mMax - mMin))), h);

   mLastValue = mVal;
}

void LadspaEffectMeter::OnSize(wxSizeEvent & WXUNUSED(evt))
{
   Refresh(false);
}

LadspaEffectOutputs::~LadspaEffectOutputs() = default;

auto LadspaEffectOutputs::Clone() const -> std::unique_ptr<EffectOutputs>
{
   return std::make_unique<LadspaEffectOutputs>(*this);
}

void LadspaEffectOutputs::Assign(EffectOutputs &&src)
{
   // Don't really need to modify src
   const auto &srcValues = static_cast<LadspaEffectOutputs&>(src).controls;
   auto &dstValues = controls;
   assert(srcValues.size() == dstValues.size());
   copy(srcValues.begin(), srcValues.end(), dstValues.data());
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
      ? RealtimeSince::Since_3_2
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
            mNumInputControls++;
         }
         else if (LADSPA_IS_PORT_OUTPUT(d)) {
            // LADSPA effects have a convention of providing latency on an output
            // control port whose name is "latency".
            if (strcmp(mData->PortNames[p], "latency") == 0)
               mLatencyPort = p;
            else {
               mInteractive = true;
               mNumOutputControls++;
            }
         }
      }
   }
   return true;
}

bool LadspaEffect::FullyInitializePlugin()
{
   if (!InitializePlugin())
      return false;

   // Reading these values from the config file can't be done in the PluginHost
   // process but isn't needed only for plugin discovery.

   mUseLatency = LadspaEffect::LoadUseLatency(*this);
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

struct LadspaEffect::Instance
   : PerTrackEffect::Instance
   , EffectInstanceWithBlockSize
{
   using PerTrackEffect::Instance::Instance;
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      ChannelNames chanMap) override;
   bool ProcessFinalize() noexcept override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   SampleCount GetLatency(const EffectSettings &settings, double sampleRate)
      const override;

   bool RealtimeInitialize(EffectSettings &settings, double sampleRate)
      override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      EffectOutputs *pOutputs, unsigned numChannels, float sampleRate)
   override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() override;
   bool RealtimeProcessStart(MessagePackage &package) override;
   size_t RealtimeProcess(size_t group, EffectSettings &settings,
      const float *const *inBuf, float *const *outBuf, size_t numSamples)
   override;
   bool RealtimeProcessEnd(EffectSettings &settings) noexcept override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   const LadspaEffect &GetEffect() const
      { return static_cast<const LadspaEffect &>(mProcessor); }

   bool mReady{ false };
   LADSPA_Handle mMaster{};

   // Realtime processing
   std::vector<LADSPA_Handle> mSlaves;
};

std::shared_ptr<EffectInstance> LadspaEffect::MakeInstance() const
{
   return std::make_shared<Instance>(*this);
}

auto LadspaEffect::Instance::GetLatency(
   const EffectSettings &settings, double) const -> SampleCount
{
   auto &effect = GetEffect();
   auto &controls = GetSettings(settings).controls;
   if (effect.mUseLatency && effect.mLatencyPort >= 0)
      return controls[effect.mLatencyPort];
   return 0;
}

bool LadspaEffect::Instance::ProcessInitialize(
   EffectSettings &settings, double sampleRate, ChannelNames)
{
   /* Instantiate the plugin */
   if (!mReady) {
      auto &effect = GetEffect();
      auto &ladspaSettings = GetSettings(settings);
      // Destructive effect processing doesn't need output ports
      mMaster = effect.InitInstance(sampleRate, ladspaSettings, nullptr);
      if (!mMaster)
         return false;
      mReady = true;
   }
   return true;
}

bool LadspaEffect::Instance::ProcessFinalize() noexcept
{
return GuardedCall<bool>([&]{
   if (mReady) {
      mReady = false;
      GetEffect().FreeInstance(mMaster);
      mMaster = nullptr;
   }

   return true;
});
}

size_t LadspaEffect::Instance::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   auto &effect = GetEffect();
   for (unsigned i = 0; i < effect.mAudioIns; ++i)
      effect.mData->connect_port(mMaster, effect.mInputPorts[i],
         const_cast<float*>(inBlock[i]));

   for (unsigned i = 0; i < effect.mAudioOuts; ++i)
      effect.mData->connect_port(mMaster, effect.mOutputPorts[i], outBlock[i]);

   effect.mData->run(mMaster, blockLen);
   return blockLen;
}

bool LadspaEffect::Instance::RealtimeInitialize(EffectSettings &, double)
{
   return true;
}

bool LadspaEffect::Instance::RealtimeAddProcessor(
   EffectSettings &settings, EffectOutputs *pOutputs, unsigned, float sampleRate)
{
   auto &effect = GetEffect();
   auto &ladspaSettings = GetSettings(settings);
   // Connect to outputs only if this is the first processor for the track.
   // (What's right when a mono effect is on a stereo channel?  Unclear, but
   // this definitely causes connection with the first channel.)
   auto pLadspaOutputs = mSlaves.empty()
      ? static_cast<LadspaEffectOutputs *>(pOutputs) : nullptr;
   auto slave = effect.InitInstance(sampleRate, ladspaSettings, pLadspaOutputs);
   if (!slave)
      return false;
   mSlaves.push_back(slave);
   return true;
}

unsigned LadspaEffect::Instance::GetAudioOutCount() const
{
   return GetEffect().mAudioOuts;
}

unsigned LadspaEffect::Instance::GetAudioInCount() const
{
   return GetEffect().mAudioIns;
}

bool LadspaEffect::Instance::RealtimeFinalize(EffectSettings &) noexcept
{
return GuardedCall<bool>([&]{
   auto &effect = GetEffect();
   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; ++i)
      effect.FreeInstance(mSlaves[i]);
   mSlaves.clear();

   return true;
});
}

bool LadspaEffect::Instance::RealtimeSuspend()
{
   if (auto fn = GetEffect().mData->deactivate)
      for (auto &slave : mSlaves)
         fn(slave);
   return true;
}

bool LadspaEffect::Instance::RealtimeResume()
{
   if (auto fn = GetEffect().mData->activate)
      for (auto &slave : mSlaves)
         fn(slave);
   return true;
}

bool LadspaEffect::Instance::RealtimeProcessStart(MessagePackage &)
{
   return true;
}

size_t LadspaEffect::Instance::RealtimeProcess(size_t group, EffectSettings &,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   if (group >= mSlaves.size())
      return 0;

   auto &effect = GetEffect();
   for (unsigned i = 0; i < effect.mAudioIns; ++i)
      effect.mData->connect_port(mSlaves[group], effect.mInputPorts[i],
         const_cast<float*>(inbuf[i]));

   for (unsigned i = 0; i < effect.mAudioOuts; ++i)
      effect.mData->connect_port(
         mSlaves[group], effect.mOutputPorts[i], outbuf[i]);

   effect.mData->run(mSlaves[group], numSamples);

   return numSamples;
}

bool LadspaEffect::Instance::RealtimeProcessEnd(EffectSettings &) noexcept
{
   return true;
}

int LadspaEffect::ShowClientInterface(wxWindow &parent, wxDialog &dialog,
   EffectUIValidator *, bool forceModal)
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

// ============================================================================
// EffectUIClientInterface Implementation
// ============================================================================

struct LadspaEffect::Validator : EffectUIValidator {
   Validator(EffectUIClientInterface &effect,
      EffectSettingsAccess &access, double sampleRate, EffectType type,
      const LadspaEffectOutputs *pOutputs)
      : EffectUIValidator{ effect, access }
      , mSampleRate{ sampleRate }
      , mType{ type }
      // Copy settings
      , mSettings{ GetSettings(access.Get()) }
      , mpOutputs{ pOutputs }
   {}

   bool UpdateUI() override;
   bool ValidateUI() override;
   void Disconnect() override;

   void PopulateUI(ShuttleGui &S);

   void OnCheckBox(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);
   void OnTextCtrl(wxCommandEvent & evt);
   void RefreshControls();

   void UpdateControl(int index, float value, float epsilon);
   void UpdateControls(const LadspaEffectSettings& src);

   const LadspaEffect &GetEffect()
      { return static_cast<const LadspaEffect &>(mEffect); }

   const double mSampleRate;
   const EffectType mType;
   LadspaEffectSettings mSettings;
   const LadspaEffectOutputs *const mpOutputs;

   NumericTextCtrl *mDuration{};
   wxWeakRef<wxDialog> mDialog;
   wxWindow *mParent{};
   ArrayOf<wxSlider*> mSliders;
   ArrayOf<wxTextCtrl*> mFields;
   ArrayOf<wxStaticText*> mLabels;
   ArrayOf<wxCheckBox*> mToggles;
   std::vector<LadspaEffectMeter *> mMeters;
};

bool LadspaEffect::Validator::UpdateUI()
{
   RefreshControls();
   return true;
}

void LadspaEffect::Validator::PopulateUI(ShuttleGui &S)
{
   auto &effect = GetEffect();
   auto &controls = mSettings.controls;
   auto parent = S.GetParent();

   mParent = parent;

   const auto &data = *effect.mData;
   mToggles.reinit( data.PortCount );
   mSliders.reinit( data.PortCount );
   mFields.reinit( data.PortCount, true);
   mLabels.reinit( data.PortCount );
   mMeters.resize( data.PortCount );

   wxASSERT(mParent); // To justify safenew
   wxScrolledWindow *const w = safenew wxScrolledWindow(mParent,
      wxID_ANY,
      wxDefaultPosition,
      wxDefaultSize,
      wxVSCROLL | wxTAB_TRAVERSAL);

   {
      auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      w->SetScrollRate(0, 20);

      // This fools NVDA into not saying "Panel" when the dialog gets focus
      w->SetName(wxT("\a"));
      w->SetLabel(wxT("\a"));

      mainSizer->Add(w, 1, wxEXPAND);
      mParent->SetSizer(mainSizer.release());
   }

   wxSizer *marginSizer;
   {
      auto uMarginSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      marginSizer = uMarginSizer.get();

      // Make user-adjustible input controls
      if (effect.mNumInputControls) {
         auto paramSizer = std::make_unique<wxStaticBoxSizer>(wxVERTICAL, w, _("Effect Settings"));

         auto gridSizer = std::make_unique<wxFlexGridSizer>(5, 0, 0);
         gridSizer->AddGrowableCol(3);

         wxControl *item;

         // Add the duration control for generators
         if (mType == EffectTypeGenerate) {
            item = safenew wxStaticText(w, 0, _("Duration:"));
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
            auto &extra = mAccess.Get().extra;
            mDuration = safenew
               NumericTextCtrl(w, ID_Duration,
                  NumericConverter::TIME,
                  extra.GetDurationFormat(),
                  extra.GetDuration(),
                  mSampleRate,
                  NumericTextCtrl::Options{}
                     .AutoPos(true));
            mDuration->SetName( XO("Duration") );
            gridSizer->Add(mDuration, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
            gridSizer->Add(1, 1, 0);
            gridSizer->Add(1, 1, 0);
            gridSizer->Add(1, 1, 0);
         }

         for (unsigned long p = 0; p < data.PortCount; ++p) {
            LADSPA_PortDescriptor d = data.PortDescriptors[p];
            if (LADSPA_IS_PORT_AUDIO(d) || LADSPA_IS_PORT_OUTPUT(d))
            {
               continue;
            }

            wxString labelText = LAT1CTOWX(data.PortNames[p]);
            item = safenew wxStaticText(w, 0, wxString::Format(_("%s:"), labelText));
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

            wxString fieldText;
            LADSPA_PortRangeHint hint = data.PortRangeHints[p];

            if (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor)) {
               mToggles[p] = safenew wxCheckBox(w, ID_Toggles + p, wxT(""));
               mToggles[p]->SetName(labelText);
               mToggles[p]->SetValue(controls[p] > 0);
               BindTo(*mToggles[p],
                  wxEVT_COMMAND_CHECKBOX_CLICKED, &Validator::OnCheckBox);
               gridSizer->Add(mToggles[p], 0, wxALL, 5);

               gridSizer->Add(1, 1, 0);
               gridSizer->Add(1, 1, 0);
               gridSizer->Add(1, 1, 0);
               continue;
            }

            wxString bound;
            float lower = -FLT_MAX;
            float upper = FLT_MAX;
            bool haslo = false;
            bool hashi = false;
            bool forceint = false;

            if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor)) {
               lower = hint.LowerBound;
               haslo = true;
            }

            if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor)) {
               upper = hint.UpperBound;
               hashi = true;
            }

            if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
               lower *= mSampleRate;
               upper *= mSampleRate;
               forceint = true;
            }

            // Limit to the UI precision
            lower = ceilf(lower * 1000000.0) / 1000000.0;
            upper = floorf(upper * 1000000.0) / 1000000.0;
            controls[p] = roundf(controls[p] * 1000000.0) / 1000000.0;

            if (haslo && controls[p] < lower)
               controls[p] = lower;

            if (hashi && controls[p] > upper)
               controls[p] = upper;

            // Don't specify a value at creation time.  This prevents unwanted events
            // being sent to the OnTextCtrl() handler before the associated slider
            // has been created.
            mFields[p] = safenew wxTextCtrl(w, ID_Texts + p);
            mFields[p]->SetName(labelText);
            BindTo(*mFields[p],
               wxEVT_COMMAND_TEXT_UPDATED, &Validator::OnTextCtrl);
            gridSizer->Add(mFields[p], 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

            wxString str;
            if (haslo) {
               if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
               {
                  str.Printf(wxT("%d"), (int)(lower + 0.5));
               }
               else
               {
                  str = Internat::ToDisplayString(lower);
               }
               item = safenew wxStaticText(w, 0, str);
               gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
            }
            else
               gridSizer->Add(1, 1, 0);

            mSliders[p] = safenew wxSliderWrapper(w, ID_Sliders + p,
               0, 0, 1000,
               wxDefaultPosition,
               wxSize(200, -1));
#if wxUSE_ACCESSIBILITY
            // so that name can be set on a standard control
            mSliders[p]->SetAccessible(safenew WindowAccessible(mSliders[p]));
#endif
            mSliders[p]->SetName(labelText);
            BindTo(*mSliders[p],
               wxEVT_COMMAND_SLIDER_UPDATED, &Validator::OnSlider);
            gridSizer->Add(mSliders[p], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);

            if (hashi) {
               if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
                  str.Printf(wxT("%d"), (int)(upper + 0.5));
               else
                  str = Internat::ToDisplayString(upper);
               item = safenew wxStaticText(w, 0, str);
               gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
            }
            else
               gridSizer->Add(1, 1, 0);

            if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint) {
               fieldText.Printf(wxT("%d"), (int)(controls[p] + 0.5));

               IntegerValidator<float> vld(&controls[p]);
               vld.SetRange(haslo ? lower : INT_MIN,
                  hashi ? upper : INT_MAX);
               mFields[p]->SetValidator(vld);
            }
            else {
               fieldText = Internat::ToDisplayString(controls[p]);

               // > 12 decimal places can cause rounding errors in display.
               FloatingPointValidator<float> vld(6, &controls[p]);
               vld.SetRange(lower, upper);

               // Set number of decimal places
               if (upper - lower < 10.0)
                  vld.SetStyle(NumValidatorStyle::THREE_TRAILING_ZEROES);
               else if (upper - lower < 100.0)
                  vld.SetStyle(NumValidatorStyle::TWO_TRAILING_ZEROES);
               else
                  vld.SetStyle(NumValidatorStyle::ONE_TRAILING_ZERO);

               mFields[p]->SetValidator(vld);
            }

            // Set the textctrl value.  This will trigger an event so OnTextCtrl()
            // can update the slider.
            mFields[p]->SetValue(fieldText);
         }

         paramSizer->Add(gridSizer.release(), 0, wxEXPAND | wxALL, 5);
         marginSizer->Add(paramSizer.release(), 0, wxEXPAND | wxALL, 5);
      }

      // Make output meters
      if (effect.mNumOutputControls > 0) {
         auto paramSizer = std::make_unique<wxStaticBoxSizer>(wxVERTICAL, w, _("Effect Output"));

         auto gridSizer = std::make_unique<wxFlexGridSizer>(2, 0, 0);
         gridSizer->AddGrowableCol(1);

         wxControl *item;

         for (unsigned long p = 0; p < data.PortCount; ++p) {
            LADSPA_PortDescriptor d = data.PortDescriptors[p];
            if (LADSPA_IS_PORT_AUDIO(d) || LADSPA_IS_PORT_INPUT(d))
               continue;

            wxString labelText = LAT1CTOWX(data.PortNames[p]);
            item = safenew wxStaticText(
               w, 0, wxString::Format(_("%s:"), labelText));
            gridSizer->Add(
               item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

            //LADSPA_PortRangeHint hint = data.PortRangeHints[p];

            wxString bound;
            float lower = 0.0;
            float upper = 1.0;

            // Limit to the UI precision
            lower = ceilf(lower * 1000000.0) / 1000000.0;
            upper = floorf(upper * 1000000.0) / 1000000.0;
            controls[p] = lower;

            // Capture const reference to output control value for later
            // display update
            static float sink;
            auto pOutput = mpOutputs ? &mpOutputs->controls[p] : &sink;
            mMeters[p] = safenew LadspaEffectMeter(
               w, *pOutput, lower, upper);
            mMeters[p]->SetLabel(labelText);    // for screen readers
            gridSizer->Add(mMeters[p], 1, wxEXPAND | wxALIGN_CENTER_VERTICAL | wxALL, 5);
         }

         paramSizer->Add(gridSizer.release(), 0, wxEXPAND | wxALL, 5);
         marginSizer->Add(paramSizer.release(), 0, wxEXPAND | wxALL, 5);
      }

      w->SetSizer(uMarginSizer.release());
   }

   w->Layout();

   // Try to give the window a sensible default/minimum size
   wxSize sz1 = marginSizer->GetMinSize();
   wxSize sz2 = mParent->GetMinSize();
   w->SetMinSize( { std::min(sz1.x, sz2.x), std::min(sz1.y, sz2.y) } );

   // And let the parent reduce to the NEW minimum if possible
   mParent->SetMinSize({ -1, -1 });
}

std::unique_ptr<EffectUIValidator>
LadspaEffect::PopulateOrExchange(ShuttleGui & S,
   EffectInstance &, EffectSettingsAccess &access,
   const EffectOutputs *pOutputs)
{
   auto pValues = static_cast<const LadspaEffectOutputs *>(pOutputs);
   auto result = std::make_unique<Validator>(*this, access, mProjectRate,
      GetType(), pValues);
   result->PopulateUI(S);
   return result;
}

bool LadspaEffect::Validator::ValidateUI()
{
   mAccess.ModifySettings([this](EffectSettings &settings){
      if (mType == EffectTypeGenerate)
         settings.extra.SetDuration(mDuration->GetValue());
      GetSettings(settings) = mSettings;
      return nullptr;
   });
   return true;
}

void LadspaEffect::Validator::Disconnect()
{
  for (auto &meter : mMeters)
     if (meter) {
        meter->Disconnect();
        meter = nullptr;
     }
}

bool LadspaEffect::CanExportPresets()
{
   return false;
}

void LadspaEffect::ExportPresets(const EffectSettings &) const
{
}

OptionalMessage LadspaEffect::ImportPresets(EffectSettings &)
{
   return { nullptr };
}

bool LadspaEffect::HasOptions()
{
   return true;
}

void LadspaEffect::ShowOptions()
{
   LadspaEffectOptionsDialog dlg(mUIParent, *this, mUseLatency);
   dlg.ShowModal();
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

LADSPA_Handle LadspaEffect::InitInstance(
   float sampleRate, LadspaEffectSettings &settings,
   LadspaEffectOutputs *pOutputs) const
{
   /* Instantiate the plugin */
   LADSPA_Handle handle = mData->instantiate(mData, sampleRate);
   if (!handle)
      return nullptr;

   auto &controls = settings.controls;
   for (unsigned long p = 0; p < mData->PortCount; ++p) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_CONTROL(d)) {
         if (LADSPA_IS_PORT_INPUT(d))
            mData->connect_port(handle, p, &controls[p]);
         else {
            static LADSPA_Data sink;
            mData->connect_port(handle, p,
               pOutputs ? &pOutputs->controls[p] : &sink);
         }
      }
   }
   if (mData->activate)
      mData->activate(handle);

   return handle;
}

void LadspaEffect::FreeInstance(LADSPA_Handle handle) const
{
   if (mData->deactivate)
   {
      mData->deactivate(handle);
   }

   mData->cleanup(handle);
}

void LadspaEffect::Validator::OnCheckBox(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Toggles;
   // 0.5 is a half of the interval
   UpdateControl(p, mToggles[p]->GetValue(), 0.5f);
   ValidateUI();
}

void LadspaEffect::Validator::OnSlider(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Sliders;

   float val;
   float lower = float(0.0);
   float upper = float(10.0);
   float range;
   bool forceint = false;

   LADSPA_PortRangeHint hint = GetEffect().mData->PortRangeHints[p];
   if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor))
      lower = hint.LowerBound;
   if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor))
      upper = hint.UpperBound;
   if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
      lower *= mSampleRate;
      upper *= mSampleRate;
      forceint = true;
   }

   range = upper - lower;
   val = (mSliders[p]->GetValue() / 1000.0) * range + lower;
   wxString str;
   if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
      str.Printf(wxT("%d"), (int)(val + 0.5));
   else
      str = Internat::ToDisplayString(val);

   mFields[p]->SetValue(str);

   UpdateControl(p, val, ControlValueTolerance);
   ValidateUI();
}

void LadspaEffect::Validator::OnTextCtrl(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Texts;

   float val;
   float lower = float(0.0);
   float upper = float(10.0);
   float range;

   val = Internat::CompatibleToDouble(mFields[p]->GetValue());

   LADSPA_PortRangeHint hint = GetEffect().mData->PortRangeHints[p];
   if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor))
      lower = hint.LowerBound;
   if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor))
      upper = hint.UpperBound;
   if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
      lower *= mSampleRate;
      upper *= mSampleRate;
   }
   range = upper - lower;
   if (val < lower)
      val = lower;
   if (val > upper)
      val = upper;

   mSliders[p]->SetValue((int)(((val-lower)/range) * 1000.0 + 0.5));

   UpdateControl(p, val, ControlValueTolerance);
   ValidateUI();
}

void LadspaEffect::Validator::RefreshControls()
{
   if (!mParent)
      return;

   // Copy from the dialog
   UpdateControls(GetSettings(mAccess.Get()));

   auto& controls = mSettings.controls;

   const auto &data = *GetEffect().mData;
   for (unsigned long p = 0; p < data.PortCount; ++p) {
      LADSPA_PortDescriptor d = data.PortDescriptors[p];
      if (!(LADSPA_IS_PORT_CONTROL(d)))
         continue;

      wxString fieldText;
      LADSPA_PortRangeHint hint = data.PortRangeHints[p];

      bool forceint = false;
      if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor))
         forceint = true;

      if (LADSPA_IS_PORT_OUTPUT(d))
         continue;

      if (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor)) {
         mToggles[p]->SetValue(controls[p] > 0);
         continue;
      }

      if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
         fieldText.Printf(wxT("%d"), (int)(controls[p] + 0.5));
      else
         fieldText = Internat::ToDisplayString(controls[p]);

      // Set the textctrl value.  This will trigger an event so OnTextCtrl()
      // can update the slider.
      mFields[p]->SetValue(fieldText);
   }
}

void LadspaEffect::Validator::UpdateControl(int index, float value, float epsilon)
{
   auto& controls = mSettings.controls;

   assert(index < static_cast<int>(controls.size()));

   if (std::abs(controls[index] - value) < epsilon)
      return;

   controls[index] = value;
   Publish({ size_t(index), value });
}

void LadspaEffect::Validator::UpdateControls(const LadspaEffectSettings& src)
{
   const auto& data = *GetEffect().mData;

   for (size_t portIndex = 0, portsCount = src.controls.size();
        portIndex < portsCount;
        ++portIndex)
   {
      LADSPA_PortDescriptor d = data.PortDescriptors[portIndex];
      
      if (!(LADSPA_IS_PORT_CONTROL(d)) || (LADSPA_IS_PORT_OUTPUT(d)))
         continue;

      LADSPA_PortRangeHint hint = GetEffect().mData->PortRangeHints[portIndex];

      const bool isIntValue = (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor)) ||
                              (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor)) ||
                              (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor));
      
      UpdateControl(
         portIndex, src.controls[portIndex],
         isIntValue ? 0.5f : ControlValueTolerance);
   }
}
