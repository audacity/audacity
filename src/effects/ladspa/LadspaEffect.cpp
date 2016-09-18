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


#include "../../Audacity.h"

#include "ladspa.h"

#include <float.h>

#include <wx/wxprec.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/dcbuffer.h>
#include <wx/dcclient.h>
#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/log.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>
#include <wx/version.h>

#include "LadspaEffect.h"       // This class's header file
#include "../../Internat.h"
#include "../../ShuttleGui.h"
#include "../../widgets/valnum.h"
#include "../../widgets/wxPanelWrapper.h"

// ============================================================================
// List of effects that ship with Audacity.  These will be autoregistered.
// ============================================================================
const static wxChar *kShippedEffects[] =
{
   wxT("sc4_1882.dll"),
};

// ============================================================================
// Module registration entry point
//
// This is the symbol that Audacity looks for when the module is built as a
// dynamic library.
//
// When the module is builtin to Audacity, we use the same function, but it is
// declared static so as not to clash with other builtin modules.
// ============================================================================
DECLARE_MODULE_ENTRY(AudacityModule)
{
   // Create and register the importer
   // Trust the module manager not to leak this
   return safenew LadspaEffectsModule(moduleManager, path);
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_MODULE(LadspaBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

LadspaEffectsModule::LadspaEffectsModule(ModuleManagerInterface *moduleManager,
                                     const wxString *path)
{
   mModMan = moduleManager;
   if (path)
   {
      mPath = *path;
   }
}

LadspaEffectsModule::~LadspaEffectsModule()
{
}

// ============================================================================
// IdentInterface implementation
// ============================================================================

wxString LadspaEffectsModule::GetPath()
{
   return mPath;
}

wxString LadspaEffectsModule::GetSymbol()
{
   return XO("LADSPA Effects");
}

wxString LadspaEffectsModule::GetName()
{
   return GetSymbol();
}

wxString LadspaEffectsModule::GetVendor()
{
   return XO("The Audacity Team");
}

wxString LadspaEffectsModule::GetVersion()
{
   // This "may" be different if this were to be maintained as a separate DLL
   return LADSPAEFFECTS_VERSION;
}

wxString LadspaEffectsModule::GetDescription()
{
   return XO("Provides LADSPA Effects");
}

// ============================================================================
// ModuleInterface implementation
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

bool LadspaEffectsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   // Autoregister effects that we "think" are ones that have been shipped with
   // Audacity.  A little simplistic, but it should suffice for now.
   wxArrayString pathList = GetSearchPaths();
   wxArrayString files;

   for (int i = 0; i < WXSIZEOF(kShippedEffects); i++)
   {
      files.Clear();
      pm.FindFilesInPathList(kShippedEffects[i], pathList, files);
      for (size_t j = 0, cnt = files.GetCount(); j < cnt; j++)
      {
         if (!pm.IsPluginRegistered(files[j]))
         {
            RegisterPlugin(pm, files[j]);
         }
      }
   }

   // We still want to be called during the normal registration process
   return false;
}

wxArrayString LadspaEffectsModule::FindPlugins(PluginManagerInterface & pm)
{
   wxArrayString pathList = GetSearchPaths();
   wxArrayString files;

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

   return files;
}

bool LadspaEffectsModule::RegisterPlugin(PluginManagerInterface & pm, const wxString & path)
{
   // Since we now have builtin VST support, ignore the VST bridge as it
   // causes duplicate menu entries to appear.
   wxFileName ff(path);
   if (ff.GetName().CmpNoCase(wxT("vst-bridge")) == 0) {
      return false;
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
   LADSPA_Descriptor_Function mainFn = NULL;
   wxDynamicLibrary lib;
   if (lib.Load(path, wxDL_NOW)) {
      wxLogNull logNo;

      mainFn = (LADSPA_Descriptor_Function) lib.GetSymbol(wxT("ladspa_descriptor"));
      if (mainFn) {
         const LADSPA_Descriptor *data;

         for (data = mainFn(index); data; data = mainFn(++index)) {
            LadspaEffect effect(path, index);
            if (effect.SetHost(NULL)) {
               pm.RegisterPlugin(this, &effect);
            }
         }
      }
   }

   if (lib.IsLoaded()) {
      // PRL:  I suspect Bug1257 -- Crash when enabling Amplio2 -- is the fault of a timing-
      // dependent multi-threading bug in the Amplio2 library itself, in case the unload of the .dll
      // comes too soon after the load.  I saw the bug in Release builds but not Debug.
      // A sleep of even 1 ms was enough to fix the problem for me, but let's be even more generous.
      ::wxMilliSleep(10);
      lib.Unload();
   }

   wxSetWorkingDirectory(saveOldCWD);
   hadpath ? wxSetEnv(wxT("PATH"), envpath) : wxUnsetEnv(wxT("PATH"));

   return index > 0;
}

bool LadspaEffectsModule::IsPluginValid(const wxString & path)
{
   wxString realPath = path.BeforeFirst(wxT(';'));
   return wxFileName::FileExists(realPath);
}

IdentInterface *LadspaEffectsModule::CreateInstance(const wxString & path)
{
   // Acquires a resource for the application.
   // For us, the path is two words.
   // 1)  The library's path
   // 2)  The LADSPA descriptor index
   long index;
   wxString realPath = path.BeforeFirst(wxT(';'));
   path.AfterFirst(wxT(';')).ToLong(&index);

   // Safety of this depends on complementary calls to DeleteInstance on the module manager side.
   return safenew LadspaEffect(realPath, (int)index);
}

void LadspaEffectsModule::DeleteInstance(IdentInterface *instance)
{
   std::unique_ptr < LadspaEffect > {
      dynamic_cast<LadspaEffect *>(instance)
   };
}

wxArrayString LadspaEffectsModule::GetSearchPaths()
{
   wxArrayString pathList;
   wxArrayString files;
   wxString pathVar;

   // Check for the LADSPA_PATH environment variable
   pathVar = wxString::FromUTF8(getenv("LADSPA_PATH"));
   if (!pathVar.empty())
   {
      wxStringTokenizer tok(pathVar);
      while (tok.HasMoreTokens())
      {
         pathList.Add(tok.GetNextToken());
      }
   }

#if defined(__WXMAC__)
#define LADSPAPATH wxT("/Library/Audio/Plug-Ins/LADSPA")

   // Look in ~/Library/Audio/Plug-Ins/LADSPA and /Library/Audio/Plug-Ins/LADSPA
   pathList.Add(wxGetHomeDir() + wxFILE_SEP_PATH + LADSPAPATH);
   pathList.Add(LADSPAPATH);

#elif defined(__WXMSW__)

   // No special paths...probably should look in %CommonProgramFiles%\LADSPA

#else
   
   pathList.Add(wxGetHomeDir() + wxFILE_SEP_PATH + wxT(".ladspa"));
   pathList.Add(wxT("/usr/local/lib/ladspa"));
   pathList.Add(wxT("/usr/lib/ladspa"));
   pathList.Add(wxT(LIBDIR) wxT("/ladspa"));

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
   LadspaEffectOptionsDialog(wxWindow * parent, EffectHostInterface *host);
   virtual ~LadspaEffectOptionsDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   EffectHostInterface *mHost;
   bool mUseLatency;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(LadspaEffectOptionsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, LadspaEffectOptionsDialog::OnOk)
END_EVENT_TABLE()

LadspaEffectOptionsDialog::LadspaEffectOptionsDialog(wxWindow * parent, EffectHostInterface *host)
:  wxDialogWrapper(parent, wxID_ANY, wxString(_("LADSPA Effect Options")))
{
   mHost = host;

   mHost->GetSharedConfig(wxT("Options"), wxT("UseLatency"), mUseLatency, true);

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
         S.StartStatic(_("Latency Compensation"));
         {
            S.AddVariableText(wxString() +
               _("As part of their processing, some LADSPA effects must delay returning ") +
               _("audio to Audacity. When not compensating for this delay, you will ") +
               _("notice that small silences have been inserted into the audio. ") +
               _("Enabling this option will provide that compensation, but it may ") +
               _("not work for all LADSPA effects."))->Wrap(650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(_("Enable &compensation"),
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
   PopulateOrExchange(S);

   mHost->SetSharedConfig(wxT("Options"), wxT("UseLatency"), mUseLatency);

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
   virtual ~LadspaEffectMeter();

private:
   void OnErase(wxEraseEvent & evt);
   void OnPaint(wxPaintEvent & evt);
   void OnIdle(wxIdleEvent & evt);
   void OnSize(wxSizeEvent & evt);

private:
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
:  wxWindow(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxDEFAULT_CONTROL_BORDER),
   mVal(val)
{
   mMin = min;
   mMax = max;
   mLastValue = -mVal;

   SetBackgroundColour(*wxWHITE);
}

LadspaEffectMeter::~LadspaEffectMeter()
{
}

void LadspaEffectMeter::OnIdle(wxIdleEvent & WXUNUSED(evt))
{
   if (mLastValue != mVal)
   {
      Refresh(false);
   }
}

void LadspaEffectMeter::OnErase(wxEraseEvent & WXUNUSED(evt))
{
   // Just ignore it to prevent flashing
}

void LadspaEffectMeter::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
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
   {
      val = mMax;
   }
   if (val < mMin)
   {
      val = mMin;
   }
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

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffect
//
///////////////////////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(LadspaEffect, wxEvtHandler)
   EVT_COMMAND_RANGE(ID_Toggles, ID_Toggles + 999, wxEVT_COMMAND_CHECKBOX_CLICKED, LadspaEffect::OnCheckBox)
   EVT_COMMAND_RANGE(ID_Sliders, ID_Sliders + 999, wxEVT_COMMAND_SLIDER_UPDATED, LadspaEffect::OnSlider)
   EVT_COMMAND_RANGE(ID_Texts, ID_Texts + 999, wxEVT_COMMAND_TEXT_UPDATED, LadspaEffect::OnTextCtrl)
END_EVENT_TABLE()

LadspaEffect::LadspaEffect(const wxString & path, int index)
{
   mPath = path;
   mIndex = index;
   mData = NULL;

   mHost = NULL;
   mMaster = NULL;
   mReady = false;

   mInteractive = false;

   mAudioIns = 0;
   mAudioOuts = 0;
   mNumInputControls = 0;
   mNumOutputControls = 0;
   mSampleRate = 44100;
   mBlockSize = 0;

   mInputPorts = NULL;
   mOutputPorts = NULL;
   mInputControls = NULL;
   mOutputControls = NULL;

   mLatencyPort = -1;

   mDialog = NULL;
   mParent = NULL;
   mSliders = NULL;
   mFields = NULL;
   mLabels = NULL;
   mToggles = NULL;
}

LadspaEffect::~LadspaEffect()
{
   if (mInputPorts)
   {
      delete [] mInputPorts;
   }

   if (mOutputPorts)
   {
      delete [] mOutputPorts;
   }

   if (mInputControls)
   {
      delete [] mInputControls;
   }

   if (mOutputControls)
   {
      delete [] mOutputControls;
   }

   if (mToggles)
   {
      delete [] mToggles;
   }

   if (mSliders)
   {
      delete [] mSliders;
   }

   if (mFields)
   {
      delete [] mFields;
   }

   if (mLabels)
   {
      delete [] mLabels;
   }
}

// ============================================================================
// IdentInterface implementation
// ============================================================================

wxString LadspaEffect::GetPath()
{
   return wxString::Format(wxT("%s;%d"), mPath.c_str(), mIndex);
}

wxString LadspaEffect::GetSymbol()
{
   return LAT1CTOWX(mData->Name);
}

wxString LadspaEffect::GetName()
{
   return GetSymbol();
}

wxString LadspaEffect::GetVendor()
{
   return LAT1CTOWX(mData->Maker);
}

wxString LadspaEffect::GetVersion()
{
   return _("n/a");
}

wxString LadspaEffect::GetDescription()
{
   return LAT1CTOWX(mData->Copyright);
}

// ============================================================================
// EffectIdentInterface implementation
// ============================================================================

EffectType LadspaEffect::GetType()
{
   if (mAudioIns == 0 && mAudioOuts == 0)
   {
      return EffectTypeNone;
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

wxString LadspaEffect::GetFamily()
{
   return LADSPAEFFECTS_FAMILY;
}

bool LadspaEffect::IsInteractive()
{
   return mInteractive;
}

bool LadspaEffect::IsDefault()
{
   return false;
}

bool LadspaEffect::IsLegacy()
{
   return false;
}

bool LadspaEffect::SupportsRealtime()
{
   return GetType() != EffectTypeGenerate;
}

bool LadspaEffect::SupportsAutomation()
{
   return mNumInputControls > 0;
}

// ============================================================================
// EffectClientInterface Implementation
// ============================================================================

bool LadspaEffect::SetHost(EffectHostInterface *host)
{
   mHost = host;

   if (!Load())
   {
      return false;
   }

   mInputPorts = new unsigned long [mData->PortCount];
   mOutputPorts = new unsigned long [mData->PortCount];
   mInputControls = new float [mData->PortCount];
   mOutputControls = new float [mData->PortCount];

   for (unsigned long p = 0; p < mData->PortCount; p++)
   {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];

      // Collect the audio ports
      if (LADSPA_IS_PORT_AUDIO(d))
      {
         if (LADSPA_IS_PORT_INPUT(d)) 
         {
            mInputPorts[mAudioIns++] = p;
         }
         else if (LADSPA_IS_PORT_OUTPUT(d))
         {
            mOutputPorts[mAudioOuts++] = p;
         }
      }
      // Determine the port's default value
      else if (LADSPA_IS_PORT_CONTROL(d) && LADSPA_IS_PORT_INPUT(d))
      {
         mInteractive = true;

         LADSPA_PortRangeHint hint = mData->PortRangeHints[p];
         float val = float(1.0);
         float lower = hint.LowerBound;
         float upper = hint.UpperBound;

         if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor))
         {
            lower *= mSampleRate;
            upper *= mSampleRate;
         }

         if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor) && val < lower)
         {
            val = lower;
         }

         if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor) && val > upper)
         {
            val = upper;
         }

         if (LADSPA_IS_HINT_DEFAULT_MINIMUM(hint.HintDescriptor))
         {
            val = lower;
         }

         if (LADSPA_IS_HINT_DEFAULT_MAXIMUM(hint.HintDescriptor))
         {
            val = upper;
         }

         if (LADSPA_IS_HINT_DEFAULT_LOW(hint.HintDescriptor))
         {
            if (LADSPA_IS_HINT_LOGARITHMIC(hint.HintDescriptor))
            {
               val = exp(log(lower) * 0.75f + log(upper) * 0.25f);
            }
            else
            {
               val = lower * 0.75f + upper * 0.25f;
            }
         }

         if (LADSPA_IS_HINT_DEFAULT_MIDDLE(hint.HintDescriptor))
         {
            if (LADSPA_IS_HINT_LOGARITHMIC(hint.HintDescriptor))
            {
               val = exp(log(lower) * 0.5f + log(upper) * 0.5f);
            }
            else
            {
               val = lower * 0.5f + upper * 0.5f;
            }
         }

         if (LADSPA_IS_HINT_DEFAULT_HIGH(hint.HintDescriptor))
         {
            if (LADSPA_IS_HINT_LOGARITHMIC(hint.HintDescriptor))
            {
               val = exp(log(lower) * 0.25f + log(upper) * 0.75f);
            }
            else
            {
               val = lower * 0.25f + upper * 0.75f;
            }
         }

         if (LADSPA_IS_HINT_DEFAULT_0(hint.HintDescriptor))
         {
            val = 0.0f;
         }

         if (LADSPA_IS_HINT_DEFAULT_1(hint.HintDescriptor))
         {
            val = 1.0f;
         }

         if (LADSPA_IS_HINT_DEFAULT_100(hint.HintDescriptor))
         {
            val = 100.0f;
         }

         if (LADSPA_IS_HINT_DEFAULT_440(hint.HintDescriptor))
         {
            val = 440.0f;
         }

         mNumInputControls++;
         mInputControls[p] = val;
      }
      else if (LADSPA_IS_PORT_CONTROL(d) && LADSPA_IS_PORT_OUTPUT(d))
      {
         mOutputControls[p] = 0.0;

         // LADSPA effects have a convention of providing latency on an output
         // control port whose name is "latency".
         if (strcmp(mData->PortNames[p], "latency") == 0)
         {
            mLatencyPort = p;
         }
         else
         {
            mInteractive = true;
            mNumOutputControls++;
         }
      }
   }

   // mHost will be null during registration
   if (mHost)
   {
      mHost->GetSharedConfig(wxT("Options"), wxT("UseLatency"), mUseLatency, true);

      bool haveDefaults;
      mHost->GetPrivateConfig(mHost->GetFactoryDefaultsGroup(), wxT("Initialized"), haveDefaults, false);
      if (!haveDefaults)
      {
         SaveParameters(mHost->GetFactoryDefaultsGroup());
         mHost->SetPrivateConfig(mHost->GetFactoryDefaultsGroup(), wxT("Initialized"), true);
      }

      LoadParameters(mHost->GetCurrentSettingsGroup());
   }

   return true;
}

unsigned LadspaEffect::GetAudioInCount()
{
   return mAudioIns;
}

unsigned LadspaEffect::GetAudioOutCount()
{
   return mAudioOuts;
}

int LadspaEffect::GetMidiInCount()
{
   return 0;
}

int LadspaEffect::GetMidiOutCount()
{
   return 0;
}

void LadspaEffect::SetSampleRate(double rate)
{
   mSampleRate = rate;
}

size_t LadspaEffect::SetBlockSize(size_t maxBlockSize)
{
   mBlockSize = maxBlockSize;

   return mBlockSize;
}

sampleCount LadspaEffect::GetLatency()
{
   if (mUseLatency && mLatencyPort >= 0 && !mLatencyDone)
   {
      mLatencyDone = true;
      return sampleCount ( mOutputControls[mLatencyPort] );
   }

   return 0;
}

size_t LadspaEffect::GetTailSize()
{
   return 0;
}

bool LadspaEffect::IsReady()
{
   return mReady;
}

bool LadspaEffect::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames WXUNUSED(chanMap))
{
   /* Instantiate the plugin */
   if (!mReady)
   {
      mMaster = InitInstance(mSampleRate);
      if (!mMaster)
      {
         return false;
      }
      mReady = true;
   }

   mLatencyDone = false;

   return true;
}

bool LadspaEffect::ProcessFinalize()
{
   if (mReady)
   {
      mReady = false;

      FreeInstance(mMaster);
      mMaster = NULL;
   }

   return true;
}

size_t LadspaEffect::ProcessBlock(float **inBlock, float **outBlock, size_t blockLen)
{
   for (int i = 0; i < mAudioIns; i++)
   {
      mData->connect_port(mMaster, mInputPorts[i], inBlock[i]);
   }

   for (int i = 0; i < mAudioOuts; i++)
   {
      mData->connect_port(mMaster, mOutputPorts[i], outBlock[i]);
   }

   mData->run(mMaster, blockLen);

   RefreshControls(true);

   return blockLen;
}

bool LadspaEffect::RealtimeInitialize()
{
   return true;
}

bool LadspaEffect::RealtimeAddProcessor(unsigned WXUNUSED(numChannels), float sampleRate)
{
   LADSPA_Handle slave = InitInstance(sampleRate);
   if (!slave)
   {
      return false;
   }

   mSlaves.Add(slave);

   return true;
}

bool LadspaEffect::RealtimeFinalize()
{
   for (size_t i = 0, cnt = mSlaves.GetCount(); i < cnt; i++)
   {
      FreeInstance(mSlaves[i]);
   }
   mSlaves.Clear();

   return true;
}

bool LadspaEffect::RealtimeSuspend()
{
   return true;
}

bool LadspaEffect::RealtimeResume()
{
   return true;
}

bool LadspaEffect::RealtimeProcessStart()
{
   return true;
}

size_t LadspaEffect::RealtimeProcess(int group,
                                          float **inbuf,
                                          float **outbuf,
                                          size_t numSamples)
{
   for (int i = 0; i < mAudioIns; i++)
   {
      mData->connect_port(mSlaves[group], mInputPorts[i], inbuf[i]);
   }

   for (int i = 0; i < mAudioOuts; i++)
   {
      mData->connect_port(mSlaves[group], mOutputPorts[i], outbuf[i]);
   }

   mData->run(mSlaves[group], numSamples);

   return numSamples;
}

bool LadspaEffect::RealtimeProcessEnd()
{
   return true;
}

bool LadspaEffect::ShowInterface(wxWindow *parent, bool forceModal)
{
   if (mDialog)
   {
      mDialog->Close(true);
      return false;
   }

   mDialog = mHost->CreateUI(parent, this);
   if (!mDialog)
   {
      return false;
   }

   mDialog->Layout();
   mDialog->Fit();
   mDialog->SetMinSize(mDialog->GetSize());

   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal)
   {
      mDialog->Show();

      return false;
   }

   bool res = mDialog->ShowModal() != 0;
   mDialog = NULL;

   return res;
}

bool LadspaEffect::GetAutomationParameters(EffectAutomationParameters & parms)
{
   for (unsigned long p = 0; p < mData->PortCount; p++)
   {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];

      if (LADSPA_IS_PORT_CONTROL(d) && LADSPA_IS_PORT_INPUT(d))
      {
         if (!parms.Write(LAT1CTOWX(mData->PortNames[p]), mInputControls[p]))
         {
            return false;
         }
      }
   }

   return true;
}

bool LadspaEffect::SetAutomationParameters(EffectAutomationParameters & parms)
{
   for (unsigned long p = 0; p < mData->PortCount; p++)
   {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];

      if (LADSPA_IS_PORT_CONTROL(d) && LADSPA_IS_PORT_INPUT(d))
      {
         wxString labelText = LAT1CTOWX(mData->PortNames[p]);
         double d = 0.0;
         if (!parms.Read(labelText, &d))
         {
            return false;
         }

         mInputControls[p] = d;
      }
   }

   return true;
}

bool LadspaEffect::LoadUserPreset(const wxString & name)
{
   if (!LoadParameters(name))
   {
      return false;
   }

   RefreshControls();

   return true;
}

bool LadspaEffect::SaveUserPreset(const wxString & name)
{
   return SaveParameters(name);
}

wxArrayString LadspaEffect::GetFactoryPresets()
{
   return wxArrayString();
}

bool LadspaEffect::LoadFactoryPreset(int WXUNUSED(id))
{
   return true;
}

bool LadspaEffect::LoadFactoryDefaults()
{
   if (!LoadParameters(mHost->GetFactoryDefaultsGroup()))
   {
      return false;
   }

   RefreshControls();

   return true;
}

// ============================================================================
// EffectUIClientInterface Implementation
// ============================================================================

void LadspaEffect::SetHostUI(EffectUIHostInterface *host)
{
   mUIHost = host;
}

bool LadspaEffect::PopulateUI(wxWindow *parent)
{
   mParent = parent;

   mParent->PushEventHandler(this);

   mToggles = new wxCheckBox *[mData->PortCount];
   mSliders = new wxSlider *[mData->PortCount];
   mFields = new wxTextCtrl *[mData->PortCount];
   mLabels = new wxStaticText *[mData->PortCount];
   mMeters = new LadspaEffectMeter *[mData->PortCount];

   memset(mFields, 0, mData->PortCount * sizeof(wxTextCtrl *));

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

      if (mNumInputControls)
      {
         auto paramSizer = std::make_unique<wxStaticBoxSizer>(wxVERTICAL, w, _("Effect Settings"));

         auto gridSizer = std::make_unique<wxFlexGridSizer>(5, 0, 0);
         gridSizer->AddGrowableCol(3);

         wxControl *item;

         // Add the duration control for generators
         if (GetType() == EffectTypeGenerate)
         {
            item = safenew wxStaticText(w, 0, _("Duration:"));
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
            mDuration = safenew
               NumericTextCtrl(NumericConverter::TIME,
               w,
               ID_Duration,
               mHost->GetDurationFormat(),
               mHost->GetDuration(),
               mSampleRate,
               wxDefaultPosition,
               wxDefaultSize,
               true);
            mDuration->SetName(_("Duration"));
            mDuration->EnableMenu();
            gridSizer->Add(mDuration, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
            gridSizer->Add(1, 1, 0);
            gridSizer->Add(1, 1, 0);
            gridSizer->Add(1, 1, 0);
         }

         for (unsigned long p = 0; p < mData->PortCount; p++)
         {
            LADSPA_PortDescriptor d = mData->PortDescriptors[p];
            if (LADSPA_IS_PORT_AUDIO(d) || LADSPA_IS_PORT_OUTPUT(d))
            {
               continue;
            }

            wxString labelText = LAT1CTOWX(mData->PortNames[p]);
            item = safenew wxStaticText(w, 0, labelText + wxT(":"));
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

            wxString fieldText;
            LADSPA_PortRangeHint hint = mData->PortRangeHints[p];

            if (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor))
            {
               mToggles[p] = safenew wxCheckBox(w, ID_Toggles + p, wxT(""));
               mToggles[p]->SetName(labelText);
               mToggles[p]->SetValue(mInputControls[p] > 0);
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

            if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor))
            {
               lower = hint.LowerBound;
               haslo = true;
            }

            if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor))
            {
               upper = hint.UpperBound;
               hashi = true;
            }

            if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor))
            {
               lower *= mSampleRate;
               upper *= mSampleRate;
               forceint = true;
            }

            // Limit to the UI precision
            lower = ceilf(lower * 1000000.0) / 1000000.0;
            upper = floorf(upper * 1000000.0) / 1000000.0;
            mInputControls[p] = roundf(mInputControls[p] * 1000000.0) / 1000000.0;

            if (haslo && mInputControls[p] < lower)
            {
               mInputControls[p] = lower;
            }

            if (hashi && mInputControls[p] > upper)
            {
               mInputControls[p] = lower;
            }

            // Don't specify a value at creation time.  This prevents unwanted events
            // being sent to the OnTextCtrl() handler before the associated slider
            // has been created.
            mFields[p] = safenew wxTextCtrl(w, ID_Texts + p);
            mFields[p]->SetName(labelText);
            gridSizer->Add(mFields[p], 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

            wxString str;
            if (haslo)
            {
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
            {
               gridSizer->Add(1, 1, 0);
            }

            mSliders[p] = safenew wxSlider(w, ID_Sliders + p,
               0, 0, 1000,
               wxDefaultPosition,
               wxSize(200, -1));
            mSliders[p]->SetName(labelText);
            gridSizer->Add(mSliders[p], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);

            if (hashi)
            {
               if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
               {
                  str.Printf(wxT("%d"), (int)(upper + 0.5));
               }
               else
               {
                  str = Internat::ToDisplayString(upper);
               }
               item = safenew wxStaticText(w, 0, str);
               gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
            }
            else
            {
               gridSizer->Add(1, 1, 0);
            }

            if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
            {
               fieldText.Printf(wxT("%d"), (int)(mInputControls[p] + 0.5));

               IntegerValidator<float> vld(&mInputControls[p]);
               vld.SetRange(haslo ? lower : INT_MIN,
                  hashi ? upper : INT_MAX);
               mFields[p]->SetValidator(vld);
            }
            else
            {
               fieldText = Internat::ToDisplayString(mInputControls[p]);

               // > 12 decimal places can cause rounding errors in display.
               FloatingPointValidator<float> vld(6, &mInputControls[p]);
               vld.SetRange(lower, upper);

               // Set number of decimal places
               if (upper - lower < 10.0)
               {
                  vld.SetStyle(NUM_VAL_THREE_TRAILING_ZEROES);
               }
               else if (upper - lower < 100.0)
               {
                  vld.SetStyle(NUM_VAL_TWO_TRAILING_ZEROES);
               }
               else
               {
                  vld.SetStyle(NUM_VAL_ONE_TRAILING_ZERO);
               }

               mFields[p]->SetValidator(vld);
            }

            // Set the textctrl value.  This will trigger an event so OnTextCtrl()
            // can update the slider.
            mFields[p]->SetValue(fieldText);
         }

         paramSizer->Add(gridSizer.release(), 0, wxEXPAND | wxALL, 5);
         marginSizer->Add(paramSizer.release(), 0, wxEXPAND | wxALL, 5);
      }

      if (mNumOutputControls > 0)
      {
         auto paramSizer = std::make_unique<wxStaticBoxSizer>(wxVERTICAL, w, _("Effect Output"));

         auto gridSizer = std::make_unique<wxFlexGridSizer>(2, 0, 0);
         gridSizer->AddGrowableCol(1);

         wxControl *item;

         for (unsigned long p = 0; p < mData->PortCount; p++)
         {
            LADSPA_PortDescriptor d = mData->PortDescriptors[p];
            if (LADSPA_IS_PORT_AUDIO(d) || LADSPA_IS_PORT_INPUT(d))
            {
               continue;
            }

            wxString labelText = LAT1CTOWX(mData->PortNames[p]);
            item = safenew wxStaticText(w, 0, labelText + wxT(":"));
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

            //LADSPA_PortRangeHint hint = mData->PortRangeHints[p];

            wxString bound;
            float lower = 0.0;
            float upper = 1.0;

            /*
            bool haslo = false;
            bool hashi = false;
            bool forceint = false;

            if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor))
            {
               lower = hint.LowerBound;
               haslo = true;
            }

            if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor))
            {
               upper = hint.UpperBound;
               hashi = true;
            }

            if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor))
            {
               lower *= mSampleRate;
               upper *= mSampleRate;
               forceint = true;
            }
            */

            // Limit to the UI precision
            lower = ceilf(lower * 1000000.0) / 1000000.0;
            upper = floorf(upper * 1000000.0) / 1000000.0;
            mInputControls[p] = roundf(mInputControls[p] * 1000000.0) / 1000000.0;

            mMeters[p] = safenew LadspaEffectMeter(w, mOutputControls[p], lower, upper);
            mMeters[p]->SetName(labelText);
            gridSizer->Add(mMeters[p], 1, wxEXPAND | wxALIGN_CENTER_VERTICAL | wxALL, 5);
         }

         paramSizer->Add(gridSizer.release(), 0, wxEXPAND | wxALL, 5);
         marginSizer->Add(paramSizer.release(), 0, wxEXPAND | wxALL, 5);

         RefreshControls(true);
      }

      w->SetSizer(uMarginSizer.release());
   }

   w->Layout();

   // Try to give the window a sensible default/minimum size
   wxSize sz1 = marginSizer->GetMinSize();
   wxSize sz2 = mParent->GetMinSize();
   w->SetSizeHints(wxSize(wxMin(sz1.x, sz2.x), wxMin(sz1.y, sz2.y)));

   // And let the parent reduce to the NEW minimum if possible
   mParent->SetSizeHints(-1, -1);

   return true;
}

bool LadspaEffect::IsGraphicalUI()
{
   return false;
}

bool LadspaEffect::ValidateUI()
{
   if (!mParent->Validate())
   {
      return false;
   }

   if (GetType() == EffectTypeGenerate)
   {
      mHost->SetDuration(mDuration->GetValue());
   }

   return true;
}

bool LadspaEffect::HideUI()
{
   return true;
}

bool LadspaEffect::CloseUI()
{
   mParent->RemoveEventHandler(this);

   if (mToggles)
   {
      delete [] mToggles;
      mToggles = NULL;
   }

   if (mSliders)
   {
      delete [] mSliders;
      mSliders = NULL;
   }

   if (mFields)
   {
      delete [] mFields;
      mFields = NULL;
   }

   if (mLabels)
   {
      delete [] mLabels;
      mLabels = NULL;
   }

   mUIHost = NULL;
   mParent = NULL;
   mDialog = NULL;

   return true;
}

bool LadspaEffect::CanExportPresets()
{
   return false;
}

void LadspaEffect::ExportPresets()
{
}

void LadspaEffect::ImportPresets()
{
}

bool LadspaEffect::HasOptions()
{
   return true;
}

void LadspaEffect::ShowOptions()
{
   LadspaEffectOptionsDialog dlg(mParent, mHost);
   if (dlg.ShowModal())
   {
      // Reinitialize configuration options
      mHost->GetSharedConfig(wxT("Options"), wxT("UseLatency"), mUseLatency, true);
   }
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

bool LadspaEffect::LoadParameters(const wxString & group)
{
   wxString parms;
   if (!mHost->GetPrivateConfig(group, wxT("Parameters"), parms, wxEmptyString))
   {
      return false;
   }

   EffectAutomationParameters eap;
   if (!eap.SetParameters(parms))
   {
      return false;
   }

   return SetAutomationParameters(eap);
}

bool LadspaEffect::SaveParameters(const wxString & group)
{
   EffectAutomationParameters eap;
   if (!GetAutomationParameters(eap))
   {
      return false;
   }

   wxString parms;
   if (!eap.GetParameters(parms))
   {
      return false;
   }

   return mHost->SetPrivateConfig(group, wxT("Parameters"), parms);
}

LADSPA_Handle LadspaEffect::InitInstance(float sampleRate)
{
   /* Instantiate the plugin */
   LADSPA_Handle handle = mData->instantiate(mData, sampleRate);
   if (!handle)
   {
      return NULL;
   }

   for (unsigned long p = 0; p < mData->PortCount; p++)
   {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_CONTROL(d))
      {
         if (LADSPA_IS_PORT_INPUT(d))
         {
            mData->connect_port(handle, p, &mInputControls[p]);
         }
         else
         {
            mData->connect_port(handle, p, &mOutputControls[p]);
         }
      }
   }

   if (mData->activate)
   {
      mData->activate(handle);
   }

   return handle;
}

void LadspaEffect::FreeInstance(LADSPA_Handle handle)
{
   if (mData->deactivate)
   {
      mData->deactivate(handle);
   }

   mData->cleanup(handle);
}

void LadspaEffect::OnCheckBox(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Toggles;

   mInputControls[p] = mToggles[p]->GetValue();
}

void LadspaEffect::OnSlider(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Sliders;

   float val;
   float lower = float(0.0);
   float upper = float(10.0);
   float range;
   bool forceint = false;

   LADSPA_PortRangeHint hint = mData->PortRangeHints[p];
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

   mInputControls[p] = val;
}

void LadspaEffect::OnTextCtrl(wxCommandEvent & evt)
{
   LadspaEffect *that = reinterpret_cast<LadspaEffect *>(this);
   int p = evt.GetId() - ID_Texts;

   float val;
   float lower = float(0.0);
   float upper = float(10.0);
   float range;

   val = Internat::CompatibleToDouble(that->mFields[p]->GetValue());

   LADSPA_PortRangeHint hint = that->mData->PortRangeHints[p];
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

   mInputControls[p] = val;

   that->mSliders[p]->SetValue((int)(((val-lower)/range) * 1000.0 + 0.5));
}

void LadspaEffect::RefreshControls(bool outputOnly)
{
   if (!mParent)
   {
      return;
   }

   for (unsigned long p = 0; p < mData->PortCount; p++)
   {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (!(LADSPA_IS_PORT_CONTROL(d)))
      {
         continue;
      }

      wxString fieldText;
      LADSPA_PortRangeHint hint = mData->PortRangeHints[p];

      bool forceint = false;
      if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor))
      {
         forceint = true;
      }

      if (LADSPA_IS_PORT_OUTPUT(d)) 
      {
         continue;
      }

      if (outputOnly)
      {
         continue;
      }

      if (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor))
      {
         mToggles[p]->SetValue(mInputControls[p] > 0);
         continue;
      }

      if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
      {
         fieldText.Printf(wxT("%d"), (int)(mInputControls[p] + 0.5));
      }
      else
      {
         fieldText = Internat::ToDisplayString(mInputControls[p]);
      }

      // Set the textctrl value.  This will trigger an event so OnTextCtrl()
      // can update the slider.
      mFields[p]->SetValue(fieldText);
   }
}
