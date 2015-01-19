/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.h

  Dominic Mazzoni

**********************************************************************/

class wxSlider;
class wxStaticText;
class wxTextCtrl;
class wxCheckBox;

#include <wx/dialog.h>

#include "audacity/EffectInterface.h"
#include "audacity/ModuleInterface.h"
#include "audacity/PluginInterface.h"

#include "../../widgets/NumericTextCtrl.h"

#include "ladspa.h"

#define LADSPAEFFECTS_VERSION wxT("1.0.0.0")
#define LADSPAEFFECTS_FAMILY wxT("LADSPA")

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffect
//
///////////////////////////////////////////////////////////////////////////////

WX_DEFINE_ARRAY_PTR(LADSPA_Handle, LadspaSlaveArray);

class LadspaEffectEventHelper;

class LadspaEffect : public EffectClientInterface,
                     public EffectUIClientInterface
{
public:
   LadspaEffect(const wxString & path, int index);
   virtual ~LadspaEffect();

   // IdentInterface implementation

   virtual wxString GetPath();
   virtual wxString GetSymbol();
   virtual wxString GetName();
   virtual wxString GetVendor();
   virtual wxString GetVersion();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();
   virtual wxString GetFamily();
   virtual bool IsInteractive();
   virtual bool IsDefault();
   virtual bool IsLegacy();
   virtual bool SupportsRealtime();
   virtual bool SupportsAutomation();

   // EffectClientInterface implementation

   virtual bool SetHost(EffectHostInterface *host);

   virtual int GetAudioInCount();
   virtual int GetAudioOutCount();

   virtual int GetMidiInCount();
   virtual int GetMidiOutCount();

   virtual void SetSampleRate(sampleCount rate);
   virtual sampleCount GetBlockSize(sampleCount maxBlockSize);

   virtual sampleCount GetLatency();
   virtual sampleCount GetTailSize();

   virtual bool IsReady();
   virtual bool ProcessInitialize();
   virtual bool ProcessFinalize();
   virtual sampleCount ProcessBlock(float **inbuf, float **outbuf, sampleCount size);

   virtual bool RealtimeInitialize();
   virtual bool RealtimeAddProcessor(int numChannels, float sampleRate);
   virtual bool RealtimeFinalize();
   virtual bool RealtimeSuspend();
   virtual bool RealtimeResume();
   virtual bool RealtimeProcessStart();
   virtual sampleCount RealtimeProcess(int group,
                                       float **inbuf,
                                       float **outbuf,
                                       sampleCount numSamples);
   virtual bool RealtimeProcessEnd();

   virtual bool ShowInterface(wxWindow *parent, bool forceModal = false);

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // EffectUIClientInterface implementation

   virtual void SetUIHost(EffectUIHostInterface *host);
   virtual bool PopulateUI(wxWindow *parent);
   virtual bool IsGraphicalUI();
   virtual bool ValidateUI();
   virtual bool HideUI();
   virtual bool CloseUI();

   virtual void LoadUserPreset(const wxString & name);
   virtual void SaveUserPreset(const wxString & name);

   virtual wxArrayString GetFactoryPresets();
   virtual void LoadFactoryPreset(int id);
   virtual void LoadFactoryDefaults();

   virtual bool CanExport();
   virtual void ExportPresets();
   virtual void ImportPresets();

   virtual bool HasOptions();
   virtual void ShowOptions();

   // LadspaEffect implementation

private:
   bool Load();
   void Unload();

   void LoadParameters(const wxString & group);
   void SaveParameters(const wxString & group);

   LADSPA_Handle InitInstance(float sampleRate);
   void FreeInstance(LADSPA_Handle handle);

   void OnCheckBox(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);
   void OnTextCtrl(wxCommandEvent & evt);
   void RefreshControls(bool outputOnly = false);

private:

   wxString mPath;
   int mIndex;
   EffectHostInterface *mHost;

   wxDynamicLibrary mLib;
   const LADSPA_Descriptor *mData;

   wxString pluginName;

   bool mReady;

   LADSPA_Handle mMaster;

   sampleCount mSampleRate;
   sampleCount mBlockSize;
   sampleCount mUserBlockSize;

   bool mInteractive;

   int mAudioIns;
   unsigned long *mInputPorts;

   int mAudioOuts;
   unsigned long *mOutputPorts;

   int mNumInputControls;
   float *mInputControls;
   int mNumOutputControls;
   float *mOutputControls;

   bool mUseLatency;
   int mLatencyPort;
   bool mLatencyDone;

   // Realtime processing
   LadspaSlaveArray mSlaves;

   EffectUIHostInterface *mUIHost;
   LadspaEffectEventHelper *mEventHelper;

   NumericTextCtrl *mDuration;
   wxDialog *mDialog;
   wxWindow *mParent;
   wxSlider **mSliders;
   wxTextCtrl **mFields;
   wxStaticText **mLabels;
   wxCheckBox **mToggles;

   friend class LadspaEffectEventHelper;
   friend class LadspaEffectsModule;
};

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffectEventHelper
//
///////////////////////////////////////////////////////////////////////////////

class LadspaEffectEventHelper : public wxEvtHandler
{
public:
   LadspaEffectEventHelper(LadspaEffect *effect);
   virtual ~LadspaEffectEventHelper();

   // LadspaEffectEventHelper implementatino

   void OnCheckBox(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);
   void OnTextCtrl(wxCommandEvent & evt);
   void ControlSetFocus(wxFocusEvent & evt);

private:
   LadspaEffect *mEffect;

   DECLARE_EVENT_TABLE();
};

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class LadspaEffectsModule : public ModuleInterface
{
public:
   LadspaEffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~LadspaEffectsModule();

   // IdentInterface implementatino

   virtual wxString GetPath();
   virtual wxString GetSymbol();
   virtual wxString GetName();
   virtual wxString GetVendor();
   virtual wxString GetVersion();
   virtual wxString GetDescription();

   // ModuleInterface implementation

   virtual bool Initialize();
   virtual void Terminate();

   virtual bool AutoRegisterPlugins(PluginManagerInterface & pm);
   virtual wxArrayString FindPlugins(PluginManagerInterface & pm);
   virtual bool RegisterPlugin(PluginManagerInterface & pm, const wxString & path);

   virtual bool IsPluginValid(const wxString & path);

   virtual IdentInterface *CreateInstance(const wxString & path);
   virtual void DeleteInstance(IdentInterface *instance);

   // LadspaEffectModule implementation

   static void Check(const wchar_t *path);

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};

