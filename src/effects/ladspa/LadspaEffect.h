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

#define LADSPAEFFECTS_VERSION wxT("1.0.0.0");
#define LADSPAEFFECTS_FAMILY L"Ladspa"

WX_DEFINE_ARRAY_PTR(LADSPA_Handle, LadspaSlaveArray);

class LadspaEffectDialog;

class LadspaEffect : public EffectClientInterface
{
public:
   LadspaEffect(const wxString & path, int index);
   virtual ~LadspaEffect();

   // IdentInterface implementation
   virtual PluginID GetID();
   virtual wxString GetPath();
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
   virtual bool IsRealtimeCapable();

   // EffectClientInterface implementation
   virtual void SetHost(EffectHostInterface *host);
   virtual bool Startup();
   virtual bool Shutdown();

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
   virtual sampleCount RealtimeProcess(int group,
                                       float **inbuf,
                                       float **outbuf,
                                       sampleCount numSamples);

   virtual bool ShowInterface(void *parent);

private:
   // LadspaEffect implementation
   bool Load();
   void Unload();

   void LoadParameters(const wxString & group);
   void SaveParameters(const wxString & group);

   LADSPA_Handle InitInstance(float sampleRate);
   void FreeInstance(LADSPA_Handle handle);

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
   float *mOutputControls;

   int mLatencyPort;
   bool mLatencyDone;

   // Realtime processing
   LadspaSlaveArray mSlaves;
   wxArrayInt mSlaveChannels;

   LadspaEffectDialog *mDlg;

   friend class LadspaEffectDialog;
   friend class LadspaEffectsModule;
};

class LadspaEffectsModule : public ModuleInterface
{
public:
   LadspaEffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~LadspaEffectsModule();

   // IdentInterface implementatino

   virtual wxString GetID();
   virtual wxString GetPath();
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

   virtual void *CreateInstance(const PluginID & ID, const wxString & path);

   // LadspaEffectModule implementation

   static void Check(const wchar_t *path);

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};

class LadspaEffectDialog : public wxDialog
{
   DECLARE_DYNAMIC_CLASS(LadspaEffectDialog)

 public:
   LadspaEffectDialog(LadspaEffect *effect,
                      wxWindow * parent,
                      const LADSPA_Descriptor *data,
                      float *inputControls,
                      sampleCount sampleRate,
                      double length);

   ~LadspaEffectDialog();

   double GetLength();

   DECLARE_EVENT_TABLE()

private:
   void OnCheckBox(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);
   void OnTextCtrl(wxCommandEvent & evt);
   void OnApply(wxCommandEvent & evt);
   void OnOK(wxCommandEvent & evt);
   void OnCancel(wxCommandEvent & evt);
   void OnPreview(wxCommandEvent & evt);
   void OnDefaults(wxCommandEvent & evt);

   void HandleText();
   void RefreshParaemters();
   void ConnectFocus(wxControl *c);
   void DisconnectFocus(wxControl *c);
   void ControlSetFocus(wxFocusEvent & evt);

private:
   bool inSlider;
   bool inText;

   int mSampleRate;
   const LADSPA_Descriptor *mData;
   wxSlider **mSliders;
   wxTextCtrl **mFields;
   wxStaticText **mLabels;
   wxCheckBox **mToggles;
   unsigned long *mPorts;
   unsigned long mNumParams;
   float *mInputControls;
   LadspaEffect *mEffect;
   NumericTextCtrl *mSeconds;
};
