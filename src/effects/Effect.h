/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.h

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_EFFECT__
#define __AUDACITY_EFFECT__

#include <set>

#include <wx/bmpbuttn.h>
#include <wx/dynarray.h>
#include <wx/intl.h>
#include <wx/string.h>
#include <wx/tglbtn.h>

class wxDialog;
class wxWindow;

#include "audacity/ConfigInterface.h"
#include "audacity/EffectInterface.h"

#include "../Experimental.h"
#include "../WaveTrack.h"
#include "../SelectedRegion.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../Internat.h"
#include "../widgets/ProgressDialog.h"

#define BUILTIN_EFFECT_PREFIX wxT("Built-in Effect: ")

class SelectedRegion;
class TimeWarper;
class EffectUIHost;

// TODO:  Apr-06-2015
// TODO:  Much more cleanup of old methods and variables is needed, but
// TODO:  can't be done until after all effects are using the new API.

class AUDACITY_DLL_API Effect : public wxEvtHandler,
                                public EffectClientInterface,
                                public EffectUIClientInterface,
                                public EffectHostInterface
{
 //
 // public methods
 //
 // Used by the outside program to determine properties of an effect and
 // apply the effect to one or more tracks.
 //
 public:
   // The constructor is called once by each subclass at the beginning of the program.
   // Avoid allocating memory or doing time-consuming processing here.
   Effect();
   virtual ~Effect();

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

   virtual sampleCount GetLatency();
   virtual sampleCount GetTailSize();

   virtual void SetSampleRate(sampleCount rate);
   virtual sampleCount SetBlockSize(sampleCount maxBlockSize);

   virtual bool IsReady();
   virtual bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL);
   virtual bool ProcessFinalize();
   virtual sampleCount ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen);

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

   virtual bool LoadUserPreset(const wxString & name);
   virtual bool SaveUserPreset(const wxString & name);

   virtual wxArrayString GetFactoryPresets();
   virtual bool LoadFactoryPreset(int id);
   virtual bool LoadFactoryDefaults();

   // EffectUIClientInterface implementation

   virtual void SetHostUI(EffectUIHostInterface *host);
   virtual bool PopulateUI(wxWindow *parent);
   virtual bool IsGraphicalUI();
   virtual bool ValidateUI();
   virtual bool HideUI();
   virtual bool CloseUI();

   virtual bool CanExportPresets();
   virtual void ExportPresets();
   virtual void ImportPresets();

   virtual bool HasOptions();
   virtual void ShowOptions();

   // EffectHostInterface implementation

   virtual double GetDefaultDuration();
   virtual double GetDuration();
   virtual wxString GetDurationFormat();
   virtual void SetDuration(double duration);

   virtual bool Apply();
   virtual void Preview();

   virtual wxDialog *CreateUI(wxWindow *parent, EffectUIClientInterface *client);

   virtual wxString GetUserPresetsGroup(const wxString & name);
   virtual wxString GetCurrentSettingsGroup();
   virtual wxString GetFactoryDefaultsGroup();
   virtual wxString GetSavedStateGroup();

   // ConfigClientInterface implementation

   virtual bool HasSharedConfigGroup(const wxString & group);
   virtual bool GetSharedConfigSubgroups(const wxString & group, wxArrayString & subgroups);

   virtual bool GetSharedConfig(const wxString & group, const wxString & key, wxString & value, const wxString & defval = wxEmptyString);
   virtual bool GetSharedConfig(const wxString & group, const wxString & key, int & value, int defval = 0);
   virtual bool GetSharedConfig(const wxString & group, const wxString & key, bool & value, bool defval = false);
   virtual bool GetSharedConfig(const wxString & group, const wxString & key, float & value, float defval = 0.0);
   virtual bool GetSharedConfig(const wxString & group, const wxString & key, double & value, double defval = 0.0);
   virtual bool GetSharedConfig(const wxString & group, const wxString & key, sampleCount & value, sampleCount defval = 0);

   virtual bool SetSharedConfig(const wxString & group, const wxString & key, const wxString & value);
   virtual bool SetSharedConfig(const wxString & group, const wxString & key, const int & value);
   virtual bool SetSharedConfig(const wxString & group, const wxString & key, const bool & value);
   virtual bool SetSharedConfig(const wxString & group, const wxString & key, const float & value);
   virtual bool SetSharedConfig(const wxString & group, const wxString & key, const double & value);
   virtual bool SetSharedConfig(const wxString & group, const wxString & key, const sampleCount & value);

   virtual bool RemoveSharedConfigSubgroup(const wxString & group);
   virtual bool RemoveSharedConfig(const wxString & group, const wxString & key);

   virtual bool HasPrivateConfigGroup(const wxString & group);
   virtual bool GetPrivateConfigSubgroups(const wxString & group, wxArrayString & subgroups);

   virtual bool GetPrivateConfig(const wxString & group, const wxString & key, wxString & value, const wxString & defval = wxEmptyString);
   virtual bool GetPrivateConfig(const wxString & group, const wxString & key, int & value, int defval = 0);
   virtual bool GetPrivateConfig(const wxString & group, const wxString & key, bool & value, bool defval = false);
   virtual bool GetPrivateConfig(const wxString & group, const wxString & key, float & value, float defval = 0.0);
   virtual bool GetPrivateConfig(const wxString & group, const wxString & key, double & value, double defval = 0.0);
   virtual bool GetPrivateConfig(const wxString & group, const wxString & key, sampleCount & value, sampleCount defval = 0);

   virtual bool SetPrivateConfig(const wxString & group, const wxString & key, const wxString & value);
   virtual bool SetPrivateConfig(const wxString & group, const wxString & key, const int & value);
   virtual bool SetPrivateConfig(const wxString & group, const wxString & key, const bool & value);
   virtual bool SetPrivateConfig(const wxString & group, const wxString & key, const float & value);
   virtual bool SetPrivateConfig(const wxString & group, const wxString & key, const double & value);
   virtual bool SetPrivateConfig(const wxString & group, const wxString & key, const sampleCount & value);

   virtual bool RemovePrivateConfigSubgroup(const wxString & group);
   virtual bool RemovePrivateConfig(const wxString & group, const wxString & key);

   // Effect implementation

   virtual PluginID GetID();

   virtual bool Startup(EffectClientInterface *client);
   virtual bool Startup();

   virtual bool GetAutomationParameters(wxString & parms);
   virtual bool SetAutomationParameters(const wxString & parms);

   virtual wxArrayString GetUserPresets();
   virtual bool HasCurrentSettings();
   virtual bool HasFactoryDefaults();
   virtual wxString GetPreset(wxWindow * parent, const wxString & parms);

   virtual bool IsBatchProcessing();
   virtual void SetBatchProcessing(bool start);

   void SetPresetParameters( const wxArrayString * Names, const wxArrayString * Values ){
      if( Names ) mPresetNames = *Names;
      if( Values ) mPresetValues = *Values;
   }

   // Returns true on success.  Will only operate on tracks that
   // have the "selected" flag set to true, which is consistent with
   // Audacity's standard UI.
   bool DoEffect(wxWindow *parent, double projectRate, TrackList *list,
                 TrackFactory *factory, SelectedRegion *selectedRegion,
                 bool shouldPrompt = true);

   // Realtime Effect Processing
   bool RealtimeAddProcessor(int group, int chans, float rate);
   sampleCount RealtimeProcess(int group,
                               int chans,
                               float **inbuf,
                               float **outbuf,
                               sampleCount numSamples);
   bool IsRealtimeActive();

   virtual bool IsHidden();

//
// protected virtual methods
//
// Each subclass of Effect overrides one or more of these methods to
// do its processing.
//
protected:
   // Called once each time an effect is called.  Perform any initialization;
   // make sure that the effect can be performed on the selected tracks and
   // return false otherwise
   virtual bool Init();

   // If necessary, open a dialog to get parameters from the user.
   // This method will not always be called (for example if a user
   // repeats an effect) but if it is called, it will be called
   // after Init.
   virtual bool PromptUser(wxWindow *parent);

   // Check whether effect should be skipped
   // Typically this is only useful in automation, for example
   // detecting that zero noise reduction is to be done,
   // or that normalisation is being done without Dc bias shift
   // or amplitude modification
   virtual bool CheckWhetherSkipEffect() { return false; }

   // Actually do the effect here.
   virtual bool Process();
   virtual bool ProcessPass();
   virtual bool InitPass1();
   virtual bool InitPass2();
   virtual int GetPass();

   // clean up any temporary memory
   virtual void End();

   // Most effects just use the previewLength, but time-stretching/compressing
   // effects need to use a different input length, so override this method.
   virtual double CalcPreviewInputLength(double previewLength);

   // The Effect class fully implements the Preview method for you.
   // Only override it if you need to do preprocessing or cleanup.
   virtual void Preview(bool dryOnly);

   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();
   virtual bool EnableApply(bool enable = true);
   virtual bool EnablePreview(bool enable = true);
   virtual void EnableDebug(bool enable = true);

   // The Progress methods all return true if the user has cancelled;
   // you should exit immediately if this happens (cleaning up memory
   // is okay, but don't try to undo).

   // Pass a fraction between 0.0 and 1.0
   bool TotalProgress(double frac);

   // Pass a fraction between 0.0 and 1.0, for the current track
   // (when doing one track at a time)
   bool TrackProgress(int whichTrack, double frac, wxString = wxT(""));

   // Pass a fraction between 0.0 and 1.0, for the current track group
   // (when doing stereo groups at a time)
   bool TrackGroupProgress(int whichGroup, double frac);

   int GetNumWaveTracks() { return mNumTracks; }

   int GetNumWaveGroups() { return mNumGroups; }

   // Calculates the start time and selection length in samples
   void GetSamples(WaveTrack *track, sampleCount *start, sampleCount *len);

   void SetTimeWarper(TimeWarper *warper);
   TimeWarper *GetTimeWarper();

   // Previewing linear effect can be optimised by pre-mixing. However this
   // should not be used for non-linear effects such as dynamic processors
   // To allow pre-mixing before Preview, set linearEffectFlag to true.
   void SetLinearEffectFlag(bool linearEffectFlag);

   // Most effects only need to preview a short selection. However some
   // (such as fade effects) need to know the full selection length.
   void SetPreviewFullSelectionFlag(bool previewDurationFlag);

   // Most effects only require selected tracks to be copied for Preview.
   // If IncludeNotSelectedPreviewTracks(true), then non-linear effects have
   // preview copies of all wave tracks.
   void IncludeNotSelectedPreviewTracks(bool includeNotSelected);

   // Use these two methods to copy the input tracks to mOutputTracks, if
   // doing the processing on them, and replacing the originals only on success (and not cancel).
   void CopyInputTracks(int trackType = Track::Wave);

   // If bGoodResult, replace mWaveTracks tracks in mTracks with successfully processed
   // mOutputTracks copies, get rid of old mWaveTracks, and set mWaveTracks to mOutputTracks.
   // Else clear and delete mOutputTracks copies.
   void ReplaceProcessedTracks(const bool bGoodResult);

   // Use this to append a new output track.
   void AddToOutputTracks(Track *t);

//
// protected data
//
// The Effect base class will set these variables, some or all of which
// may be needed by any particular subclass of Effect.
//
protected:
   ProgressDialog *mProgress;
   double         mProjectRate; // Sample rate of the project - new tracks should
                               // be created with this rate...
   double         mSampleRate;
   TrackFactory   *mFactory;
   TrackList      *mTracks;      // the complete list of all tracks
   TrackList      *mOutputTracks; // used only if CopyInputTracks() is called.
   double         mT0;
   double         mT1;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   double         mF0;
   double         mF1;
#endif
   TimeWarper     *mWarper;
   wxArrayString  mPresetNames;
   wxArrayString  mPresetValues;
   int            mPass;

   // UI
   wxDialog       *mUIDialog;
   wxWindow       *mUIParent;
   int            mUIResultID;

   sampleCount    mSampleCnt;

   // type of the tracks on mOutputTracks
   int            mOutputTracksType;

 // Used only by the base Effect class
 //
 private:
   void CommonInit();
   void CountWaveTracks();

   // Driver for client effects
   bool ProcessTrack(int count,
                     ChannelNames map,
                     WaveTrack *left,
                     WaveTrack *right,
                     sampleCount leftStart,
                     sampleCount rightStart,
                     sampleCount len);
 
 //
 // private data
 //
 // Used only by the base Effect class
 //
private:
   wxWindow *mParent;

   bool mIsBatch;

   bool mIsLinearEffect;
   bool mPreviewWithNotSelected;
   bool mPreviewFullSelection;

   bool mIsSelection;
   double mDuration;
   wxString mDurationFormat;

   bool mIsPreview;

   bool mUIDebug;

   wxArrayPtrVoid mIMap;
   wxArrayPtrVoid mOMap;

   int mNumTracks; //v This is really mNumWaveTracks, per CountWaveTracks() and GetNumWaveTracks().
   int mNumGroups;

   // For client driver
   EffectClientInterface *mClient;
   int mNumAudioIn;
   int mNumAudioOut;

   float **mInBuffer;
   float **mOutBuffer;
   float **mInBufPos;
   float **mOutBufPos;

   sampleCount mBufferSize;
   sampleCount mBlockSize;
   int mNumChannels;

   wxArrayInt mGroupProcessor;
   int mCurrentProcessor;

   wxCriticalSection mRealtimeSuspendLock;
   int mRealtimeSuspendCount;

   const static wxString kUserPresetIdent;
   const static wxString kFactoryPresetIdent;
   const static wxString kCurrentSettingsIdent;
   const static wxString kFactoryDefaultsIdent;

   friend class EffectManager;// so it can call PromptUser in support of batch commands.
   friend class EffectRack;
   friend class EffectUIHost;
   friend class EffectPresetsDialog;
};


// FIXME:
// FIXME:  Remove this once all effects are using the new dialog
// FIXME:

#define ID_EFFECT_PREVIEW ePreviewID

// Base dialog for regular effect
class AUDACITY_DLL_API EffectDialog:public wxDialog
{
public:
   // constructors and destructors
   EffectDialog(wxWindow * parent,
                const wxString & title,
                int type = 0,
                int flags = wxDEFAULT_DIALOG_STYLE,
                int additionalButtons = 0);

   void Init();

   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();
   virtual bool Validate();
   virtual void OnPreview(wxCommandEvent & evt);
   virtual void OnOk(wxCommandEvent & evt);

private:
   int mType;
   int mAdditionalButtons;

   DECLARE_EVENT_TABLE();
};

//
class EffectUIHost : public wxDialog,
                     public EffectUIHostInterface
{
public:
   // constructors and destructors
   EffectUIHost(wxWindow *parent,
                Effect *effect,
                EffectUIClientInterface *client);
   virtual ~EffectUIHost();

#if defined(__WXMAC__)
   virtual bool Show(bool show = true);
#endif

   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

   virtual int ShowModal();

   bool Initialize();

private:
   void OnInitDialog(wxInitDialogEvent & evt);
   void OnErase(wxEraseEvent & evt);
   void OnPaint(wxPaintEvent & evt);
   void OnClose(wxCloseEvent & evt);
   void OnApply(wxCommandEvent & evt);
   void OnCancel(wxCommandEvent & evt);
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
   wxBitmap CreateBitmap(const char *xpm[], bool up, bool pusher);
   void LoadUserPresets();

   void InitializeRealtime();
   void CleanupRealtime();

private:
   AudacityProject *mProject;
   wxWindow *mParent;
   Effect *mEffect;
   EffectUIClientInterface *mClient;

   wxArrayString mUserPresets;
   bool mInitialized;
   bool mSupportsRealtime;
   bool mIsGUI;
   bool mIsBatch;

#if defined(__WXMAC__)
   bool mIsModal;
#endif

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

   DECLARE_EVENT_TABLE();
};

class EffectPresetsDialog : public wxDialog
{
public:
   EffectPresetsDialog(wxWindow *parent, Effect *effect);
   virtual ~EffectPresetsDialog();

   wxString GetSelected() const;
   void SetSelected(const wxString & parms);

private:
   void SetPrefix(const wxString & type, const wxString & prefix);
   void UpdateUI();

   void OnType(wxCommandEvent & evt);
   void OnOk(wxCommandEvent & evt);
   void OnCancel(wxCommandEvent & evt);

private:
   wxChoice *mType;
   wxListBox *mPresets;

   wxArrayString mFactoryPresets;
   wxArrayString mUserPresets;
   wxString mSelection;

   DECLARE_EVENT_TABLE();
};

// Utility functions

inline float TrapFloat(float x, float min, float max)
{
   if (x <= min)
      return min;

   if (x >= max)
      return max;

   return x;
}

inline double TrapDouble(double x, double min, double max)
{
   if (x <= min)
      return min;

   if (x >= max)
      return max;

   return x;
}

inline long TrapLong(long x, long min, long max)
{
   if (x <= min)
      return min;

   if (x >= max)
      return max;

   return x;
}

// Helper macros for defining, reading and verifying effect parameters

#define Param(name, type, key, def, min, max, scale) \
   static const wxChar * KEY_ ## name = (key); \
   static const type DEF_ ## name = (def); \
   static const type MIN_ ## name = (min); \
   static const type MAX_ ## name = (max); \
   static const type SCL_ ## name = (scale);

#define PBasic(name, type, key, def) \
   static const wxChar * KEY_ ## name = (key); \
   static const type DEF_ ## name = (def);

#define PRange(name, type, key, def, min, max) \
   PBasic(name, type, key, def); \
   static const type MIN_ ## name = (min); \
   static const type MAX_ ## name = (max);

#define PScale(name, type, key, def, min, max, scale) \
   PRange(name, type, key, def, min, max); \
   static const type SCL_ ## name = (scale);

#define ReadParam(type, name) \
   type name; \
   if (!parms.ReadAndVerify(KEY_ ## name, &name, DEF_ ## name, MIN_ ## name, MAX_ ## name)) \
      return false;

#define ReadBasic(type, name) \
   type name; \
   if (!parms.ReadAndVerify(KEY_ ## name, &name, DEF_ ## name)) \
      return false;

#define ReadAndVerifyEnum(name, list) \
   int name; \
   if (!parms.ReadAndVerify(KEY_ ## name, &name, DEF_ ## name, list)) \
      return false;

#define ReadAndVerifyInt(name) ReadParam(int, name)
#define ReadAndVerifyDouble(name) ReadParam(double, name)
#define ReadAndVerifyFloat(name) ReadParam(float, name)
#define ReadAndVerifyBool(name) ReadBasic(bool, name)
#define ReadAndVerifyString(name) ReadBasic(wxString, name)

#endif
