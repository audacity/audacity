/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.h

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_EFFECT__
#define __AUDACITY_EFFECT__

#include "../Audacity.h"
#include "../MemoryX.h"
#include <set>

#include "../MemoryX.h"
#include <wx/bmpbuttn.h>
#include <wx/dynarray.h>
#include <wx/intl.h>
#include <wx/string.h>
#include <wx/tglbtn.h>

class wxCheckBox;
class wxChoice;
class wxListBox;
class wxWindow;

#include "audacity/ConfigInterface.h"
#include "audacity/EffectInterface.h"

#include "../Experimental.h"
#include "../SelectedRegion.h"
#include "../Shuttle.h"
#include "../Internat.h"
#include "../widgets/ProgressDialog.h"

#include "../Track.h"

class ShuttleGui;

#define BUILTIN_EFFECT_PREFIX wxT("Built-in Effect: ")

class AudacityProject;
class LabelTrack;
class SelectedRegion;
class TimeWarper;
class EffectUIHost;
class Track;
class TrackList;
class TrackFactory;
class WaveTrack;

// TODO:  Apr-06-2015
// TODO:  Much more cleanup of old methods and variables is needed, but
// TODO:  can't be done until after all effects are using the NEW API.

class AUDACITY_DLL_API Effect /* not final */ : public wxEvtHandler,
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

   wxString GetPath() override;
   wxString GetSymbol() override;
   wxString GetName() override;
   wxString GetVendor() override;
   wxString GetVersion() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;
   wxString GetFamily() override;
   bool IsInteractive() override;
   bool IsDefault() override;
   bool IsLegacy() override;
   bool SupportsRealtime() override;
   bool SupportsAutomation() override;

   // EffectClientInterface implementation

   bool SetHost(EffectHostInterface *host) override;
   
   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;

   int GetMidiInCount() override;
   int GetMidiOutCount() override;

   sampleCount GetLatency() override;
   size_t GetTailSize() override;

   void SetSampleRate(double rate) override;
   size_t SetBlockSize(size_t maxBlockSize) override;

   bool IsReady() override;
   bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) override;
   bool ProcessFinalize() override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;

   bool RealtimeInitialize() override;
   bool RealtimeAddProcessor(unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize() override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() override;
   bool RealtimeProcessStart() override;
   size_t RealtimeProcess(int group,
                                       float **inbuf,
                                       float **outbuf,
                                       size_t numSamples) override;
   bool RealtimeProcessEnd() override;

   bool ShowInterface(wxWindow *parent, bool forceModal = false) override;

   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

   bool LoadUserPreset(const wxString & name) override;
   bool SaveUserPreset(const wxString & name) override;

   wxArrayString GetFactoryPresets() override;
   bool LoadFactoryPreset(int id) override;
   bool LoadFactoryDefaults() override;

   // EffectUIClientInterface implementation

   void SetHostUI(EffectUIHostInterface *host) override;
   bool PopulateUI(wxWindow *parent) override;
   bool IsGraphicalUI() override;
   bool ValidateUI() override;
   bool HideUI() override;
   bool CloseUI() override;

   bool CanExportPresets() override;
   void ExportPresets() override;
   void ImportPresets() override;

   bool HasOptions() override;
   void ShowOptions() override;

   // EffectHostInterface implementation

   double GetDefaultDuration() override;
   double GetDuration() override;
   wxString GetDurationFormat() override;
   virtual wxString GetSelectionFormat() /* not override? */; // time format in Selection toolbar
   void SetDuration(double duration) override;

   bool Apply() override;
   void Preview() override;

   wxDialog *CreateUI(wxWindow *parent, EffectUIClientInterface *client) override;

   wxString GetUserPresetsGroup(const wxString & name) override;
   wxString GetCurrentSettingsGroup() override;
   wxString GetFactoryDefaultsGroup() override;
   virtual wxString GetSavedStateGroup() /* not override? */;

   // ConfigClientInterface implementation

   bool HasSharedConfigGroup(const wxString & group) override;
   bool GetSharedConfigSubgroups(const wxString & group, wxArrayString & subgroups) override;

   bool GetSharedConfig(const wxString & group, const wxString & key, wxString & value, const wxString & defval = wxEmptyString) override;
   bool GetSharedConfig(const wxString & group, const wxString & key, int & value, int defval = 0) override;
   bool GetSharedConfig(const wxString & group, const wxString & key, bool & value, bool defval = false) override;
   bool GetSharedConfig(const wxString & group, const wxString & key, float & value, float defval = 0.0) override;
   bool GetSharedConfig(const wxString & group, const wxString & key, double & value, double defval = 0.0) override;

   bool SetSharedConfig(const wxString & group, const wxString & key, const wxString & value) override;
   bool SetSharedConfig(const wxString & group, const wxString & key, const int & value) override;
   bool SetSharedConfig(const wxString & group, const wxString & key, const bool & value) override;
   bool SetSharedConfig(const wxString & group, const wxString & key, const float & value) override;
   bool SetSharedConfig(const wxString & group, const wxString & key, const double & value) override;

   bool RemoveSharedConfigSubgroup(const wxString & group) override;
   bool RemoveSharedConfig(const wxString & group, const wxString & key) override;

   bool HasPrivateConfigGroup(const wxString & group) override;
   bool GetPrivateConfigSubgroups(const wxString & group, wxArrayString & subgroups) override;

   bool GetPrivateConfig(const wxString & group, const wxString & key, wxString & value, const wxString & defval = wxEmptyString) override;
   bool GetPrivateConfig(const wxString & group, const wxString & key, int & value, int defval = 0) override;
   bool GetPrivateConfig(const wxString & group, const wxString & key, bool & value, bool defval = false) override;
   bool GetPrivateConfig(const wxString & group, const wxString & key, float & value, float defval = 0.0) override;
   bool GetPrivateConfig(const wxString & group, const wxString & key, double & value, double defval = 0.0) override;

   bool SetPrivateConfig(const wxString & group, const wxString & key, const wxString & value) override;
   bool SetPrivateConfig(const wxString & group, const wxString & key, const int & value) override;
   bool SetPrivateConfig(const wxString & group, const wxString & key, const bool & value) override;
   bool SetPrivateConfig(const wxString & group, const wxString & key, const float & value) override;
   bool SetPrivateConfig(const wxString & group, const wxString & key, const double & value) override;

   bool RemovePrivateConfigSubgroup(const wxString & group) override;
   bool RemovePrivateConfig(const wxString & group, const wxString & key) override;

   // Effect implementation

   // NEW virtuals
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

   /* not virtual */ void SetPresetParameters( const wxArrayString * Names, const wxArrayString * Values ) {
      if( Names ) mPresetNames = *Names;
      if( Values ) mPresetValues = *Values;
   }

   // Returns true on success.  Will only operate on tracks that
   // have the "selected" flag set to true, which is consistent with
   // Audacity's standard UI.
   /* not virtual */ bool DoEffect(wxWindow *parent, double projectRate, TrackList *list,
                 TrackFactory *factory, SelectedRegion *selectedRegion,
                 bool shouldPrompt = true);

   // Realtime Effect Processing
   /* not virtual */ bool RealtimeAddProcessor(int group, unsigned chans, float rate);
   /* not virtual */ size_t RealtimeProcess(int group,
                               unsigned chans,
                               float **inbuf,
                               float **outbuf,
                               size_t numSamples);
   /* not virtual */ bool IsRealtimeActive();

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

   // No more virtuals!

   // The Progress methods all return true if the user has cancelled;
   // you should exit immediately if this happens (cleaning up memory
   // is okay, but don't try to undo).

   // Pass a fraction between 0.0 and 1.0
   bool TotalProgress(double frac);

   // Pass a fraction between 0.0 and 1.0, for the current track
   // (when doing one track at a time)
   bool TrackProgress(int whichTrack, double frac, const wxString & = wxEmptyString);

   // Pass a fraction between 0.0 and 1.0, for the current track group
   // (when doing stereo groups at a time)
   bool TrackGroupProgress(int whichGroup, double frac, const wxString & = wxEmptyString);

   int GetNumWaveTracks() { return mNumTracks; }

   int GetNumWaveGroups() { return mNumGroups; }

   // Calculates the start time and selection length in samples
   void GetSamples(WaveTrack *track, sampleCount *start, sampleCount *len);

   void SetTimeWarper(std::unique_ptr<TimeWarper> &&warper);
   TimeWarper *GetTimeWarper();

   // Previewing linear effect can be optimised by pre-mixing. However this
   // should not be used for non-linear effects such as dynamic processors
   // To allow pre-mixing before Preview, set linearEffectFlag to true.
   void SetLinearEffectFlag(bool linearEffectFlag);

   // Most effects only need to preview a short selection. However some
   // (such as fade effects) need to know the full selection length.
   void SetPreviewFullSelectionFlag(bool previewDurationFlag);

   // Use this if the effect needs to know if it is previewing
   bool IsPreviewing() { return mIsPreview; }

   // Most effects only require selected tracks to be copied for Preview.
   // If IncludeNotSelectedPreviewTracks(true), then non-linear effects have
   // preview copies of all wave tracks.
   void IncludeNotSelectedPreviewTracks(bool includeNotSelected);

   // Use these two methods to copy the input tracks to mOutputTracks, if
   // doing the processing on them, and replacing the originals only on success (and not cancel).
   void CopyInputTracks(); // trackType = Track::Wave
   void CopyInputTracks(int trackType);

   // For the use of analyzers, which don't need to make output wave tracks,
   // but may need to add label tracks.
   class AddedAnalysisTrack {
      friend Effect;
      AddedAnalysisTrack(Effect *pEffect, const wxString &name);
      AddedAnalysisTrack(const AddedAnalysisTrack&) PROHIBITED;

   public:

      AddedAnalysisTrack() {}

      // So you can have a vector of them
      AddedAnalysisTrack(AddedAnalysisTrack &&that);

      LabelTrack *get() const { return mpTrack; }

      // Call this to indicate successful completion of the analyzer.
      void Commit();

      // Destructor undoes the addition of the analysis track if not committed.
      ~AddedAnalysisTrack();

   private:
      Effect *mpEffect{};
      LabelTrack *mpTrack{};
   };

   // Set name to given value if that is not empty, else use default name
   std::shared_ptr<AddedAnalysisTrack> AddAnalysisTrack(const wxString &name = wxString());

   // For the use of analyzers, which don't need to make output wave tracks,
   // but may need to modify label tracks.
   class ModifiedAnalysisTrack {
      friend Effect;
      ModifiedAnalysisTrack
         (Effect *pEffect, const LabelTrack *pOrigTrack, const wxString &name);
      ModifiedAnalysisTrack(const ModifiedAnalysisTrack&) PROHIBITED;

   public:

      ModifiedAnalysisTrack();

      // So you can have a vector of them
      ModifiedAnalysisTrack(ModifiedAnalysisTrack &&that);

      LabelTrack *get() const { return mpTrack; }

      // Call this to indicate successful completion of the analyzer.
      void Commit();

      // Destructor undoes the modification of the analysis track if not committed.
      ~ModifiedAnalysisTrack();

   private:
      Effect *mpEffect{};
      LabelTrack *mpTrack{};
      movable_ptr<Track> mpOrigTrack{};
   };

   // Set name to given value if that is not empty, else use default name
   ModifiedAnalysisTrack ModifyAnalysisTrack
      (const LabelTrack *pOrigTrack, const wxString &name = wxString());

   // If bGoodResult, replace mWaveTracks tracks in mTracks with successfully processed
   // mOutputTracks copies, get rid of old mWaveTracks, and set mWaveTracks to mOutputTracks.
   // Else clear and DELETE mOutputTracks copies.
   void ReplaceProcessedTracks(const bool bGoodResult);

   // Use this to append a NEW output track.
   Track *AddToOutputTracks(std::unique_ptr<Track> &&t);

//
// protected data
//
// The Effect base class will set these variables, some or all of which
// may be needed by any particular subclass of Effect.
//
protected:
   ProgressDialog *mProgress; // Temporary pointer, NOT deleted in destructor.
   double         mProjectRate; // Sample rate of the project - NEW tracks should
                               // be created with this rate...
   double         mSampleRate;
   TrackFactory   *mFactory;
   TrackList      *mTracks;      // the complete list of all tracks
   std::unique_ptr<TrackList> mOutputTracks; // used only if CopyInputTracks() is called.
   double         mT0;
   double         mT1;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   double         mF0;
   double         mF1;
#endif
   std::unique_ptr<TimeWarper> mWarper;
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

   std::vector<Track*> mIMap;
   std::vector<Track*> mOMap;

   int mNumTracks; //v This is really mNumWaveTracks, per CountWaveTracks() and GetNumWaveTracks().
   int mNumGroups;

   // For client driver
   EffectClientInterface *mClient;
   unsigned mNumAudioIn;
   unsigned mNumAudioOut;

   float **mInBuffer;
   float **mOutBuffer;
   float **mInBufPos;
   float **mOutBufPos;

   size_t mBufferSize;
   size_t mBlockSize;
   unsigned mNumChannels;

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
// FIXME:  Remove this once all effects are using the NEW dialog
// FIXME:

#define ID_EFFECT_PREVIEW ePreviewID

// Base dialog for regular effect
class AUDACITY_DLL_API EffectDialog /* not final */ : public wxDialogWrapper
{
public:
   // constructors and destructors
   EffectDialog(wxWindow * parent,
                const wxString & title,
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
};

//
class EffectUIHost final : public wxDialogWrapper,
                     public EffectUIHostInterface
{
public:
   // constructors and destructors
   EffectUIHost(wxWindow *parent,
                Effect *effect,
                EffectUIClientInterface *client);
   virtual ~EffectUIHost();

   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   int ShowModal() override;

   bool Initialize();

private:
   void OnInitDialog(wxInitDialogEvent & evt);
   void OnErase(wxEraseEvent & evt);
   void OnPaint(wxPaintEvent & evt);
   void OnClose(wxCloseEvent & evt);
   void OnApply(wxCommandEvent & evt);
   void DoCancel();
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

   DECLARE_EVENT_TABLE()
};

class EffectPresetsDialog final : public wxDialogWrapper
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

   DECLARE_EVENT_TABLE()
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
   wxUnusedVar(MIN_ ##name); \
   wxUnusedVar(MAX_ ##name); \
   wxUnusedVar(SCL_ ##name); \
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
