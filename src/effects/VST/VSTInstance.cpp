/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTInstance.cpp

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.cpp


*//********************************************************************/
#include "VSTInstance.h"

#if USE_VST

#include <wx/time.h>

#if 0
// NOTE:  To debug the subprocess, use wxLogDebug and, on Windows, Debugview
//        from TechNet (Sysinternals).

// ============================================================================
//
// Module registration entry point
//
// This is the symbol that Audacity looks for when the module is built as a
// dynamic library.
//
// When the module is builtin to Audacity, we use the same function, but it is
// declared static so as not to clash with other builtin modules.
//
// ============================================================================
DECLARE_PROVIDER_ENTRY(AudacityModule)
{
   // Create our effects module and register
   // Trust the module manager not to leak this
   return std::make_unique<VSTEffectsModule>();
}

// ============================================================================
//
// Register this as a builtin module
// 
// We also take advantage of the fact that wxModules are initialized before
// the wxApp::OnInit() method is called.  We check to see if Audacity was
// executed to scan a VST effect in a different process.
//
// ============================================================================
DECLARE_BUILTIN_PROVIDER(VSTBuiltin);


///////////////////////////////////////////////////////////////////////////////
///
/// Auto created at program start up, this initialises VST.
///
///////////////////////////////////////////////////////////////////////////////

VSTEffectsModule::VSTEffectsModule()
{
}

VSTEffectsModule::~VSTEffectsModule()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath VSTEffectsModule::GetPath() const
{
   return {};
}

ComponentInterfaceSymbol VSTEffectsModule::GetSymbol() const
{
   return XO("VST Effects");
}

VendorSymbol VSTEffectsModule::GetVendor() const
{
   return XO("The Audacity Team");
}

wxString VSTEffectsModule::GetVersion() const
{
   // This "may" be different if this were to be maintained as a separate DLL
   return AUDACITY_VERSION_STRING;
}

TranslatableString VSTEffectsModule::GetDescription() const
{
   return XO("Adds the ability to use VST effects in Audacity.");
}

// ============================================================================
// PluginProvider implementation
// ============================================================================

bool VSTEffectsModule::Initialize()
{
   // Nothing to do here
   return true;
}

void VSTEffectsModule::Terminate()
{
   // Nothing to do here
   return;
}

EffectFamilySymbol VSTEffectsModule::GetOptionalFamilySymbol()
{
#if USE_VST
   return VSTPLUGINTYPE;
#else
   return {};
#endif
}

const FileExtensions &VSTEffectsModule::GetFileExtensions()
{
   static FileExtensions result{{ _T("vst") }};
   return result;
}

FilePath VSTEffectsModule::InstallPath()
{
   // Not yet ready for VST drag-and-drop...
   // return FileNames::PlugInDir();

   return {};
}

void VSTEffectsModule::AutoRegisterPlugins(PluginManagerInterface &)
{
}

PluginPaths VSTEffectsModule::FindModulePaths(PluginManagerInterface & pm)
{
   FilePaths pathList;
   FilePaths files;

   // Check for the VST_PATH environment variable
   wxString vstpath = wxString::FromUTF8(getenv("VST_PATH"));
   if (!vstpath.empty())
   {
      wxStringTokenizer tok(vstpath, wxPATH_SEP);
      while (tok.HasMoreTokens())
      {
         pathList.push_back(tok.GetNextToken());
      }
   }

#if defined(__WXMAC__)  
#define VSTPATH wxT("/Library/Audio/Plug-Ins/VST")

   // Look in ~/Library/Audio/Plug-Ins/VST and /Library/Audio/Plug-Ins/VST
   pathList.push_back(wxGetHomeDir() + wxFILE_SEP_PATH + VSTPATH);
   pathList.push_back(VSTPATH);

   // Recursively search all paths for Info.plist files.  This will identify all
   // bundles.
   pm.FindFilesInPathList(wxT("Info.plist"), pathList, files, true);

   // Remove the 'Contents/Info.plist' portion of the names
   for (size_t i = 0; i < files.size(); i++)
   {
      files[i] = wxPathOnly(wxPathOnly(files[i]));
      if (!files[i].EndsWith(wxT(".vst")))
      {
         files.erase( files.begin() + i-- );
      }
   }

#elif defined(__WXMSW__)

   TCHAR dpath[MAX_PATH];
   TCHAR tpath[MAX_PATH];
   DWORD len;

   // Try HKEY_CURRENT_USER registry key first
   len = WXSIZEOF(tpath);
   if (SHRegGetUSValue(wxT("Software\\VST"),
                       wxT("VSTPluginsPath"),
                       NULL,
                       tpath,
                       &len,
                       FALSE,
                       NULL,
                       0) == ERROR_SUCCESS)
   {
      tpath[len] = 0;
      dpath[0] = 0;
      ExpandEnvironmentStrings(tpath, dpath, WXSIZEOF(dpath));
      pathList.push_back(dpath);
   }

   // Then try HKEY_LOCAL_MACHINE registry key
   len = WXSIZEOF(tpath);
   if (SHRegGetUSValue(wxT("Software\\VST"),
                       wxT("VSTPluginsPath"),
                       NULL,
                       tpath,
                       &len,
                       TRUE,
                       NULL,
                       0) == ERROR_SUCCESS)
   {
      tpath[len] = 0;
      dpath[0] = 0;
      ExpandEnvironmentStrings(tpath, dpath, WXSIZEOF(dpath));
      pathList.push_back(dpath);
   }

   // Add the default path last
   dpath[0] = 0;
   ExpandEnvironmentStrings(wxT("%ProgramFiles%\\Steinberg\\VSTPlugins"),
                            dpath,
                            WXSIZEOF(dpath));
   pathList.push_back(dpath);

   dpath[0] = 0;
   ExpandEnvironmentStrings(wxT("%COMMONPROGRAMFILES%\\VST2"),
                            dpath,
                            WXSIZEOF(dpath));
   pathList.push_back(dpath);

   // Recursively scan for all DLLs
   pm.FindFilesInPathList(wxT("*.dll"), pathList, files, true);

#else

   // Nothing specified in the VST_PATH environment variable...provide defaults
   if (vstpath.empty())
   {
      // We add this "non-default" one
      pathList.push_back(wxT(LIBDIR) wxT("/vst"));

      // These are the defaults used by other hosts
      pathList.push_back(wxT("/usr/lib/vst"));
      pathList.push_back(wxT("/usr/local/lib/vst"));
      pathList.push_back(wxGetHomeDir() + wxFILE_SEP_PATH + wxT(".vst"));
   }

   // Recursively scan for all shared objects
   pm.FindFilesInPathList(wxT("*.so"), pathList, files, true);

#endif

   return { files.begin(), files.end() };
}

unsigned VSTEffectsModule::DiscoverPluginsAtPath(
   const PluginPath & path, TranslatableString &errMsg,
   const RegistrationCallback &callback)
{

   VSTEffect effect(path);
   if(effect.InitializePlugin())
   {
      auto effectIDs = effect.GetEffectIDs();
      if(effectIDs.empty())
         //Each VST plugin path in Audacity should have id(index) part in it
         effectIDs.push_back(0);

      for(auto id : effectIDs)
      {
         //Subsequent VSTEffect::Load may seem like overhead, but we need
         //to initialize EffectDefinitionInterface part, which includes
         //properly formatted plugin path
         VSTEffect subeffect(wxString::Format("%s;%d", path, id));
         subeffect.Load();
         if(callback)
            callback(this, &subeffect);
      }
      return effectIDs.size();
   }
   errMsg = XO("Could not load the library");
   return 0;
}

std::unique_ptr<ComponentInterface>
VSTEffectsModule::LoadPlugin(const PluginPath & path)
{
   // Acquires a resource for the application.
   // For us, the ID is simply the path to the effect
   auto result = std::make_unique<VSTEffect>(path);
   if (!result->InitializePlugin())
      result.reset();
   return result;
}

bool VSTEffectsModule::CheckPluginExist(const PluginPath& path) const
{
   const auto modulePath = path.BeforeFirst(wxT(';'));
   return wxFileName::FileExists(modulePath) || wxFileName::DirExists(modulePath);
}

// ============================================================================
// ModuleEffectInterface implementation
// ============================================================================

// ============================================================================
// VSTEffectsModule implementation
// ============================================================================

///////////////////////////////////////////////////////////////////////////////
//
// Dialog for configuring latency, buffer size and graphics mode for a
// VST effect.
//
///////////////////////////////////////////////////////////////////////////////
class VSTEffectOptionsDialog final : public wxDialogWrapper
{
public:
   explicit VSTEffectOptionsDialog(const EffectDefinitionInterface &effect);
   virtual ~VSTEffectOptionsDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   const EffectDefinitionInterface &mEffect;
   int mBufferSize;
   bool mUseLatency;
   bool mUseGUI;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(VSTEffectOptionsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, VSTEffectOptionsDialog::OnOk)
END_EVENT_TABLE()

VSTEffectOptionsDialog::VSTEffectOptionsDialog(
   const EffectDefinitionInterface &effect
)  : wxDialogWrapper{ nullptr, wxID_ANY, XO("VST Effect Options") }
   , mEffect{ effect }
{
   GetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("BufferSize"), mBufferSize, 8192);
   GetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("UseLatency"), mUseLatency, true);
   GetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("UseGUI"), mUseGUI, true);

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

VSTEffectOptionsDialog::~VSTEffectOptionsDialog()
{
}

void VSTEffectOptionsDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(false);
      {
         S.StartStatic(XO("Buffer Size"));
         {
            S.AddVariableText( XO(
"The buffer size controls the number of samples sent to the effect "
"on each iteration. Smaller values will cause slower processing and "
"some effects require 8192 samples or less to work properly. However "
"most effects can accept large buffers and using them will greatly "
"reduce processing time."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               wxTextCtrl *t;
               t = S.Validator<IntegerValidator<int>>(
                     &mBufferSize, NumValidatorStyle::DEFAULT, 8, 1048576 * 1)
                  .MinSize( { 100, -1 } )
                  .TieNumericTextBox(XXO("&Buffer Size (8 to 1048576 samples):"),
                                       mBufferSize,
                                       12);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(XO("Latency Compensation"));
         {
            S.AddVariableText( XO(
"As part of their processing, some VST effects must delay returning "
"audio to Audacity. When not compensating for this delay, you will "
"notice that small silences have been inserted into the audio. "
"Enabling this option will provide that compensation, but it may "
"not work for all VST effects."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(XXO("Enable &compensation"),
                             mUseLatency);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(XO("Graphical Mode"));
         {
            S.AddVariableText( XO(
"Most VST effects have a graphical interface for setting parameter values."
" A basic text-only method is also available. "
" Reopen the effect for this to take effect."),
               false, 0, 650);
            S.TieCheckBox(XXO("Enable &graphical interface"),
                          mUseGUI);
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

void VSTEffectOptionsDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   if (!Validate())
   {
      return;
   }

   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   SetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("BufferSize"), mBufferSize);
   SetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("UseLatency"), mUseLatency);
   SetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("UseGUI"), mUseGUI);

   EndModal(wxID_OK);
}

///////////////////////////////////////////////////////////////////////////////
///
/// Wrapper for wxTimer that calls a VST effect at regular intervals.
///
/// \todo should there be tests for no timer available?
///
///////////////////////////////////////////////////////////////////////////////
class VSTTimer final : public wxTimer
{
public:
   VSTTimer(VSTEditor* pEditor)
   :  wxTimer(),
      mpEditor(pEditor)
   {
   }

   ~VSTTimer()
   {
   }

   void Notify()
   {
      mpEditor->OnTimer();
   }

private:
   VSTEditor* mpEditor;
};

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffect
//
///////////////////////////////////////////////////////////////////////////////
enum
{
   ID_Duration = 20000,
   ID_Sliders = 21000,
};

wxDEFINE_EVENT(EVT_SIZEWINDOW, wxCommandEvent);
DEFINE_LOCAL_EVENT_TYPE(EVT_UPDATEDISPLAY);

VSTEffect::VSTEffect(const PluginPath & path)
:  VSTWrapper(path)
{
   memset(&mTimeInfo, 0, sizeof(mTimeInfo));
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = 44100.0;  // this is a bogus value, but it's only for the display
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid;
}

VSTEffect::~VSTEffect()
{
}

PluginPath VSTEffect::GetPath() const
{
   return mPath;
}

ComponentInterfaceSymbol VSTEffect::GetSymbol() const
{
   return VSTWrapper::GetSymbol();
}

VendorSymbol VSTEffect::GetVendor() const
{
   return { mVendor };
}

wxString VSTEffect::GetVersion() const
{
   wxString version;

   bool skipping = true;
   for (int i = 0, s = 0; i < 4; i++, s += 8)
   {
      int dig = (mVersion >> s) & 0xff;
      if (dig != 0 || !skipping)
      {
         version += !skipping ? wxT(".") : wxT("");
         version += wxString::Format(wxT("%d"), dig);
         skipping = false;
      }
   }

   return version;
}

TranslatableString VSTEffect::GetDescription() const
{
   // VST does have a product string opcode and some effects return a short
   // description, but most do not or they just return the name again.  So,
   // try to provide some sort of useful information.
   return XO("Audio In: %d, Audio Out: %d").Format( mAudioIns, mAudioOuts );
}

// ============================================================================
// EffectDefinitionInterface Implementation
// ============================================================================

EffectType VSTEffect::GetType() const
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


EffectFamilySymbol VSTEffect::GetFamily() const
{
   return VSTPLUGINTYPE;
}

bool VSTEffect::IsInteractive() const
{
   return mInteractive;
}

bool VSTEffect::IsDefault() const
{
   return false;
}

auto VSTEffect::RealtimeSupport() const -> RealtimeSince
{
   return RealtimeSince::Always;

   /* return GetType() == EffectTypeProcess
      ? RealtimeSince::Always
      : RealtimeSince::Never; */
}

bool VSTEffect::SupportsAutomation() const
{
   return mAutomatable;
}

bool VSTEffect::InitializePlugin()
{
   if (!mAEffect)
   {
      Load();
   }

   if (!mAEffect)
   {
      return false;
   }

   return true;
}
#endif

std::unique_ptr<EffectInstance::Message> VSTInstance::MakeMessage() const
{
   // The purpose here is just to allocate vectors (chunk and paramVector)
   // with sufficient size, not to get the values too
   VSTSettings settings;
   FetchSettings(settings, /* doFetch = */ false);

   VSTMessage::ParamVector paramVector;
   paramVector.resize(mAEffect->numParams, std::nullopt);

   return std::make_unique<VSTMessage>( std::move(settings.mChunk), std::move(paramVector) );
}


std::unique_ptr<EffectInstance::Message> VSTInstance::MakeMessage(int id, double value) const
{
   return std::make_unique<VSTMessage>(id, value, mAEffect->numParams);
}


unsigned VSTInstance::GetAudioInCount() const
{
   return mAudioIns;
}

unsigned VSTInstance::GetAudioOutCount() const
{
   return mAudioOuts;
}

size_t VSTInstance::SetBlockSize(size_t maxBlockSize)
{
   // Issue 3935 for IEM plug-ins, VST 2 versions:
   // It is mysterious why this further limitation of size works to
   // prevent the crashes in destructive processing, or why this is not
   // needed for non-destructive, but here it is
   // Those plugins report many channels (like 64) but most others will not
   // be affected by these lines with the default size of 8192
   // Note it may make the Block Size option of the settings dialog misleading
   auto numChannels = std::max({ 1u, GetAudioInCount(), GetAudioOutCount() });
   maxBlockSize = std::max(size_t(1),
      std::min(maxBlockSize, size_t(0x8000u / numChannels)));

   mBlockSize = std::min( maxBlockSize, mUserBlockSize );
   return mBlockSize;
}

size_t VSTInstance::GetBlockSize() const
{
   return mBlockSize;
}

auto VSTInstance::GetLatency(
   const EffectSettings& settings, double sampleRate) const -> SampleCount
{
   if (mUseLatency)
      return mBufferDelay;
   return 0;
}

bool VSTInstance::IsReady()
{
   return mReady;
}

bool VSTInstance::ProcessInitialize(
   EffectSettings& settings, double sampleRate, ChannelNames)
{
   // Issue 3942: Copy the contents of settings first.
   // settings may refer to what is in the RealtimeEffectState, but that might
   // get reassigned by EffectSettingsAccess::Set, when the validator's
   // Automate() is called-back by the plug-in during callSetParameter.
   // So this avoids a dangling reference.
   auto copiedSettings = GetSettings(settings);
   StoreSettings(copiedSettings);

   return DoProcessInitialize(sampleRate);
}

bool VSTInstance::DoProcessInitialize(double sampleRate)
{
   // Initialize time info
   memset(&mTimeInfo, 0, sizeof(mTimeInfo));
   mTimeInfo.sampleRate = sampleRate;
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid | kVstTransportPlaying;

   // Set processing parameters...power must be off for this
   callDispatcher(effSetSampleRate, 0, 0, NULL, sampleRate);
   callDispatcher(effSetBlockSize, 0, mBlockSize, NULL, 0.0);

   // Turn on the power
   PowerOn();

   // Set the initial buffer delay
   SetBufferDelay(mAEffect->initialDelay);

   mReady = true;
   return true;
}

bool VSTInstance::ProcessFinalize() noexcept
{
   return GuardedCall<bool>([&] {
      mReady = false;

      PowerOff();

      return true;
   });

}

size_t VSTInstance::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   // Only call the effect if there's something to do...some do not like zero-length block
   if (blockLen)
   {
      // Go let the plugin moleste the samples
      callProcessReplacing(inBlock, outBlock, blockLen);

      // And track the position
      mTimeInfo.samplePos += (double) blockLen;
   }

   return blockLen;
}

bool VSTInstance::RealtimeInitialize(EffectSettings &settings, double sampleRate)
{
   // Temporarily disconnect from any validator, so that setting the chunk
   // does not cause Automate() callbacks (as some effects will do) that then
   // would send slider movement messages that might destroy information in
   // the settings.
   auto vr = valueRestorer(mpOwningValidator, (VSTUIWrapper*)nullptr);
   return ProcessInitialize(settings, sampleRate, {});
}

bool VSTInstance::RealtimeAddProcessor(EffectSettings &settings,
   EffectOutputs *, unsigned numChannels, float sampleRate)
{
   if (!mRecruited)
   {
      // Assign self to the first processor
      mRecruited = true;
      return true;
   }

   auto &effect = static_cast<const PerTrackEffect &>(mProcessor);
   auto slave = std::make_unique<VSTInstance>(
      const_cast<PerTrackEffect &>(effect),
      mPath, mBlockSize, mUserBlockSize, mUseLatency);

   slave->SetBlockSize(mBlockSize);

   if (!slave->ProcessInitialize(settings, sampleRate, ChannelNames()))
      return false;

   mSlaves.emplace_back(move(slave));
   return true;
}

bool VSTInstance::RealtimeFinalize(EffectSettings&) noexcept
{
return GuardedCall<bool>([&]{

   if (mpOwningValidator)
      mpOwningValidator->Flush();

   mRecruited = false;

   for (const auto &slave : mSlaves)
      slave->ProcessFinalize();
   mSlaves.clear();

   return ProcessFinalize();
});
}

bool VSTInstance::RealtimeSuspend()
{
   PowerOff();

   for (const auto &slave : mSlaves)
      slave->PowerOff();

   return true;
}

bool VSTInstance::RealtimeResume()
{
   PowerOn();

   for (const auto &slave : mSlaves)
      slave->PowerOn();

   return true;
}

bool VSTInstance::OnePresetWasLoadedWhilePlaying()
{
   return mPresetLoadedWhilePlaying.exchange(false);
}

void VSTInstance::DeferChunkApplication()
{
   std::lock_guard<std::mutex> guard(mDeferredChunkMutex);

   if (! mChunkToSetAtIdleTime.empty() )
   {    
      ApplyChunk(mChunkToSetAtIdleTime);
      mChunkToSetAtIdleTime.resize(0);
   }
}

void VSTInstance::ApplyChunk(std::vector<char>& chunk)
{
   VstPatchChunkInfo info = {
      1, mAEffect->uniqueID, mAEffect->version, mAEffect->numParams, "" };

   const auto len = chunk.size();
   const auto data = chunk.data();

   callSetChunk(true, len, data, &info);
   for (auto& slave : mSlaves)
      slave->callSetChunk(true, len, data, &info);
}

bool VSTInstance::ChunkMustBeAppliedInMainThread() const
{
   // Some plugins (e.g. Melda) can not have their chunk set in the
   // audio thread, resulting in making the whole app hang.
   // This is why we defer the setting of the chunk in the main thread.

   const bool IsAudioThread = (mMainThreadId != std::this_thread::get_id());
   
   return IsAudioThread && mIsMeldaPlugin;
}

bool VSTInstance::UsesMessages() const noexcept
{
   return true;
}

bool VSTInstance::RealtimeProcessStart(MessagePackage& package)
{
   const bool applyChunkInMainThread = ChunkMustBeAppliedInMainThread();

   if (applyChunkInMainThread)
      mDeferredChunkMutex.lock();

   if (!package.pMessage)
      return true;

   auto& message = static_cast<VSTMessage&>(*package.pMessage);

   auto &chunk = message.mChunk;

   if (!chunk.empty())
   {
      if (applyChunkInMainThread)
      {
         // Apply the chunk later
         //
         mChunkToSetAtIdleTime = chunk;
      }
      else
      {
         // Apply the chunk now
         ApplyChunk(chunk);
      }

      // Don't apply the chunk again until another message supplies a chunk
      chunk.resize(0);

      // Don't return yet.  Maybe some slider movements also accumulated after
      // the change of the chunk.

      const bool IsAudioThread = (mMainThreadId != std::this_thread::get_id());
      if (IsAudioThread)
      {
         // At the moment, the only reason why this method would be called in the audio thread,
         // is because a preset was loaded while playing

         mPresetLoadedWhilePlaying.store(true);
      }

   }


   assert(message.mParamsVec.size() == mAEffect->numParams);

   for (size_t paramID=0; paramID < mAEffect->numParams; paramID++)
   {
      if (message.mParamsVec[paramID])
      {
         float val = (float)(*message.mParamsVec[paramID]);

         // set the change on the recruited "this" instance
         callSetParameter(paramID, val);

         // set the change on any existing slaves
         for (auto& slave : mSlaves)
         {
            slave->callSetParameter(paramID, val);
         }

         // clear the used info
         message.mParamsVec[paramID] = std::nullopt;
      }
   }

   return true;
}

size_t VSTInstance::RealtimeProcess(size_t group, EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   if (!mRecruited)
   {
      // unexpected!
      return 0;
   }

   wxASSERT(numSamples <= mBlockSize);

   if (group == 0)
   {
      // use the recruited "this" instance
      return ProcessBlock(settings, inbuf, outbuf, numSamples);
   }
   else if (group <= mSlaves.size())
   {
      // use the slave which maps to the group
      return mSlaves[group - 1]->ProcessBlock(settings, inbuf, outbuf, numSamples);
   }
   else
      return 0;
}

bool VSTInstance::RealtimeProcessEnd(EffectSettings &) noexcept
{
   if ( ChunkMustBeAppliedInMainThread() )
      mDeferredChunkMutex.unlock();

   return true;
}

void VSTInstance::NeedIdle()
{
   if (mpOwningValidator)
   {
      mpOwningValidator->NeedIdle();
   }
}

void VSTInstance::PowerOn()
{
   if (!mHasPower)
   {
      // Turn the power on
      callDispatcher(effMainsChanged, 0, 1, NULL, 0.0);

      // Tell the effect we're going to start processing
      if (mVstVersion >= 2)
      {
         callDispatcher(effStartProcess, 0, 0, NULL, 0.0);
      }

      // Set state
      mHasPower = true;
   }
}

void VSTInstance::PowerOff()
{
   if (mHasPower)
   {
      // Tell the effect we're going to stop processing
      if (mVstVersion >= 2)
      {
         callDispatcher(effStopProcess, 0, 0, NULL, 0.0);
      }

      // Turn the power off
      callDispatcher(effMainsChanged, 0, 0, NULL, 0.0);

      // Set state
      mHasPower = false;
   }
}

void VSTInstance::SizeWindow(int w, int h)
{
   if (mpOwningValidator)
   {
      mpOwningValidator->SizeWindow(w, h);
   }
}

void VSTInstance::SetBufferDelay(int samples)
{
   // We do not support negative delay
   if (samples >= 0 && mUseLatency)
   {
      mBufferDelay = samples;
   }

   return;
}

void VSTInstance::callProcessReplacing(const float *const *inputs,
   float *const *outputs, int sampleframes)
{
   mAEffect->processReplacing(mAEffect,
      const_cast<float**>(inputs),
      const_cast<float**>(outputs), sampleframes);
}

void VSTInstance::Automate(int index, float value)
{
   if (mMainThreadId != std::this_thread::get_id())
      return;

   if (mpOwningValidator)
   {
      mpOwningValidator->Automate(index, value);
   }
}

VSTInstance::VSTInstance
(
   PerTrackEffect& effect,
   const PluginPath& path,
   size_t            blockSize,
   size_t            userBlockSize,
   bool              useLatency
)

   : PerTrackEffect::Instance(effect)
   , VSTWrapper(path)
   , mUseLatency{ useLatency }
{
   // what also happens in the effect ctor
   //
   memset(&mTimeInfo, 0, sizeof(mTimeInfo));
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = 44100.0;  // this is a bogus value, but it's only for the display
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid;

   mBlockSize = blockSize;
   mUserBlockSize = userBlockSize;

   Load();

   if (!IsReady() )
   {
      // Set some defaults since some VSTs need them...these will be reset when
      // normal or realtime processing begins
      mBlockSize = 8192;
      DoProcessInitialize(44100.0);
   }

   mIsMeldaPlugin = (mVendor == "MeldaProduction");
}

VSTInstance::~VSTInstance()
{
   PowerOff();
}

void VSTInstance::SetOwningValidator(VSTUIWrapper* vi)
{
   mpOwningValidator = vi;
}

#endif // USE_VST
