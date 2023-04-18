/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTWrapper.h

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.h

**********************************************************************/
#if USE_VST

#ifndef __AUDACITY_VST_WRAPPER__
#define __AUDACITY_VST_WRAPPER__

#include "EffectInterface.h"
#include "CFResources.h"
#include "XMLTagHandler.h"

#include <wx/dynlib.h>
#include <wx/string.h>

#include <unordered_map>
#include <vector>
#include <mutex>
#include <thread>

#include "aeffectx.h"

typedef intptr_t (*dispatcherFn)(AEffect * effect,
                                 int opCode,
                                 int index,
                                 intptr_t value,
                                 void *ptr,
                                 float opt);

typedef void (*processFn)(AEffect * effect,
                          float **inputs,
                          float **outputs,
                          int sampleframes);

typedef void (*setParameterFn)(AEffect * effect,
                               int index,
                               float parameter);

typedef float (*getParameterFn)(AEffect * effect,
                                int index);

typedef AEffect *(*vstPluginMain)(audioMasterCallback audioMaster);

class wxDynamicLibrary;

#if defined(__WXMAC__)
struct __CFBundle;
typedef struct __CFBundle *CFBundleRef;
#if __LP64__
typedef int CFBundleRefNum;
#else
typedef signed short                    SInt16;
typedef SInt16 CFBundleRefNum;
#endif
#endif

class VST_API VSTLink /* not final */
{
public:
   virtual ~VSTLink() {};
   virtual intptr_t callDispatcher(int opcode, int index, intptr_t value, void *ptr, float opt) = 0;
};

struct VSTSettings
{
   // These are saved in the Config and checked against when loading a preset, to make sure
   // that we are loading a Config  which is compatible.
   //
   int32_t mUniqueID{};
   int32_t mVersion{};
   int32_t mNumParams{};

   // When loading a preset, the preferred way is to use the chunk; when not present in
   // the Config or failing to load, we fall back to loading single parameters (ID, value) pairs.
   //
   // It looks like a plugin might not support this (if their effFlagsProgramChunks bit is off)
   // If not, then hold an empty vector
   //
   std::vector<char> mChunk;

   // Fallback data used when the chunk is not available.
   std::unordered_map<wxString, std::optional<double> > mParamsMap;
};

struct VST_API VSTUIWrapper
{
   virtual void Idle();
   virtual void NeedIdle();
   virtual void SizeWindow(int w, int h);
   virtual void Automate(int index, float value);
   virtual void Flush();
};

struct VST_API VSTWrapper
   : public VSTLink, public XMLTagHandler, public VSTUIWrapper
{
   static inline VSTSettings& GetSettings(EffectSettings& settings)
   {
      auto pSettings = settings.cast<VSTSettings>();
      assert(pSettings);
      return *pSettings;
   }

   static inline const VSTSettings& GetSettings(const EffectSettings& settings)
   {
      auto pSettings = settings.cast<VSTSettings>();
      assert(pSettings);
      return *pSettings;
   }

   explicit VSTWrapper(const PluginPath& path)
      : mPath(path)
      , mMainThreadId{ std::this_thread::get_id() }
   {}

   ~VSTWrapper();

   AEffect* mAEffect = nullptr;
   std::thread::id mMainThreadId;

   intptr_t callDispatcher(int opcode, int index,
      intptr_t value, void* ptr, float opt) override;

   intptr_t constCallDispatcher(int opcode, int index,
      intptr_t value, void* ptr, float opt) const;

   std::recursive_mutex mDispatcherLock;

   float callGetParameter(int index) const;

   void callSetChunk(bool isPgm, int len, void* buf);
   void callSetChunk(bool isPgm, int len, void* buf, VstPatchChunkInfo* info) const;


   int      GetString(wxString& outstr, int opcode, int index = 0) const;
   wxString GetString(int opcode, int index = 0) const;

   struct ParameterInfo
   {
      int      mID;
      wxString mName;
   };

   //! @return true  continue visiting
   //! @return false stop     visiting
   using ParameterVisitor = std::function< bool(const ParameterInfo& pi) >;

   void ForEachParameter(ParameterVisitor visitor) const;

   bool FetchSettings(VSTSettings& vst3Settings, bool doFetch=true) const;

   bool StoreSettings(const VSTSettings& vst3settings) const;

   VstPatchChunkInfo GetChunkInfo() const;

   bool IsCompatible(const VstPatchChunkInfo&) const;

   // These are here because they are used by the import/export methods
   int mVstVersion;
   wxString mName;

   // XML load/save
   bool mInSet;
   bool mInChunk;
   wxString mChunk;
   long mXMLVersion;
   VstPatchChunkInfo mXMLInfo;

   bool LoadXML(const wxFileName& fn);
   bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
   void HandleXMLEndTag(const std::string_view& tag) override;
   void HandleXMLContent(const std::string_view& content) override;
   XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

   void SetString(int opcode, const wxString& str, int index = 0);

   ComponentInterfaceSymbol GetSymbol() const;

   void callSetParameter(int index, float value) const;

   void SaveXML(const wxFileName& fn) const;

   // Other formats for import/export
   bool LoadFXB(const wxFileName& fn);
   bool LoadFXP(const wxFileName& fn);
   bool LoadFXProgram(unsigned char** bptr, ssize_t& len, int index, bool dryrun);
   void callSetProgram(int index);

   void SaveFXB(const wxFileName& fn) const;
   void SaveFXP(const wxFileName& fn) const;
   void SaveFXProgram(wxMemoryBuffer& buf, int index) const;


   intptr_t mCurrentEffectID {};
   
   bool Load();
   PluginPath   mPath;

   // Define a manager class for a handle to a module
#if defined(__WXMSW__)
   using ModuleHandle = std::unique_ptr<wxDynamicLibrary>;
#else
   struct ModuleDeleter {
      void operator() (void*) const;
   };
   using ModuleHandle = std::unique_ptr < char, ModuleDeleter >;
#endif
   ModuleHandle mModule{};

   wxString mVendor;
   wxString mDescription;
   int      mVersion;
   bool     mInteractive{ false };
   unsigned mAudioIns{ 0 };
   unsigned mAudioOuts{ 0 };
   int      mMidiIns{ 0 };
   int      mMidiOuts{ 0 };
   bool     mAutomatable;

   void Unload();

   void ResetModuleAndHandle();

#if defined(__WXMAC__)
   // These members must be ordered after mModule

   using BundleHandle = CF_ptr<CFBundleRef>;

   BundleHandle mBundleRef;

   struct ResourceHandle {
      ResourceHandle(
         CFBundleRef pHandle = nullptr, CFBundleRefNum num = 0)
         : mpHandle{ pHandle }, mNum{ num }
      {}
      ResourceHandle& operator=(ResourceHandle&& other)
      {
         if (this != &other) {
            mpHandle = other.mpHandle;
            mNum = other.mNum;
            other.mpHandle = nullptr;
            other.mNum = 0;
         }
         return *this;
      }
      ~ResourceHandle() { reset(); }
      void reset();

      CFBundleRef mpHandle{};
      CFBundleRefNum mNum{};
   };
   ResourceHandle mResource;
#endif

   
   VstTimeInfo* GetTimeInfo();
   float        GetSampleRate();
   VstTimeInfo  mTimeInfo;

   int mBufferDelay{ 0 };

   int GetProcessLevel();
   int mProcessLevel{ 1 };  // in GUI thread

   // The vst callback is currently called both for the effect and for instances.
   //
   static intptr_t AudioMaster(AEffect *effect,
                               int32_t opcode,
                               int32_t index,
                               intptr_t value,
                               void * ptr,
                               float opt);

   // Some of the methods called by the callback make sense for the Effect:
   //
   // - All GUI-related stuff 
   
   // Some other methods called by the callback make sense for Instances:
   virtual void SetBufferDelay(int samples);


   // Make message carrying all the information in settings, including chunks
   // This is called only on the main thread
   std::unique_ptr<EffectInstance::Message>
      MakeMessageFS(const VSTSettings& settings) const;

   // This is an immutable property determined once, when mAEffect is loaded
   // Whether the effect is capable of fancy native UI
   bool mGui{ false };
};

struct VSTMessage : EffectInstance::Message
{
   using ParamVector = std::vector<std::optional<double> >;

   // Make a message from a chunk and ID-value pairs
   explicit VSTMessage(std::vector<char> chunk, ParamVector params)
      : mChunk(std::move(chunk)),
        mParamsVec(std::move(params))
   {
   }

   // Make a message from a single parameter
   explicit VSTMessage(int id, double value, size_t numParams)
   {
      mParamsVec.resize(numParams, std::nullopt);
      if (id < numParams)
         mParamsVec[id] = value;
   }

   ~VSTMessage() override;

   std::unique_ptr<Message> Clone() const override;
   void Assign(Message&& src) override;
   void Merge(Message&& src) override;


   std::vector<char> mChunk;
   ParamVector       mParamsVec;

};

#endif

#endif // USE_VST
