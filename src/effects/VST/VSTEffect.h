/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/

#include "../../Audacity.h"

#if USE_VST

#include "../Effect.h"

#include "aeffectx.h"

#define VSTCMDKEY wxT("-checkvst")
#define VSTPLUGINTYPE wxT("VST")

#define audacityVSTID CCONST('a', 'u', 'D', 'y');

typedef long (*dispatcherFn)(AEffect * effect, long opCode,
                             long index, long value, void *ptr,
                             float opt);

typedef void (*processFn)(AEffect * effect, float **inputs,
                          float **outputs, long sampleframes);

typedef void (*setParameterFn)(AEffect * effect, long index,
                               float parameter);

typedef float (*getParameterFn)(AEffect * effect, long index);

typedef AEffect *(*vstPluginMain)(audioMasterCallback audioMaster);

class VSTEffect:public Effect
{
 public:

   VSTEffect(const wxString & path);
   virtual ~VSTEffect();

   virtual wxString GetEffectName();

   virtual wxString GetEffectIdentifier();

   virtual std::set<wxString> GetEffectCategories();

   virtual wxString GetEffectAction();

   virtual bool Init();

   virtual bool PromptUser();
   
   virtual bool Process();
   
   virtual void End();

   // Plugin loading and unloading

   bool Load();
   void Unload();

   // Plugin probing

   static int Scan();
   static void Check(const wxChar *fname);

   static void ScanOnePlugin( const wxString & file );
   static int ShowPluginListDialog( const wxArrayString & files );
   static void ShowProgressDialog( const wxString & longest, const wxArrayString & files );

   // Utility methods

   int GetChannels();
   VstTimeInfo *GetTimeInfo();
   void SetBufferDelay(int samples);

   wxString GetString(int opcode, int index = 0);
   void SetString(int opcode, const wxString & str, int index = 0);

   // VST methods

   long callDispatcher(long opcode, long index, long value, void *ptr, float opt);
   void callProcess(float **inputs, float **outputs, long sampleframes);
   void callProcessReplacing(float **inputs, float **outputs, long sampleframes);
   void callSetParameter(long index, float parameter);
   float callGetParameter(long index);

 private:
   bool ProcessStereo(int count,
                      WaveTrack *left,
                      WaveTrack *right,
                      sampleCount lstart,
                      sampleCount rstart,
                      sampleCount len);

   wxString mPath;
#if defined(__WXMAC__)
   // Cheating a little ... type is really CFBundle
   void *mBundleRef;
   // Cheating a little ... type is really CFBundleRefNum
   int mResource;
#endif
   void *mModule;
   AEffect *mAEffect;

   wxString mVendor;
   wxString mName;

   VstTimeInfo mTimeInfo;

   bool mUseBufferDelay;
   int mBufferDelay;

   int mBufferSize;

   sampleCount mBlockSize;
   sampleCount mWTBlockSize;
   float **mInBuffer;
   float **mOutBuffer;
   int mInputs;
   int mOutputs;
   int mChannels;
};

void RegisterVSTEffects();

#endif // USE_VST
