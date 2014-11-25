/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

*******************************************************************//**

\class Effect
\brief Base class for many of the effects in Audacity.

*//****************************************************************//**

\class EffectDialog
\brief New (Jun-2006) base class for effects dialogs.  Likely to get
greater use in future.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/string.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/timer.h>
#include <wx/hashmap.h>

#include "audacity/ConfigInterface.h"

#include "Effect.h"
#include "../AudioIO.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../WaveTrack.h"
#include "../widgets/ProgressDialog.h"
#include "../ondemand/ODManager.h"
#include "TimeWarper.h"

WX_DECLARE_VOIDPTR_HASH_MAP( bool, t2bHash );

//
// public static methods
//

double Effect::sDefaultGenerateLen = 30.0;


wxString Effect::StripAmpersand(const wxString& str)
{
   wxString strippedStr = str;
   strippedStr.Replace(wxT("&"), wxT(""));
   // ! is used for hiding effects, and should not affect sort order.
   strippedStr.Replace(wxT("!"), wxT(""));
   return strippedStr;
}


//
// public methods
//

// Legacy (or full blown effect)
Effect::Effect()
{
   mParent = NULL;

   mClient = NULL;

   mWarper = NULL;

   mTracks = NULL;
   mOutputTracks = NULL;
   mOutputTracksType = Track::None;
   mDuration = 0.0;
   mNumTracks = 0;
   mNumGroups = 0;
   mProgress = NULL;

   // Can change effect flags later (this is the new way)
   // OR using the old way, over-ride GetEffectFlags().
   mFlags = BUILTIN_EFFECT | PROCESS_EFFECT | ADVANCED_EFFECT;

   mNumAudioIn = 0;
   mNumAudioOut = 0;

   mInBuffer = NULL;
   mOutBuffer = NULL;
   mInBufPos = NULL;
   mOutBufPos = NULL;

   mBufferSize = 0;
   mBlockSize = 0;
   mNumChannels = 0;
}

Effect::~Effect()
{
   if (mWarper != NULL)
   {
      delete mWarper;
   }
}

// EffectIdentInterface implementation

EffectType Effect::GetType()
{
   if (mClient)
   {
      return mClient->GetType();
   }

   if (mFlags & HIDDEN_EFFECT)
   {
      return EffectTypeNone;
   }

   if (mFlags & INSERT_EFFECT)
   {
      return EffectTypeGenerate;
   }

   if (mFlags & PROCESS_EFFECT)
   {
      return EffectTypeProcess;
   }

   if (mFlags & ANALYZE_EFFECT)
   {
      return EffectTypeAnalyze;
   }

   wxASSERT( true );

   return EffectTypeNone;
}

PluginID Effect::GetID()
{
   if (mClient)
   {
      return mClient->GetID();
   }

   return wxString::Format(wxT("LEGACY_EFFECT_ID_%d"), GetEffectID());
}

wxString Effect::GetPath()
{
   if (mClient)
   {
      return mClient->GetPath();
   }

   return wxEmptyString;
}

wxString Effect::GetName()
{
   if (mClient)
   {
      return mClient->GetName();
   }

   return GetEffectIdentifier();
}

wxString Effect::GetVendor()
{
   if (mClient)
   {
      return mClient->GetVendor();
   }

   return _("Audacity");
}

wxString Effect::GetVersion()
{
   if (mClient)
   {
      return mClient->GetVersion();
   }

   return wxT("Various");
}

wxString Effect::GetDescription()
{
   if (mClient)
   {
      return mClient->GetDescription();
   }

   return GetEffectIdentifier();
}

wxString Effect::GetFamily()
{
   if (mClient)
   {
      return mClient->GetFamily();
   }

   return _("Audacity");
}

bool Effect::IsInteractive()
{
   if (mClient)
   {
      return mClient->IsInteractive();
   }

   return GetEffectName().EndsWith(wxT("..."));
}

bool Effect::IsDefault()
{
   if (mClient)
   {
      return mClient->IsDefault();
   }

   return (mFlags & BUILTIN_EFFECT) != 0;
}

bool Effect::IsLegacy() 
{
   if (mClient)
   {
      return false;
   }

   return true;
}

bool Effect::SupportsAutomation()
{
   if (mClient)
   {
      return mClient->SupportsAutomation();
   }

   return SupportsChains();
}

// EffectHostInterface implementation

double Effect::GetDuration()
{
   if (mT1 > mT0)
   {
      return mT1 - mT0;
   }

   if (mClient->GetType() == EffectTypeGenerate)
   {
      return sDefaultGenerateLen;
   }

   return 0;
}

bool Effect::SetDuration(double seconds)
{
   mDuration = seconds;

   return true;
}

bool Effect::Apply()
{
   // This is absolute hackage...but easy
   //
   // It should callback to the EffectManager to kick off the processing
   GetActiveProject()->OnEffect(GetID(), true);

   return true;
}

void Effect::Preview()
{
   Preview(false);
}

wxDialog *Effect::CreateUI(wxWindow *parent, EffectUIClientInterface *client)
{
   EffectUIHost *dlg = new EffectUIHost(parent, this, client);

   if (dlg->Initialize())
   {
      return dlg;
   }

   delete dlg;

   return NULL;
}

wxString Effect::GetUserPresetsGroup(const wxString & name)
{
   wxString group = wxT("UserPresets");
   if (!name.IsEmpty())
   {
      group += wxCONFIG_PATH_SEPARATOR + name;
   }

   return group;
}

wxString Effect::GetCurrentSettingsGroup()
{
   return wxT("CurrentSettings");
}

wxString Effect::GetFactoryDefaultsGroup()
{
   return wxT("FactoryDefaults");
}

// ConfigClientInterface implementation

bool Effect::GetSharedConfigSubgroups(const wxString & group, wxArrayString & subgroups)
{
   return PluginManager::Get().GetSharedConfigSubgroups(GetID(), group, subgroups);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, wxString & value, const wxString & defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, int & value, int defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, bool & value, bool defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, float & value, float defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, double & value, double defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, sampleCount & value, sampleCount defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const wxString & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const int & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const bool & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const float & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const double & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const sampleCount & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::RemoveSharedConfigSubgroup(const wxString & group)
{
   return PluginManager::Get().RemoveSharedConfigSubgroup(GetID(), group);
}

bool Effect::RemoveSharedConfig(const wxString & group, const wxString & key)
{
   return PluginManager::Get().RemoveSharedConfig(GetID(), group, key);
}

bool Effect::GetPrivateConfigSubgroups(const wxString & group, wxArrayString & subgroups)
{
   return PluginManager::Get().GetPrivateConfigSubgroups(GetID(), group, subgroups);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, wxString & value, const wxString & defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, int & value, int defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, bool & value, bool defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, float & value, float defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, double & value, double defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, sampleCount & value, sampleCount defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const wxString & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const int & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const bool & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const float & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const double & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const sampleCount & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::RemovePrivateConfigSubgroup(const wxString & group)
{
   return PluginManager::Get().RemovePrivateConfigSubgroup(GetID(), group);
}

bool Effect::RemovePrivateConfig(const wxString & group, const wxString & key)
{
   return PluginManager::Get().RemovePrivateConfig(GetID(), group, key);
}

// Effect implementation

bool Effect::Startup(EffectClientInterface *client)
{
   // Let destructor know we need to be shutdown
   mClient = client;

   // Set host so client startup can use our services
   if (!mClient->SetHost(this))
   {
      // Bail if the client startup fails
      mClient = NULL;
      return false;
   }

   mNumAudioIn = mClient->GetAudioInCount();
   mNumAudioOut = mClient->GetAudioOutCount();

   int flags = PLUGIN_EFFECT;
   switch (mClient->GetType())
   {
      case EffectTypeGenerate:
         flags |= INSERT_EFFECT;
      break;

      case EffectTypeProcess:
         flags |= PROCESS_EFFECT;
      break;

      case EffectTypeAnalyze:
         flags |= INSERT_EFFECT;
      break;

      case EffectTypeNone:
         // Nothing to set
      break;
   }

   SetEffectFlags(flags);

   return true;
}

bool Effect::GetAutomationParameters(wxString & parms)
{
   if (mClient)
   {
      EffectAutomationParameters eap;
      if (!mClient->GetAutomationParameters(eap))
      {
         return false;
      }

      return eap.GetParameters(parms);
   }

   ShuttleCli shuttle;
   shuttle.mbStoreInClient = false;
   if (!TransferParameters(shuttle))
   {
      return false;
   }

   parms = shuttle.mParams;

   return true;
}

bool Effect::SetAutomationParameters(const wxString & parms)
{
   if (mClient)
   {
      EffectAutomationParameters eap;
      eap.SetParameters(parms);
      return mClient->SetAutomationParameters(eap);
   }

   ShuttleCli shuttle;
   shuttle.mParams = parms;
   shuttle.mbStoreInClient = true;
   return TransferParameters(shuttle);
}

// All legacy effects should have this overridden
wxString Effect::GetEffectName()
{
   if (mClient)
   {
      return mClient->GetName();
   }

   return wxT("DummyIdentifier");
}

// All legacy effects should have this overridden
wxString Effect::GetEffectIdentifier()
{
   if (mClient)
   {
      return mClient->GetName();
   }

   return wxT("DummyIdentifier");
}

// All legacy effects should have this overridden
wxString Effect::GetEffectAction()
{
   if (mClient)
   {
      return _("Applying ") + mClient->GetName();
   }

   return wxT("DummyName");
}

bool Effect::DoEffect(wxWindow *parent, int flags,
                      double projectRate,
                      TrackList *list,
                      TrackFactory *factory,
                      SelectedRegion *selectedRegion, wxString params)
{
   double t0 = selectedRegion->t0();
   double t1 = selectedRegion->t1();
   wxASSERT(t0 <= t1);

   if (mOutputTracks)
   {
      delete mOutputTracks;
      mOutputTracks = NULL;
   }

   mFactory = factory;
   mProjectRate = projectRate;
   mParent = parent;
   mTracks = list;
   mT0 = t0;
   mT1 = t1;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   mF0 = selectedRegion->f0();
   mF1 = selectedRegion->f1();
   wxArrayString Names;
   if( mF0 != SelectedRegion::UndefinedFrequency )
      Names.Add(wxT("control-f0"));
   if( mF1 != SelectedRegion::UndefinedFrequency )
      Names.Add(wxT("control-f1"));
   SetPresetParameters( &Names, NULL );

#endif
   CountWaveTracks();

   // Note: Init may read parameters from preferences
   if (!Init())
   {
      return false;
   }

   // If a parameter string was provided, it overrides any remembered settings
   // (but if the user is to be prompted, that takes priority)
   if (!params.IsEmpty())
   {
      ShuttleCli shuttle;
      shuttle.mParams = params;
      shuttle.mbStoreInClient=true;
      if( !TransferParameters( shuttle ))
      {
         wxMessageBox(
            wxString::Format(
               _("Could not set parameters of effect %s\n to %s."),
               GetEffectName().c_str(),
               params.c_str()
            )
         );
         return false;
      }
   }

   // Don't prompt user if we are dealing with a
   // effect that is already configured, e.g. repeating
   // the last effect on a different selection.
   if ((flags & CONFIGURED_EFFECT) == 0)
   {
      if (!PromptUser())
      {
         return false;
      }
   }

   bool returnVal = true;
   bool skipFlag = CheckWhetherSkipEffect();
   if (skipFlag == false)
   {
      mProgress = new ProgressDialog(StripAmpersand(GetEffectName()),
                                     GetEffectAction(),
                                     pdlgHideStopButton);
      returnVal = Process();
      delete mProgress;
      mProgress = NULL;
   }

   End();

   if (mOutputTracks)
   {
      delete mOutputTracks;
      mOutputTracks = NULL;
   }

   if (returnVal)
   {
      selectedRegion->setTimes(mT0, mT1);
   }

   return returnVal;
}

// All legacy effects should have this overridden
bool Effect::Init()
{
   return true;
}

bool Effect::PromptUser()
{
   return PromptUser(mParent);
}

bool Effect::PromptUser(wxWindow *parent, bool forceModal)
{
   if (mClient)
   {
      bool res = mClient->ShowInterface(parent, forceModal);

      // Really need to clean this up...should get easier when
      // all effects get converted.
      if (!res || (SupportsRealtime() && !forceModal))
      {
         // Return false to force DoEffect() to skip processing since
         // this UI has either been shown modeless or there was an error.
         return false;
      }
   }
   else
   {
      PromptUser();
   }

   return true;
}

// All legacy effects should have this overridden
bool Effect::Process()
{
   if (!mClient)
   {
      return false;
   }

   bool isGenerator = mClient->GetType() == EffectTypeGenerate;

   CopyInputTracks(Track::All);
   bool bGoodResult = true;

   mInBuffer = NULL;
   mOutBuffer = NULL;

   sampleCount prevBufferSize = 0;
   mBufferSize = 0;
   mBlockSize = 0;

   TrackListIterator iter(mOutputTracks);
   int count = 0;
   bool clear = false;
   Track* t = iter.First();

   for (t = iter.First(); t; t = iter.Next())
   {
      if (t->GetKind() != Track::Wave || !t->GetSelected())
      {
         if (t->IsSyncLockSelected())
         {
            t->SyncLockAdjust(mT1, mT0 + mDuration);
         }
         continue;
      }

      WaveTrack *left = (WaveTrack *)t;
      WaveTrack *right;
      sampleCount len;
      sampleCount leftStart;
      sampleCount rightStart;

      if (!isGenerator)
      {
         GetSamples(left, &leftStart, &len);
      }
      else
      {
         len = 0;
         leftStart = 0;
      }

      mNumChannels = 1;

      right = NULL;
      rightStart = 0;
      if (left->GetLinked() && mNumAudioIn > 1)
      {
         right = (WaveTrack *) iter.Next();
         if (!isGenerator)
         {
            GetSamples(right, &rightStart, &len);
         }
         clear = false;
         mNumChannels = 2;
      }

      // Let the client know the sample rate
      mClient->SetSampleRate(left->GetRate());

      // Get the block size the client wants to use
      sampleCount max = left->GetMaxBlockSize() * 2;
      mBlockSize = mClient->GetBlockSize(max);

      // Calculate the buffer size to be at least the max rounded up to the clients
      // selected block size.
      prevBufferSize = mBufferSize;
      mBufferSize = ((max + (mBlockSize - 1)) / mBlockSize) * mBlockSize;

      // If the buffer size has changed, then (re)allocate the buffers
      if (prevBufferSize != mBufferSize)
      {
         // Get rid of any previous buffers
         if (mInBuffer)
         {
            for (int i = 0; i < mNumAudioIn; i++)
            {
               if (mInBuffer[i])
               {
                  delete [] mInBuffer[i];
               }
            }
            delete [] mInBuffer;
            delete [] mInBufPos;
         }

         // Always create the number of input buffers the client expects even if we don't have
         // the same number of channels.
         mInBufPos = new float *[mNumAudioIn];
         mInBuffer = new float *[mNumAudioIn];
         for (int i = 0; i < mNumAudioIn; i++)
         {
            mInBuffer[i] = new float[mBufferSize];
         }

         // We won't be using more than the first 2 buffers, so clear the rest (if any)
         for (int i = 2; i < mNumAudioIn; i++)
         {
            for (int j = 0; j < mBufferSize; j++)
            {
               mInBuffer[i][j] = 0.0;
            }
         }

         // Get rid of any previous buffers
         if (mOutBuffer)
         {
            for (int i = 0; i < mNumAudioOut; i++)
            {
               if (mOutBuffer[i])
               {
                  delete [] mOutBuffer[i];
               }
            }
            delete [] mOutBuffer;
            delete [] mOutBufPos;
         }

         // Always create the number of output buffers the client expects even if we don't have
         // the same number of channels.
         mOutBufPos = new float *[mNumAudioOut];
         mOutBuffer = new float *[mNumAudioOut];
         for (int i = 0; i < mNumAudioOut; i++)
         {
            // Output buffers get an extra mBlockSize worth to give extra room if
            // the plugin adds latency
            mOutBuffer[i] = new float[mBufferSize + mBlockSize];
         }
      }

      // (Re)Set the input buffer positions
      for (int i = 0; i < mNumAudioIn; i++)
      {
         mInBufPos[i] = mInBuffer[i];
      }

      // (Re)Set the output buffer positions
      for (int i = 0; i < mNumAudioOut; i++)
      {
         mOutBufPos[i] = mOutBuffer[i];
      }

      // Clear unused input buffers
      if (!right && !clear && mNumAudioIn > 1)
      {
         for (int j = 0; j < mBufferSize; j++)
         {
            mInBuffer[1][j] = 0.0;
         }
         clear = true;
      }

      // Go process the track(s)
      bGoodResult = ProcessTrack(count, left, right, leftStart, rightStart, len);
      if (!bGoodResult)
      {
         break;
      }

      count++;
   }

   if (mOutBuffer)
   {
      for (int i = 0; i < mNumAudioOut; i++)
      {
         delete [] mOutBuffer[i];
      }
      delete [] mOutBuffer;
      delete [] mOutBufPos;
      mOutBuffer = NULL;
      mOutBufPos = NULL;
   }

   if (mInBuffer)
   {
      for (int i = 0; i < mNumAudioIn; i++)
      {
         delete [] mInBuffer[i];
      }
      delete [] mInBuffer;
      delete [] mInBufPos;
      mInBuffer = NULL;
      mInBufPos = NULL;
   }

   ReplaceProcessedTracks(bGoodResult); 

   return bGoodResult;
}

bool Effect::ProcessTrack(int count,
                          WaveTrack *left,
                          WaveTrack *right,
                          sampleCount leftStart,
                          sampleCount rightStart,
                          sampleCount len)
{
   bool rc = true;

   // Give the plugin a chance to initialize
   mClient->ProcessInitialize();

   // For each input block of samples, we pass it to the effect along with a
   // variable output location.  This output location is simply a pointer into a
   // much larger buffer.  This reduces the number of calls required to add the
   // samples to the output track.
   //
   // Upon return from the effect, the output samples are "moved to the left" by
   // the number of samples in the current latency setting, effectively removing any
   // delay introduced by the effect.
   //
   // At the same time the total number of delayed samples are gathered and when
   // there is no further input data to process, the loop continues to call the
   // effect with an empty input buffer until the effect has had a chance to 
   // return all of the remaining delayed samples.
   sampleCount inLeftPos = leftStart;
   sampleCount inRightPos = rightStart;
   sampleCount outLeftPos = leftStart;
   sampleCount outRightPos = rightStart;

   sampleCount inputRemaining = len;
   sampleCount delayRemaining = 0;
   sampleCount curBlockSize = 0;
   sampleCount curDelay = 0;

   sampleCount inputBufferCnt = 0;
   sampleCount outputBufferCnt = 0;
   bool cleared = false;

   WaveTrack *genLeft = NULL;
   WaveTrack *genRight = NULL;
   sampleCount genLength = 0;
   bool isGenerator = mClient->GetType() == EffectTypeGenerate;
   bool isProcessor = mClient->GetType() == EffectTypeProcess;
   if (isGenerator)
   {
      genLength = left->GetRate() * mDuration;
      delayRemaining = genLength;
      cleared = true;

      // Create temporary tracks
      genLeft = mFactory->NewWaveTrack(left->GetSampleFormat(), left->GetRate());
      if (right)
      {
         genRight = mFactory->NewWaveTrack(right->GetSampleFormat(), right->GetRate());
      }
   }

   // Call the effect until we run out of input or delayed samples
   while (inputRemaining || delayRemaining)
   {
      // Still working on the input samples
      if (inputRemaining)
      {
         // Need to refill the input buffers
         if (inputBufferCnt == 0)
         {
            // Calculate the number of samples to get
            inputBufferCnt = mBufferSize;
            if (inputBufferCnt > inputRemaining)
            {
               inputBufferCnt = inputRemaining;
            }

            // Fill the input buffers
            left->Get((samplePtr) mInBuffer[0], floatSample, inLeftPos, inputBufferCnt);
            if (right)
            {
               right->Get((samplePtr) mInBuffer[1], floatSample, inRightPos, inputBufferCnt);
            }

            // Reset the input buffer positions
            for (int i = 0; i < mNumChannels; i++)
            {
               mInBufPos[i] = mInBuffer[i];
            }
         }

         // Calculate the number of samples to process
         curBlockSize = mBlockSize;
         if (curBlockSize > inputRemaining)
         {
            // We've reached the last block...set current block size to what's left 
            curBlockSize = inputRemaining;
            inputRemaining = 0;

            // Clear the remainder of the buffers so that a full block can be passed
            // to the effect
            sampleCount cnt = mBlockSize - curBlockSize;
            for (int i = 0; i < mNumChannels; i++)
            {
               for (int j = 0 ; j < cnt; j++)
               {
                  mInBufPos[i][j + curBlockSize] = 0.0;
               }
            }

            // Might be able to use up some of the delayed samples
            if (delayRemaining)
            {
               // Don't use more than needed
               if (delayRemaining < cnt)
               {
                  cnt = delayRemaining;
               }
               delayRemaining -= cnt;
               curBlockSize += cnt;
            }
         }
      }
      // We've exhausted the input samples and are now working on the delay
      else if (delayRemaining)
      {
         // Calculate the number of samples to process
         curBlockSize = mBlockSize;
         if (curBlockSize > delayRemaining)
         {
            curBlockSize = delayRemaining;
         }
         delayRemaining -= curBlockSize;

         // From this point on, we only want to feed zeros to the plugin
         if (!cleared)
          {
            // Reset the input buffer positions
            for (int i = 0; i < mNumChannels; i++)
            {
               mInBufPos[i] = mInBuffer[i];

               // And clear
               for (int j = 0; j < mBlockSize; j++)
               {
                  mInBuffer[i][j] = 0.0;
               }
            }
            cleared = true;
         }
      }

      // Finally call the plugin to process the block
      try
      {
         mClient->ProcessBlock(mInBufPos, mOutBufPos, curBlockSize);
      }
      catch(...)
      {
         return false;
      }

      // Bump to next input buffer position
      if (inputRemaining)
      {
         for (int i = 0; i < mNumChannels; i++)
         {
            mInBufPos[i] += curBlockSize;
         }
         inputRemaining -= curBlockSize;
         inputBufferCnt -= curBlockSize;
      }

      // Get the current number of delayed samples and accumulate
      if (isProcessor)
      {
         sampleCount delay = mClient->GetLatency();
         curDelay += delay;
         delayRemaining += delay;

         // If the plugin has delayed the output by more samples than our current
         // block size, then we leave the output pointers alone.  This effectively
         // removes those delayed samples from the output buffer.
         if (curDelay >= curBlockSize)
         {
            curDelay -= curBlockSize;
            curBlockSize = 0;
         }
         // We have some delayed samples, at the beginning of the output samples,
         // so overlay them by shifting the remaining output samples.
         else if (curDelay > 0)
         {
            curBlockSize -= curDelay;
            for (int i = 0; i < wxMin(mNumAudioOut, mNumChannels); i++)
            {
               memmove(mOutBufPos[i], mOutBufPos[i] + curDelay, SAMPLE_SIZE(floatSample) * curBlockSize);
            }
            curDelay = 0;
         }
      }

      //
      outputBufferCnt += curBlockSize;

      //
      if (outputBufferCnt < mBufferSize)
      {
         // Bump to next output buffer position
         for (int i = 0; i < wxMin(mNumAudioOut, mNumChannels); i++)
         {
            mOutBufPos[i] += curBlockSize;
         }
      }
      // Output buffers have filled
      else
      {
         if (isProcessor)
         {
            // Write them out
            left->Set((samplePtr) mOutBuffer[0], floatSample, outLeftPos, outputBufferCnt);
            if (right)
            {
               right->Set((samplePtr) mOutBuffer[1], floatSample, outRightPos, outputBufferCnt);
            }
         }
         else if (isGenerator)
         {
            genLeft->Append((samplePtr) mOutBuffer[0], floatSample, outputBufferCnt);
            if (genRight)
            {
               genRight->Append((samplePtr) mOutBuffer[1], floatSample, outputBufferCnt);
            }
         }

         // Reset the output buffer positions
         for (int i = 0; i < wxMin(mNumAudioOut, mNumChannels); i++)
         {
            mOutBufPos[i] = mOutBuffer[i];
         }

         // Bump to the next track position
         outLeftPos += outputBufferCnt;
         outRightPos += outputBufferCnt;
         outputBufferCnt = 0;
      }

      // "ls" and "rs" serve as the input sample index for the left and
      // right channels when processing the input samples.  If we flip
      // over to processing delayed samples, they simply become counters
      // for the progress display.
      inLeftPos += curBlockSize;
      inRightPos += curBlockSize;

      if (mNumChannels > 1)
      {
         if (TrackGroupProgress(count, (inLeftPos - leftStart) / (double) len))
         {
            rc = false;
            break;
         }
      }
      else
      {
         if (TrackProgress(count, (inLeftPos - leftStart) / (double) len))
         {
            rc = false;
            break;
         }
      }
   }

   // Put any remaining output
   if (outputBufferCnt)
   {
      if (isProcessor)
      {
         left->Set((samplePtr) mOutBuffer[0], floatSample, outLeftPos, outputBufferCnt);
         if (right)
         {
            right->Set((samplePtr) mOutBuffer[1], floatSample, outRightPos, outputBufferCnt);
         }
      }
      else if (isGenerator)
      {
         genLeft->Append((samplePtr) mOutBuffer[0], floatSample, outputBufferCnt);
         if (genRight)
         {
            genRight->Append((samplePtr) mOutBuffer[1], floatSample, outputBufferCnt);
         }
      }
   }

   if (isGenerator)
   {
      // Transfer the data from the temporary tracks to the actual ones
      genLeft->Flush();
      SetTimeWarper(new StepTimeWarper(mT0 + genLength, genLength - (mT1 - mT0)));
      left->ClearAndPaste(mT0, mT1, genLeft, true, true, GetTimeWarper());
      delete genLeft;

      if (genRight)
      {
         genRight->Flush();
         right->ClearAndPaste(mT0, mT1, genRight, true, true, GetTimeWarper());
         delete genRight;
      }
   }

   // Allow the plugin to cleanup
   mClient->ProcessFinalize();

   return rc;
}

void Effect::End()
{
}

bool Effect::TotalProgress(double frac)
{
   int updateResult = (mProgress ?
      mProgress->Update(frac) :
      eProgressSuccess);
   return (updateResult != eProgressSuccess);
}

bool Effect::TrackProgress(int whichTrack, double frac, wxString msg)
{
   int updateResult = (mProgress ?
      mProgress->Update(whichTrack + frac, (double) mNumTracks, msg) :
      eProgressSuccess);
   return (updateResult != eProgressSuccess);
}

bool Effect::TrackGroupProgress(int whichGroup, double frac)
{
   int updateResult = (mProgress ?
      mProgress->Update(whichGroup + frac, (double) mNumGroups) :
      eProgressSuccess);
   return (updateResult != eProgressSuccess);
}

void Effect::GetSamples(WaveTrack *track, sampleCount *start, sampleCount *len)
{
   double trackStart = track->GetStartTime();
   double trackEnd = track->GetEndTime();
   double t0 = mT0 < trackStart ? trackStart : mT0;
   double t1 = mT1 > trackEnd ? trackEnd : mT1;

#if 0
   if (GetType() & INSERT_EFFECT) {
      t1 = t0 + mDuration;
      if (mT0 == mT1) {
         // Not really part of the calculation, but convenient to put here
         bool bResult = track->InsertSilence(t0, t1);
         wxASSERT(bResult); // TO DO: Actually handle this.
      }
   }
#endif

   if (t1 > t0) {
      *start = track->TimeToLongSamples(t0);
      sampleCount end = track->TimeToLongSamples(t1);
      *len = (sampleCount)(end - *start);
   }
   else {
      *start = 0;
      *len  = 0;
   }
}

void Effect::SetTimeWarper(TimeWarper *warper)
{
   if (mWarper != NULL)
   {
      delete mWarper;
      mWarper = NULL;
   }

   wxASSERT(warper != NULL);
   mWarper = warper;
}

TimeWarper *Effect::GetTimeWarper()
{
   wxASSERT(mWarper != NULL);
   return mWarper;
}

//
// private methods
//
// Use these two methods to copy the input tracks to mOutputTracks, if
// doing the processing on them, and replacing the originals only on success (and not cancel).
// Copy the group tracks that have tracks selected
void Effect::CopyInputTracks(int trackType)
{
   // Reset map
   mIMap.Clear();
   mOMap.Clear();

   mOutputTracks = new TrackList();
   mOutputTracksType = trackType;

   //iterate over tracks of type trackType (All types if Track::All)
   TrackListOfKindIterator aIt(trackType, mTracks);
   t2bHash added;

   for (Track *aTrack = aIt.First(); aTrack; aTrack = aIt.Next())
   {
      // Include selected tracks, plus sync-lock selected tracks for Track::All.
      if (aTrack->GetSelected() ||
            (trackType == Track::All && aTrack->IsSyncLockSelected()))
      {
         Track *o = aTrack->Duplicate();
         mOutputTracks->Add(o);
         mIMap.Add(aTrack);
         mOMap.Add(o);
      }
   }
}

void Effect::AddToOutputTracks(Track *t)
{
   mOutputTracks->Add(t);
   mIMap.Add(NULL);
   mOMap.Add(t);
}

// If bGoodResult, replace mTracks tracks with successfully processed mOutputTracks copies.
// Else clear and delete mOutputTracks copies.
void Effect::ReplaceProcessedTracks(const bool bGoodResult)
{
   wxASSERT(mOutputTracks != NULL); // Make sure we at least did the CopyInputTracks().

   if (!bGoodResult) {
      // Processing failed or was cancelled so throw away the processed tracks.
      mOutputTracks->Clear(true); // true => delete the tracks

      // Reset map
      mIMap.Clear();
      mOMap.Clear();

      //TODO:undo the non-gui ODTask transfer
      return;
   }

   TrackListIterator iterOut(mOutputTracks);

   Track *x;
   size_t cnt = mOMap.GetCount();
   size_t i = 0;

   for (Track *o = iterOut.First(); o; o = x, i++) {
      // If tracks were removed from mOutputTracks, then there will be
      // tracks in the map that must be removed from mTracks.
      while (i < cnt && mOMap[i] != o) {
         Track *t = (Track *) mIMap[i];
         if (t) {
            mTracks->Remove(t, true);
         }
         i++;
      }

      // This should never happen
      wxASSERT(i < cnt);

      // Remove the track from the output list...don't delete it
      x = iterOut.RemoveCurrent(false);

      Track *t = (Track *) mIMap[i];
      if (t == NULL)
      {
         // This track is a new addition to output tracks; add it to mTracks
         mTracks->Add(o);
      }
      else
      {
         // Replace mTracks entry with the new track
         mTracks->Replace(t, o, false);

         // Swap the wavecache track the ondemand task uses, since now the new
         // one will be kept in the project
         if (ODManager::IsInstanceCreated()) {
            ODManager::Instance()->ReplaceWaveTrack((WaveTrack *)t,
                                                    (WaveTrack *)o);
         }

         // No longer need the original track
         delete t;
      }
   }

   // If tracks were removed from mOutputTracks, then there may be tracks
   // left at the end of the map that must be removed from mTracks.
   while (i < cnt) {
      Track *t = (Track *) mIMap[i];
      if (t) {
         mTracks->Remove((Track *)mIMap[i], true);
      }
      i++;
   }

   // Reset map
   mIMap.Clear();
   mOMap.Clear();

   // Make sure we processed everything
   wxASSERT(iterOut.First() == NULL);

   // The output list is no longer needed
   delete mOutputTracks;
   mOutputTracks = NULL;
   mOutputTracksType = Track::None;
}

void Effect::CountWaveTracks()
{
   mNumTracks = 0;
   mNumGroups = 0;

   TrackListOfKindIterator iter(Track::Wave, mTracks);
   Track *t = iter.First();

   while(t) {
      if (!t->GetSelected()) {
         t = iter.Next();
         continue;
      }

      if (t->GetKind() == Track::Wave) {
         mNumTracks++;
         if (!t->GetLinked())
            mNumGroups++;
      }
      t = iter.Next();
   }
}

float TrapFloat(float x, float min, float max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

double TrapDouble(double x, double min, double max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

long TrapLong(long x, long min, long max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

double Effect::CalcPreviewInputLength(double previewLength)
{
   return previewLength;
}

wxString Effect::GetPreviewName()
{
   return _("Pre&view");
}

bool Effect::SupportsRealtime()
{
#if defined(EXPERIMENTAL_REALTIME_EFFECTS)
   if (mClient)
   {
      return mClient->SupportsRealtime();
   }
#endif

   return false;
}

bool Effect::RealtimeInitialize()
{
#if defined(EXPERIMENTAL_REALTIME_EFFECTS)
   if (mClient)
   {
      mHighGroup = -1;
      return mClient->RealtimeInitialize();
   }
#endif

   return false;
}

bool Effect::RealtimeFinalize()
{
#if defined(EXPERIMENTAL_REALTIME_EFFECTS)
   if (mClient)
   {
      return mClient->RealtimeFinalize();
   }
#endif

   return false;
}

bool Effect::RealtimeSuspend()
{
#if defined(EXPERIMENTAL_REALTIME_EFFECTS)
   if (mClient)
   {
      return mClient->RealtimeSuspend();
   }
#endif

   return false;
}

bool Effect::RealtimeResume()
{
#if defined(EXPERIMENTAL_REALTIME_EFFECTS)
   if (mClient)
   {
      return mClient->RealtimeResume();
   }
#endif

   return false;
}

sampleCount Effect::RealtimeProcess(int group,
                                    int chans,
                                    float rate,
                                    float **inbuf,
                                    float **outbuf,
                                    sampleCount numSamples)
{
#if defined(EXPERIMENTAL_REALTIME_EFFECTS)
   //
   // The caller passes the number of channels to process and specifies
   // the number of input and output buffers.  There will always be the
   // same number of output buffers as there are input buffers.
   //
   // Effects always require a certain number of input and output buffers,
   // so if the number of channels we're curently processing are different
   // that was the effect expects, then we use a few methods of satisfying
   // the effects requirements.
   float **clientIn = (float **) alloca(mNumAudioIn * sizeof(float *));
   float **clientOut = (float **) alloca(mNumAudioOut * sizeof(float *));
   float *dummybuf = (float *) alloca(numSamples * sizeof(float));
   sampleCount len = 0;
   int ichans = chans;
   int ochans = chans;
   int gchans = chans;
   int indx = 0;
   int ondx = 0;

   if (group == 0)
   {
      mCurrentGroup = 0;
   }

   // Call the client until we run out of input or output channels
   while (ichans > 0 && ochans > 0)
   {
      // If we don't have enough input channels to accomodate the client's
      // requirements, then we replicate the input channels until the
      // client's needs are met.
      if (ichans < mNumAudioIn)
      {
         for (int i = 0; i < mNumAudioIn; i++)
         {
            if (indx == ichans)
            {
               indx = 0;
            }
            clientIn[i] = inbuf[indx++];
         }

         // All input channels have been consumed
         ichans = 0;
      }
      // Otherwise fullfil the client's needs with as many input channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ichans >= mNumAudioIn)
      {
         gchans = 0;
         for (int i = 0; i < mNumAudioIn; i++, ichans--, gchans++)
         {
            clientIn[i] = inbuf[indx++];
         }
      }

      // If we don't have enough output channels to accomodate the client's
      // requirements, then we provide all of the output channels and fulfill
      // the client's needs with dummy buffers.  These will just get tossed.
      if (ochans < mNumAudioOut)
      {
         for (int i = 0; i < mNumAudioOut; i++)
         {
            if (i < ochans)
            {
               clientOut[i] = outbuf[i];
            }
            else
            {
               clientOut[i] = dummybuf;
            }
         }

         // All output channels have been consumed
         ochans = 0;
      }
      // Otherwise fullfil the client's needs with as many output channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ochans >= mNumAudioOut)
      {
         for (int i = 0; i < mNumAudioOut; i++, ochans--)
         {
            clientOut[i] = outbuf[ondx++];
         }
      }

      // If the current group hasn't yet been seen, then we must
      // add a new processor to handle this channel (sub)group
      if (mCurrentGroup > mHighGroup)
      {
         mClient->RealtimeAddProcessor(gchans, rate);
         mHighGroup = mCurrentGroup;
      }

      // Finally call the plugin to process the block
      len = 0;
      sampleCount maxBlock = mClient->GetBlockSize(numSamples);
      for (sampleCount block = 0; block < numSamples; block += maxBlock)
      {
         sampleCount cnt = (block + maxBlock > numSamples ? numSamples - block : maxBlock);
         len += mClient->RealtimeProcess(mCurrentGroup, clientIn, clientOut, cnt);

         for (int i = 0 ; i < mNumAudioIn; i++)
         {
            clientIn[i] += cnt;
         }

         for (int i = 0 ; i < mNumAudioOut; i++)
         {
            clientOut[i] += cnt;
         }
      }
      mCurrentGroup++;
   }

   return len;
#else
   return 0;
#endif
}

void Effect::Preview(bool dryOnly)
{
   if (mNumTracks==0) // nothing to preview
      return;

   wxWindow* FocusDialog = wxWindow::FindFocus();
   if (gAudioIO->IsBusy())
      return;

   // Mix a few seconds of audio from all of the tracks
   double previewLen = 6.0;
   gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLen);

   WaveTrack *mixLeft = NULL;
   WaveTrack *mixRight = NULL;
   double rate = mProjectRate;
   double t0 = mT0;
   double t1 = t0 + CalcPreviewInputLength(previewLen);

   if (t1 > mT1)
      t1 = mT1;

   // Generators can run without a selection.
   if (!GeneratorPreview() && (t1 <= t0))
      return;

   bool success = ::MixAndRender(mTracks, mFactory, rate, floatSample, t0, t1,
                                 &mixLeft, &mixRight);

   if (!success) {
      return;
   }

   // Save the original track list
   TrackList *saveTracks = mTracks;

   // Build new tracklist from rendering tracks
   mTracks = new TrackList();
   mixLeft->SetSelected(true);
   mixLeft->SetDisplay(WaveTrack::NoDisplay);
   mTracks->Add(mixLeft);
   if (mixRight) {
      mixRight->SetSelected(true);
      mTracks->Add(mixRight);
   }

   // Update track/group counts
   CountWaveTracks();

   // Reset times
   t0 = mixLeft->GetStartTime();
   t1 = mixLeft->GetEndTime();

   double t0save = mT0;
   double t1save = mT1;
   mT0 = t0;
   mT1 = t1;

   // Apply effect

   bool bSuccess(true);
   if (!dryOnly) {
      // Effect is already inited; we call Process, End, and then Init
      // again, so the state is exactly the way it was before Preview
      // was called.
      mProgress = new ProgressDialog(StripAmpersand(GetEffectName()),
            _("Preparing preview"),
            pdlgHideCancelButton); // Have only "Stop" button.
      bSuccess = Process();
      delete mProgress;
      mProgress = NULL;
      End();
      Init();
   }

   // Restore original selection
   mT0 = t0save;
   mT1 = t1save;

   if (bSuccess)
   {
      WaveTrackArray playbackTracks;
      WaveTrackArray recordingTracks;
      // Probably not the same tracks post-processing, so can't rely on previous values of mixLeft & mixRight.
      TrackListOfKindIterator iter(Track::Wave, mTracks);
      mixLeft = (WaveTrack*)(iter.First());
      mixRight = (WaveTrack*)(iter.Next());
      playbackTracks.Add(mixLeft);
      if (mixRight)
         playbackTracks.Add(mixRight);

      t1 = wxMin(mixLeft->GetEndTime(), t0 + previewLen);

#ifdef EXPERIMENTAL_MIDI_OUT
      NoteTrackArray empty;
#endif
      // Start audio playing
      int token =
         gAudioIO->StartStream(playbackTracks, recordingTracks,
#ifdef EXPERIMENTAL_MIDI_OUT
                               empty,
#endif
                               NULL, rate, t0, t1, NULL);

      if (token) {
         int previewing = eProgressSuccess;

         mProgress = new ProgressDialog(StripAmpersand(GetEffectName()),
                                        _("Previewing"), pdlgHideCancelButton);

         while (gAudioIO->IsStreamActive(token) && previewing == eProgressSuccess) {
            ::wxMilliSleep(100);
            previewing = mProgress->Update(gAudioIO->GetStreamTime() - t0, t1 - t0);
         }
         gAudioIO->StopStream();

         while (gAudioIO->IsBusy()) {
            ::wxMilliSleep(100);
         }

         delete mProgress;
         mProgress = NULL;
      }
      else {
         wxMessageBox(_("Error while opening sound device. Please check the playback device settings and the project sample rate."),
                      _("Error"), wxOK | wxICON_EXCLAMATION, FocusDialog);
      }
   }

   if (FocusDialog) {
      FocusDialog->SetFocus();
   }

   delete mOutputTracks;
   mOutputTracks = NULL;

   mTracks->Clear(true); // true => delete the tracks
   delete mTracks;

   mTracks = saveTracks;
}

int Effect::GetAudioInCount()
{
   if (mClient)
   {
      return mClient->GetAudioInCount();
   }

   return 0;
}

int Effect::GetAudioOutCount()
{
   if (mClient)
   {
      return mClient->GetAudioInCount();
   }

   return 0;
}

EffectDialog::EffectDialog(wxWindow * parent,
                           const wxString & title,
                           int type,
                           int flags,
                           int additionalButtons)
: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, flags)
{
   mType = type;
   mAdditionalButtons = additionalButtons;
}

void EffectDialog::Init()
{
   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      PopulateOrExchange(S);

      long buttons = eOkButton;

      if (mType == PROCESS_EFFECT || mType == INSERT_EFFECT)
      {
         buttons |= eCancelButton;
         if (mType == PROCESS_EFFECT)
         {
            buttons |= ePreviewButton;
         }
      }
      S.AddStandardButtons(buttons|mAdditionalButtons);
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

/// This is a virtual function which will be overridden to
/// provide the actual parameters that we want for each
/// kind of dialog.
void EffectDialog::PopulateOrExchange(ShuttleGui & WXUNUSED(S))
{
   return;
}

bool EffectDialog::TransferDataToWindow()
{
   ShuttleGui S(this, eIsSettingToDialog);
   PopulateOrExchange(S);

   return true;
}

bool EffectDialog::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   return true;
}

bool EffectDialog::Validate()
{
   return true;
}

void EffectDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   return;
}

///////////////////////////////////////////////////////////////////////////////
//
// EffectPanel
//
///////////////////////////////////////////////////////////////////////////////

class EffectPanel : public wxScrolledWindow
{
public:
   EffectPanel(wxWindow *parent)
   :  wxScrolledWindow(parent,                                               
                       wxID_ANY,
                       wxDefaultPosition,
                       wxDefaultSize,
                       wxVSCROLL | wxTAB_TRAVERSAL)
   {
   }

   virtual ~EffectPanel()
   {
   }

   // ============================================================================
   // wxWindow implementation
   // ============================================================================

   virtual bool AcceptsFocus() const
   {
      // Only accept focus if we're not a GUI host.
      //
      // This assumes that any effect will have more than one control in its
      // interface unless it is a GUI interface.  It's a fairly safe assumption.
#if defined(__WXMAC__)
      return GetChildren().GetCount() > 2;
#else      
      return GetChildren().GetCount() > 1;
#endif
   }
};

///////////////////////////////////////////////////////////////////////////////
//
// EffectUIHost
//
///////////////////////////////////////////////////////////////////////////////

enum
{
   kSaveAsID = 30001,
   kImportID = 30002,
   kExportID = 30003,
   kDefaultsID = 30004,
   kOptionsID = 30005,
   kDeleteAllID = 30006,
   kUserPresetsID = 31000,
   kDeletePresetID = 32000,
   kFactoryPresetsID = 33000,
};

BEGIN_EVENT_TABLE(EffectUIHost, wxDialog)
   EVT_CLOSE(EffectUIHost::OnClose)
   EVT_BUTTON(wxID_OK, EffectUIHost::OnOk)
   EVT_BUTTON(wxID_CANCEL, EffectUIHost::OnCancel)
   EVT_BUTTON(ePreviewID, EffectUIHost::OnPreview)
   EVT_BUTTON(eSettingsID, EffectUIHost::OnSettings)
   EVT_MENU(kSaveAsID, EffectUIHost::OnSaveAs)
   EVT_MENU(kImportID, EffectUIHost::OnImport)
   EVT_MENU(kExportID, EffectUIHost::OnExport)
   EVT_MENU(kOptionsID, EffectUIHost::OnOptions)
   EVT_MENU(kDefaultsID, EffectUIHost::OnDefaults)
   EVT_MENU(kDeleteAllID, EffectUIHost::OnDeleteAllPresets)
   EVT_MENU_RANGE(kUserPresetsID, kUserPresetsID + 999, EffectUIHost::OnUserPreset)
   EVT_MENU_RANGE(kDeletePresetID, kDeletePresetID + 999, EffectUIHost::OnDeletePreset)
   EVT_MENU_RANGE(kFactoryPresetsID, kFactoryPresetsID + 999, EffectUIHost::OnFactoryPreset)
END_EVENT_TABLE()

EffectUIHost::EffectUIHost(wxWindow *parent,
                           EffectHostInterface *host,
                           EffectUIClientInterface *client)
:  wxDialog(parent, wxID_ANY, host->GetName(),
   wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
{
   SetExtraStyle(wxWS_EX_VALIDATE_RECURSIVELY);

   mParent = parent;
   mHost = host;
   mClient = client;
   mClient->SetUIHost(this);
}

EffectUIHost::~EffectUIHost()
{
   if (mClient)
   {
      mClient->CloseUI();
      mClient = NULL;
   }
}

// ============================================================================
// EffectUIHost implementation
// ============================================================================

bool EffectUIHost::Initialize()
{
   wxBoxSizer *vs = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *hs = new wxBoxSizer(wxHORIZONTAL);

   EffectPanel *w = new EffectPanel(this);

   // Try to give the window a sensible default/minimum size
   w->SetMinSize(wxSize(wxMax(600, mParent->GetSize().GetWidth() * 2/3),
                        mParent->GetSize().GetHeight() / 2));

   w->SetScrollRate(0, 20);

   wxSizer *s = CreateStdButtonSizer(this, eSettingsButton | eOkButton | eCancelButton);

   hs->Add(w, 1, wxEXPAND);
   vs->Add(hs, 1, wxEXPAND);
   vs->Add(s, 0, wxEXPAND | wxALIGN_CENTER_VERTICAL);
   SetSizer(vs);

   if (!mClient->PopulateUI(w))
   {
      return false;
   }

   Layout();
   Fit();
   Center();

#if defined(__WXMAC__)
   (w->GetChildren().GetCount() > 2 ? w : FindWindowById(wxID_OK))->SetFocus();
#else
   (w->GetChildren().GetCount() > 1 ? w : FindWindowById(wxID_OK))->SetFocus();
#endif
 
   LoadUserPresets();

   mClient->LoadUserPreset(mHost->GetCurrentSettingsGroup());

   return true;
}

void EffectUIHost::OnClose(wxCloseEvent & WXUNUSED(evt))
{
   Hide();

   mClient->CloseUI();
   mClient = NULL;
   
   Destroy();
}

void EffectUIHost::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   if (!mClient->ValidateUI())
   {
      return;
   }

   mClient->SaveUserPreset(mHost->GetCurrentSettingsGroup());

   if (IsModal())
   {
      SetReturnCode(true);

      Close();

#if !defined(__WXGTK__)
      EndModal(true);
#endif
      return;
   }

   mHost->Apply();

   if (!mClient->HideUI())
   {
      return;
   }

   Hide();

   Close();

   return;
}

void EffectUIHost::OnCancel(wxCommandEvent & WXUNUSED(evt))
{
   if (IsModal())
   {
      SetReturnCode(false);

      Close();

#if !defined(__WXGTK__)
      EndModal(false);
#endif

      return;
   }

   Hide();

   Close();

   return;
}

void EffectUIHost::OnPreview(wxCommandEvent & WXUNUSED(evt))
{
   mHost->Preview();
}

void EffectUIHost::OnSettings(wxCommandEvent & evt)
{
   wxButton *b = (wxButton *)evt.GetEventObject();

   wxMenu *menu = new wxMenu();
   wxMenu *sub;

   LoadUserPresets();

   sub = new wxMenu();
   for (size_t i = 0, cnt = mUserPresets.GetCount(); i < cnt; i++)
   {
      sub->Append(kUserPresetsID + i, mUserPresets[i]);
   }
   menu->Append(0, _("User Presets"), sub);

   wxArrayString factory = mClient->GetFactoryPresets();

   sub = new wxMenu();
   sub->Append(kDefaultsID, _("Defaults"));
   if (factory.GetCount() > 0)
   {
      sub->AppendSeparator();
      for (size_t i = 0, cnt = factory.GetCount(); i < cnt; i++)
      {
         wxString label = factory[i];
         if (label.IsEmpty())
         {
            label = _("None");
         }

         sub->Append(kFactoryPresetsID + i, label);
      }
   }
   menu->Append(0, _("Factory Presets"), sub);

   sub = new wxMenu();
   for (size_t i = 0, cnt = mUserPresets.GetCount(); i < cnt; i++)
   {
      sub->Append(kDeletePresetID + i, mUserPresets[i]);
   }
   menu->Append(0, _("Delete Preset"), sub);

   menu->AppendSeparator();
   menu->Append(kSaveAsID, _("Save As..."));
   menu->AppendSeparator();
   menu->Append(kImportID, _("Import..."));
   menu->Append(kExportID, _("Export..."));
   menu->AppendSeparator();
   menu->Append(kOptionsID, _("Options..."));
   menu->AppendSeparator();

   sub = new wxMenu();

   sub->Append(0, wxString::Format(_("Type: %s"), mHost->GetFamily().c_str()));
   sub->Append(0, wxString::Format(_("Name: %s"), mHost->GetName().c_str()));
   sub->Append(0, wxString::Format(_("Version: %s"), mHost->GetVersion().c_str()));
   sub->Append(0, wxString::Format(_("Vendor: %s"), mHost->GetVendor().c_str()));
   sub->Append(0, wxString::Format(_("Description: %s"), mHost->GetDescription().c_str()));
//   sub->Append(0, wxString::Format(_("Audio In: %d"), mHost->GetAudioInCount()));
//   sub->Append(0, wxString::Format(_("Audio Out: %d"), mHost->GetAudioOutCount()));

   menu->Append(0, _("About"), sub);

   wxRect r = b->GetRect();
   PopupMenu(menu, wxPoint(r.GetLeft(), r.GetBottom()));

   delete menu;
}

void EffectUIHost::OnSaveAs(wxCommandEvent & WXUNUSED(evt))
{
   wxTextCtrl *text;
   wxString name;
   wxDialog dlg(this, wxID_ANY, wxString(_("Save Preset")));
   ShuttleGui S(&dlg, eIsCreating);

   S.StartPanel();
   {
      S.StartVerticalLay(0);
      {
         S.StartHorizontalLay(wxALIGN_LEFT, 0);
         {
            text = S.AddTextBox(_("Preset name:"), name, 30);
         }
         S.EndHorizontalLay();
         S.AddStandardButtons();
      }
      S.EndVerticalLay();
   }
   S.EndPanel();
   dlg.SetSize(dlg.GetSizer()->GetMinSize());
   dlg.Center();

   while (true)
   {
      int rc = dlg.ShowModal();

      if (rc != wxID_OK)
      {
         break;
      }

      name = text->GetValue();
      if (mUserPresets.Index(name) == wxNOT_FOUND)
      {
         mClient->SaveUserPreset(mHost->GetUserPresetsGroup(name));
         LoadUserPresets();
         break;
      }

      wxMessageBox(_("Preset already exists"),
                     _("Save Preset"),
                     wxICON_EXCLAMATION);
   }

   return;
}

void EffectUIHost::OnImport(wxCommandEvent & WXUNUSED(evt))
{
   mClient->ImportPresets();

   LoadUserPresets();

   return;
}

void EffectUIHost::OnExport(wxCommandEvent & WXUNUSED(evt))
{
   mClient->ExportPresets();

   return;
}

void EffectUIHost::OnDefaults(wxCommandEvent & WXUNUSED(evt))
{
   mClient->LoadFactoryDefaults();

   return;
}

void EffectUIHost::OnOptions(wxCommandEvent & WXUNUSED(evt))
{
   mClient->ShowOptions();

   return;
}

void EffectUIHost::OnUserPreset(wxCommandEvent & evt)
{
   int preset = evt.GetId() - kUserPresetsID;

   mClient->LoadUserPreset(mHost->GetUserPresetsGroup(mUserPresets[preset]));

   return;
}

void EffectUIHost::OnFactoryPreset(wxCommandEvent & evt)
{
   mClient->LoadFactoryPreset(evt.GetId() - kFactoryPresetsID);

   return;
}

void EffectUIHost::OnDeletePreset(wxCommandEvent & evt)
{
   mHost->RemovePrivateConfigSubgroup(mHost->GetUserPresetsGroup(mUserPresets[evt.GetId() - kDeletePresetID]));

   LoadUserPresets();

   return;
}

void EffectUIHost::OnDeleteAllPresets(wxCommandEvent & WXUNUSED(evt))
{
   int res = wxMessageBox(_("This will delete all of your user presets.  Are you sure?"),
                          _("Delete All Presets"),
                          wxICON_QUESTION | wxYES_NO);
   if (res == wxID_YES)
   {
      mHost->RemovePrivateConfigSubgroup(mHost->GetUserPresetsGroup(wxEmptyString));
      mUserPresets.Clear();
   }

   return;
}

void EffectUIHost::LoadUserPresets()
{
   mUserPresets.Clear();

   mHost->GetPrivateConfigSubgroups(mHost->GetUserPresetsGroup(wxEmptyString), mUserPresets);

   mUserPresets.Sort();

   return;
}
