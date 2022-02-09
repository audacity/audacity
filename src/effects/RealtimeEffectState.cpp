/**********************************************************************

  Audacity: A Digital Audio Editor

  @file RealtimeEffectState.cpp

  Paul Licameli split from RealtimeEffectManager.cpp

 *********************************************************************/

#include "RealtimeEffectState.h"

#include "EffectInterface.h"
#include "PluginManager.h"

RealtimeEffectState::RealtimeEffectState(const PluginID & id)
{
   SetID(id);
}

RealtimeEffectState::~RealtimeEffectState() = default;

void RealtimeEffectState::SetID(const PluginID & id)
{
   bool empty = id.empty();
   if (mID.empty() && !empty) {
      mID = id;
      GetEffect();
   }
   else
      // Set mID to non-empty at most once
      assert(empty);
}

EffectProcessor *RealtimeEffectState::GetEffect()
{
   if (!mEffect)
      mEffect = EffectFactory::Call(mID);
   return mEffect.get();
}

bool RealtimeEffectState::Suspend()
{
   ++mSuspendCount;
   return mSuspendCount != 1 || (mEffect && mEffect->RealtimeSuspend());
}

bool RealtimeEffectState::Resume() noexcept
{
   assert(mSuspendCount > 0);
   --mSuspendCount;
   return mSuspendCount != 0 || (mEffect && mEffect->RealtimeResume());
}

bool RealtimeEffectState::Initialize(double rate)
{
   if (!mEffect)
      return false;

   mCurrentProcessor = 0;
   mGroups.clear();
   mEffect->SetSampleRate(rate);
   return mEffect->RealtimeInitialize();
}

//! Set up processors to be visited repeatedly in Process.
/*! The iteration over channels in AddTrack and Process must be the same */
bool RealtimeEffectState::AddTrack(Track *track, unsigned chans, float rate)
{
   if (!mEffect)
      return false;

   auto ichans = chans;
   auto ochans = chans;
   auto gchans = chans;

   mGroups[track] = mCurrentProcessor;

   const auto numAudioIn = mEffect->GetAudioInCount();
   const auto numAudioOut = mEffect->GetAudioOutCount();

   // Call the client until we run out of input or output channels
   while (ichans > 0 && ochans > 0)
   {
      // If we don't have enough input channels to accommodate the client's
      // requirements, then we replicate the input channels until the
      // client's needs are met.
      if (ichans < numAudioIn)
      {
         // All input channels have been consumed
         ichans = 0;
      }
      // Otherwise fulfill the client's needs with as many input channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ichans >= numAudioIn)
      {
         gchans = numAudioIn;
         ichans -= gchans;
      }

      // If we don't have enough output channels to accommodate the client's
      // requirements, then we provide all of the output channels and fulfill
      // the client's needs with dummy buffers.  These will just get tossed.
      if (ochans < numAudioOut)
      {
         // All output channels have been consumed
         ochans = 0;
      }
      // Otherwise fulfill the client's needs with as many output channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ochans >= numAudioOut)
      {
         ochans -= numAudioOut;
      }

      // Add a NEW processor
      mEffect->RealtimeAddProcessor(gchans, rate);
      mCurrentProcessor++;
   }

   return true;
}

bool RealtimeEffectState::ProcessStart()
{
   if (!mEffect)
      return false;

   return mEffect->RealtimeProcessStart();
}

//! Visit the effect processors that were added in AddTrack
/*! The iteration over channels in AddTrack and Process must be the same */
size_t RealtimeEffectState::Process(Track *track,
                                    unsigned chans,
                                    float **inbuf,
                                    float **outbuf,
                                    size_t numSamples)
{
   if (!mEffect) {
      for (size_t ii = 0; ii < chans; ++ii)
         memcpy(outbuf[ii], inbuf[ii], numSamples * sizeof(float));
      return numSamples; // consider all samples to be trivially processed
   }

   // The caller passes the number of channels to process and specifies
   // the number of input and output buffers.  There will always be the
   // same number of output buffers as there are input buffers.
   //
   // Effects always require a certain number of input and output buffers,
   // so if the number of channels we're currently processing are different
   // than what the effect expects, then we use a few methods of satisfying
   // the effects requirements.
   const auto numAudioIn = mEffect->GetAudioInCount();
   const auto numAudioOut = mEffect->GetAudioOutCount();

   float **clientIn = (float **) alloca(numAudioIn * sizeof(float *));
   float **clientOut = (float **) alloca(numAudioOut * sizeof(float *));
   float *dummybuf = (float *) alloca(numSamples * sizeof(float));
   decltype(numSamples) len = 0;
   auto ichans = chans;
   auto ochans = chans;
   auto gchans = chans;
   unsigned indx = 0;
   unsigned ondx = 0;

   auto processor = mGroups[track];

   // Call the client until we run out of input or output channels
   while (ichans > 0 && ochans > 0)
   {
      // If we don't have enough input channels to accommodate the client's
      // requirements, then we replicate the input channels until the
      // client's needs are met.
      if (ichans < numAudioIn)
      {
         for (size_t i = 0; i < numAudioIn; i++)
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
      // Otherwise fulfill the client's needs with as many input channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ichans >= numAudioIn)
      {
         gchans = 0;
         for (size_t i = 0; i < numAudioIn; i++, ichans--, gchans++)
         {
            clientIn[i] = inbuf[indx++];
         }
      }

      // If we don't have enough output channels to accommodate the client's
      // requirements, then we provide all of the output channels and fulfill
      // the client's needs with dummy buffers.  These will just get tossed.
      if (ochans < numAudioOut)
      {
         for (size_t i = 0; i < numAudioOut; i++)
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
      // Otherwise fulfill the client's needs with as many output channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ochans >= numAudioOut)
      {
         for (size_t i = 0; i < numAudioOut; i++, ochans--)
         {
            clientOut[i] = outbuf[ondx++];
         }
      }

      // Finally call the plugin to process the block
      len = 0;
      const auto blockSize = mEffect->GetBlockSize();
      for (decltype(numSamples) block = 0; block < numSamples; block += blockSize)
      {
         auto cnt = std::min(numSamples - block, blockSize);
         len += mEffect->RealtimeProcess(processor, clientIn, clientOut, cnt);

         for (size_t i = 0 ; i < numAudioIn; i++)
         {
            clientIn[i] += cnt;
         }

         for (size_t i = 0 ; i < numAudioOut; i++)
         {
            clientOut[i] += cnt;
         }
      }
      processor++;
   }

   return len;
}

bool RealtimeEffectState::ProcessEnd()
{
   if (!mEffect)
      return false;

   return mEffect->RealtimeProcessEnd();
}

bool RealtimeEffectState::IsActive() const noexcept
{
   return mSuspendCount == 0;
}

bool RealtimeEffectState::Finalize() noexcept
{
   mGroups.clear();

   if (!mEffect)
      return false;

   return mEffect->RealtimeFinalize();
}

const std::string &RealtimeEffectState::XMLTag()
{
   static const std::string result{"effect"};
   return result;
}

static const auto idAttribute = "id";
static const auto versionAttribute = "version";
static const auto parametersAttribute = "parameters";
static const auto parameterAttribute = "parameter";
static const auto nameAttribute = "name";
static const auto valueAttribute = "value";

bool RealtimeEffectState::HandleXMLTag(
   const std::string_view &tag, const AttributesList &attrs)
{
   if (tag == XMLTag()) {
      mParameters.clear();
      mEffect.reset();
      mID.clear();

      for (auto pair : attrs) {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == idAttribute) {
            SetID(value.ToWString());
            if (!mEffect) {
               // TODO - complain!!!!
            }
         }
         else if (attr == versionAttribute) {
         }
      }

      return true;
   }
   else if (tag == parametersAttribute)
      return true;
   else if (tag == parameterAttribute) {
      wxString n;
      wxString v;

      for (auto pair : attrs) {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == nameAttribute)
            n = value.ToWString();
         else if (attr == valueAttribute)
            v = value.ToWString();
      }

      mParameters += wxString::Format(wxT("\"%s=%s\" "), n, v);

      return true;
   }
   else
      return false;
}

void RealtimeEffectState::HandleXMLEndTag(const std::string_view &tag)
{
   if (tag == XMLTag()) {
      if (mEffect && !mParameters.empty()) {
         CommandParameters parms(mParameters);
         mEffect->SetAutomationParameters(parms);
      }
      mParameters.clear();
   }
}

XMLTagHandler *RealtimeEffectState::HandleXMLChild(const std::string_view &tag)
{
   // Tag may be for the state, or the list of parameters, or for one parameter.
   // See the writing method below.  All are handled by this
   return this;
}

void RealtimeEffectState::WriteXML(XMLWriter &xmlFile)
{
   if (!mEffect)
      return;

   xmlFile.StartTag(XMLTag());
   xmlFile.WriteAttr(idAttribute, XMLWriter::XMLEsc(PluginManager::GetID(mEffect.get())));
   xmlFile.WriteAttr(versionAttribute, XMLWriter::XMLEsc(mEffect->GetVersion()));

   CommandParameters cmdParms;
   if (mEffect->GetAutomationParameters(cmdParms)) {
      xmlFile.StartTag(parametersAttribute);

      wxString entryName;
      long entryIndex;
      bool entryKeepGoing;

      entryKeepGoing = cmdParms.GetFirstEntry(entryName, entryIndex);
      while (entryKeepGoing) {
         wxString entryValue = cmdParms.Read(entryName, "");

         xmlFile.StartTag(parameterAttribute);
         xmlFile.WriteAttr(nameAttribute, XMLWriter::XMLEsc(entryName));
         xmlFile.WriteAttr(valueAttribute, XMLWriter::XMLEsc(entryValue));
         xmlFile.EndTag(parameterAttribute);

         entryKeepGoing = cmdParms.GetNextEntry(entryName, entryIndex);
      }

      xmlFile.EndTag(parametersAttribute);
   }

   xmlFile.EndTag(XMLTag());
}
