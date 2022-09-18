/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistEnvironment.h

  Dominic Mazzoni

  Paul Licameli split from Nyquist.h

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST_ENVIRONMENT__
#define __AUDACITY_EFFECT_NYQUIST_ENVIRONMENT__

#include "Internat.h"
#include "SampleCount.h"

class Effect;
class WaveTrack;

//! A long lived object that creates short-lived RAII scope objects
class NyquistEnvironment {
public:
   bool              mRedirectOutput{ false };

   bool              mStop{ false };
   bool              mBreak{ false };
   bool              mCont{ false };

   // RAII for set-up and tear-down of processing of a selection
   class Scope_t {
      friend NyquistEnvironment;
      Scope_t(NyquistEnvironment &env, TranslatableString message);
      Scope_t(const Scope_t &) = delete;
      NyquistEnvironment &mEnv;
   public:
      ~Scope_t();
   };
   [[nodiscard]] Scope_t Scope(TranslatableString message)
      { return { *this, std::move(message) }; }

   // RAII for set-up and tear-down of processing of a track
   class Subscope_t {
      friend NyquistEnvironment;
      Subscope_t(NyquistEnvironment &env, Scope_t &);
      Subscope_t(const Scope_t &) = delete;
      NyquistEnvironment &mEnv;
   public:
      ~Subscope_t();
   };
   [[nodiscard]] Subscope_t Subscope(Scope_t &scope)
      { return { *this, scope }; }

   //! Get debug output, applying any needed translations
   const wxString &DebugOutput() const;

   //! Add translatable strings to the beginning of pending debug output
   void PrependDebug(TranslatableString message);

private:
   wxString          mDebugOutputStr;
   //! The complete translation of this string depends on mDebugOutputStr
   TranslatableString mDebugOutput;
   //! This caches the computation of mDebugOutput.Translation()
   mutable wxString mTranslation;

   void OutputCallback(int c);
   void OSCallback();

   static void StaticOutputCallback(int c, void *userdata);
   static void StaticOSCallback(void *userdata);
};

//! Transfers data both ways between WaveTrack and the sound object of Nyquist
class NyquistTrack {
public:
   using Buffer = std::unique_ptr<float[]>;

   NyquistTrack(Effect &effect, double scale)
      : mEffect{ effect }
      , mScale{ scale }
   {}

   Effect            &mEffect;

   WaveTrack *const *CurTracks() const { return mCurTrack; }
   sampleCount CurLength() const { return mCurLen; }
   unsigned CurNumChannels() const { return mCurNumChannels; }

   static int StaticGetCallback(float *buffer, int channel,
      int64_t start, int64_t len, int64_t totlen, void *userdata);

   static int StaticPutCallback(float *buffer, int channel,
      int64_t start, int64_t len, int64_t totlen, void *userdata);

   bool NextTrack(WaveTrack *pTrack, double t0, double t1, sampleCount maxLen);

   void AccumulateProgress()
   {
      mProgressTot += mProgressIn + mProgressOut;
      mProgressIn = 0.0;
      mProgressOut = 0.0;
   }

   //! Resets some state for the next pass
   //! May throw exceptions
   std::vector<std::shared_ptr<WaveTrack>>
   GetResult(unsigned outChannels, double &outputTime);

private:
   void NewOutputTrack(unsigned outChannels);

   const double      mScale;
   double            mProgressIn{ 0 };
   double            mProgressOut{ 0 };
   double            mProgressTot{ 0 };

   WaveTrack         *mCurTrack[2];
   sampleCount       mCurStart[2];
   sampleCount       mCurLen;
   unsigned          mCurNumChannels;

   std::shared_ptr<WaveTrack> mOutputTrack[2];
   Buffer            mCurBuffer[2];
   std::exception_ptr mpException{};

   int GetCallback(float *buffer, int channel,
      int64_t start, int64_t len, int64_t totlen);
   int PutCallback(float *buffer, int channel,
      int64_t start, int64_t len, int64_t totlen);

   sampleCount       mCurBufferStart[2];
   size_t            mCurBufferLen[2];
};
#endif
