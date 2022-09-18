/*!********************************************************************

  Audacity: A Digital Audio Editor

  NyquistEnvironment.cpp

  Dominic Mazzoni

  Paul Licameli split from Nyquist.cpp

**********************************************************************/
#include "NyquistEnvironment.h"
#include "NyquistControls.h"
#include "../Effect.h"
#include "nyx.h"
#include "../../WaveTrack.h"
#include <iostream>

static void RegisterFunctions();

const wxString &NyquistEnvironment::DebugOutput() const
{
   if (mTranslation.empty())
      mTranslation = mDebugOutput.Translation();
   return mTranslation;
}

void NyquistEnvironment::PrependDebug(TranslatableString message)
{
   mDebugOutput = message + mDebugOutput;
   mTranslation.clear();
}

NyquistEnvironment::Scope_t::Scope_t(
   NyquistEnvironment &env, TranslatableString message
)  : mEnv{ env }
{
   mEnv.mDebugOutput = std::move(message);
   mEnv.mTranslation.clear();
}

NyquistEnvironment::Scope_t::~Scope_t()
{
   mEnv.mTranslation = wxString{};
}

NyquistEnvironment::Subscope_t::Subscope_t(NyquistEnvironment &env, Scope_t &
)  : mEnv{ env }
{
   RegisterFunctions();

   // Prepare to accumulate more debug output in OutputCallback
   // Assign the accumulated output, translating any translatables, to wxString
   mEnv.mDebugOutputStr = mEnv.DebugOutput();
   // Then this trick lets mDebugOutputStr have more raw characters appended,
   // while the + operator of TranslatableString allows more translatables
   // to be prepended; because TranslatableString::Format knows how to
   // capture a std::reference_wrapper differently from a by-value string and
   // do delayed interpolation only at Translate()
   mEnv.mDebugOutput = Verbatim( "%s" )
      .Format( std::cref( mEnv.mDebugOutputStr ) );
   // Clear the cached translation
   mEnv.mTranslation.clear();

   nyx_init();
   nyx_set_os_callback(NyquistEnvironment::StaticOSCallback, &mEnv);
   nyx_capture_output(NyquistEnvironment::StaticOutputCallback, &mEnv);
}

NyquistEnvironment::Subscope_t::~Subscope_t()
{
   nyx_capture_output(nullptr, nullptr);
   nyx_set_os_callback(nullptr, nullptr);
   nyx_cleanup();
}

bool NyquistTrack::NextTrack(
   WaveTrack *pTrack, double t0, double t1, const sampleCount maxLen)
{
   mCurNumChannels = 1;
   mCurTrack[0] = mCurTrack[1] = nullptr;
   if (pTrack) {
      mCurTrack[0] = pTrack;
      auto channels = TrackList::Channels(mCurTrack[0]);
      if (channels.size() > 1) {
         // TODO: more-than-two-channels
         // Pay attention to consistency of mNumSelectedChannels
         // with the running tally made in NyquistEffect::Process!
         mCurNumChannels = 2;

         mCurTrack[1] = * ++ channels.first;
         if (mCurTrack[1]->GetRate() != mCurTrack[0]->GetRate()) {
            mEffect.MessageBox(
               XO(
   "Sorry, cannot apply effect on stereo tracks where the tracks don't match."),
               wxOK | wxCENTRE );
            return false;
         }
         mCurStart[1] = mCurTrack[1]->TimeToLongSamples(t0);
      }

      mCurStart[0] = mCurTrack[0]->TimeToLongSamples(t0);
      auto end = mCurTrack[0]->TimeToLongSamples(t1);
      mCurLen = end - mCurStart[0];

      if (mCurLen > NYQ_MAX_LEN) {
         float hours = (float)NYQ_MAX_LEN / (44100 * 60 * 60);
         const auto message =
            XO(
   "Selection too long for Nyquist code.\nMaximum allowed selection is %ld samples\n(about %.1f hours at 44100 Hz sample rate).")
               .Format((long)NYQ_MAX_LEN, hours);
         mEffect.MessageBox(
            message,
            wxOK | wxCENTRE,
            XO("Nyquist Error") );
         return false;
      }
      mCurLen = std::min(mCurLen, maxLen);
   }
   return true;
}

void NyquistTrack::NewOutputTrack(unsigned outChannels)
{
   mpException = {};
   double rate = mCurTrack[0]->GetRate();
   unsigned i = 0;
   for (; i < outChannels; ++i) {
      if (outChannels == (int)mCurNumChannels)
         rate = mCurTrack[i]->GetRate();

      mOutputTrack[i] = mCurTrack[i]->EmptyCopy();
      mOutputTrack[i]->SetRate( rate );
   }
   for (; i < 2; ++i)
      mOutputTrack[i] = NULL;
}

std::vector<std::shared_ptr<WaveTrack>>
NyquistTrack::GetResult(unsigned outChannels, double &outputTime)
{
   for (auto &buffer : mCurBuffer)
      buffer.reset();

   NewOutputTrack(outChannels);

   // Now fully evaluate the sound
   int success = nyx_get_audio(NyquistTrack::StaticPutCallback, this);
   if (mpException)
      std::rethrow_exception(mpException);
   else if (!success)
      return {};

   std::vector<std::shared_ptr<WaveTrack>> result;
   for (auto &outputTrack : mOutputTrack) {
      if (!outputTrack)
         break;
      outputTrack->Flush();
      outputTime = outputTrack->GetEndTime();
      if (outputTime <= 0) {
         mEffect.MessageBox( XO("Nyquist returned nil audio.\n") );
         return {};
      }
      result.push_back(move(outputTrack));
   }
   return result;
}

int NyquistTrack::StaticGetCallback(float *buffer,
   int channel, int64_t start, int64_t len, int64_t totlen, void *userdata)
{
   auto This = static_cast<NyquistTrack *>(userdata);
   return This->GetCallback(buffer, channel, start, len, totlen);
}

int NyquistTrack::GetCallback(float *buffer, int ch,
   int64_t start, int64_t len, int64_t WXUNUSED(totlen))
{
   if (mCurBuffer[ch]) {
      if ((mCurStart[ch] + start) < mCurBufferStart[ch] ||
          (mCurStart[ch] + start)+len >
          mCurBufferStart[ch]+mCurBufferLen[ch]) {
         mCurBuffer[ch].reset();
      }
   }

   if (!mCurBuffer[ch]) {
      mCurBufferStart[ch] = (mCurStart[ch] + start);
      mCurBufferLen[ch] = mCurTrack[ch]->GetBestBlockSize(mCurBufferStart[ch]);

      if (mCurBufferLen[ch] < (size_t) len) {
         mCurBufferLen[ch] = mCurTrack[ch]->GetIdealBlockSize();
      }

      mCurBufferLen[ch] =
         limitSampleBufferSize( mCurBufferLen[ch],
                                mCurStart[ch] + mCurLen - mCurBufferStart[ch] );

      // C++20
      // mCurBuffer[ch] = std::make_unique_for_overwrite(mCurBufferLen[ch]);
      mCurBuffer[ch] = Buffer{ safenew float[ mCurBufferLen[ch] ] };
      try {
         mCurTrack[ch]->GetFloats( mCurBuffer[ch].get(),
            mCurBufferStart[ch], mCurBufferLen[ch]);
      }
      catch ( ... ) {
         // Save the exception object for re-throw when out of the library
         mpException = std::current_exception();
         return -1;
      }
   }

   // We have guaranteed above that this is nonnegative and bounded by
   // mCurBufferLen[ch]:
   auto offset = ( mCurStart[ch] + start - mCurBufferStart[ch] ).as_size_t();
   const void *src = &mCurBuffer[ch][offset];
   std::memcpy(buffer, src, len * sizeof(float));

   if (ch == 0) {
      double progress = mScale *
         ( (start+len)/ mCurLen.as_double() );

      if (progress > mProgressIn)
         mProgressIn = progress;

      if (mEffect.TotalProgress(mProgressIn + mProgressOut + mProgressTot))
         return -1;
   }

   return 0;
}

int NyquistTrack::StaticPutCallback(float *buffer,
   int channel, int64_t start, int64_t len, int64_t totlen, void *userdata)
{
   auto This = static_cast<NyquistTrack *>(userdata);
   return This->PutCallback(buffer, channel, start, len, totlen);
}

int NyquistTrack::PutCallback(
   float *buffer, int channel, int64_t start, int64_t len, int64_t totlen)
{
   // Don't let C++ exceptions propagate through the Nyquist library
   return GuardedCall<int>( [&] {
      if (channel == 0) {
         double progress = mScale*((float)(start+len)/totlen);

         if (progress > mProgressOut)
            mProgressOut = progress;

         if (mEffect.TotalProgress(mProgressIn + mProgressOut + mProgressTot))
            return -1;
      }

      mOutputTrack[channel]->Append((samplePtr)buffer, floatSample, len);

      return 0; // success
   }, MakeSimpleGuard( -1 ) ); // translate all exceptions into failure
}

void NyquistEnvironment::StaticOutputCallback(int c, void *This)
{
   (static_cast<NyquistEnvironment*>(This))->OutputCallback(c);
}

void NyquistEnvironment::OutputCallback(int c)
{
   // Always collect Nyquist error messages for normal plug-ins
   if (!mRedirectOutput) {
      mDebugOutputStr += (wxChar)c;
      // Invalidate any cached translation
      mTranslation.clear();
      return;
   }
   std::cout << (char)c;
}

void NyquistEnvironment::StaticOSCallback(void *This)
{
   (static_cast<NyquistEnvironment*>(This))->OSCallback();
}

void NyquistEnvironment::OSCallback()
{
   if (mStop) {
      mStop = false;
      nyx_stop();
   }
   else if (mBreak) {
      mBreak = false;
      nyx_break();
   }
   else if (mCont) {
      mCont = false;
      nyx_continue();
   }

   // LLL:  STF figured out that yielding while the effect is being applied
   //       produces an EXTREME slowdown.  It appears that yielding is not
   //       really necessary on Linux and Windows.
   //
   //       However, on the Mac, the spinning cursor appears during longer
   //       Nyquist processing and that may cause the user to think Audacity
   //       has crashed or hung.  In addition, yielding or not on the Mac
   //       doesn't seem to make much of a difference in execution time.
   //
   //       So, yielding on the Mac only...
#if defined(__WXMAC__)
   wxYieldIfNeeded();
#endif
}

// Registration of extra functions in XLisp.
#include "../../../lib-src/libnyquist/nyquist/xlisp/xlisp.h"

static LVAL gettext()
{
   auto string = UTF8CTOWX(getstring(xlgastring()));
#if !HAS_I18N_CONTEXTS
   // allow ignored context argument
   if ( moreargs() )
      nextarg();
#endif
   xllastarg();
   return cvstring(GetCustomTranslation(string).mb_str(wxConvUTF8));
}

static LVAL gettextc()
{
#if HAS_I18N_CONTEXTS
   auto string = UTF8CTOWX(getstring(xlgastring()));
   auto context = UTF8CTOWX(getstring(xlgastring()));
   xllastarg();
   return cvstring(wxGetTranslation( string, "", 0, "", context )
      .mb_str(wxConvUTF8));
#else
   return gettext();
#endif
}

static LVAL ngettext()
{
   auto string1 = UTF8CTOWX(getstring(xlgastring()));
   auto string2 = UTF8CTOWX(getstring(xlgastring()));
   auto number = getfixnum(xlgafixnum());
#if !HAS_I18N_CONTEXTS
   // allow ignored context argument
   if ( moreargs() )
      nextarg();
#endif
   xllastarg();
   return cvstring(
      wxGetTranslation(string1, string2, number).mb_str(wxConvUTF8));
}

static LVAL ngettextc()
{
#if HAS_I18N_CONTEXTS
   auto string1 = UTF8CTOWX(getstring(xlgastring()));
   auto string2 = UTF8CTOWX(getstring(xlgastring()));
   auto number = getfixnum(xlgafixnum());
   auto context = UTF8CTOWX(getstring(xlgastring()));
   xllastarg();
   return cvstring(wxGetTranslation( string1, string2, number, "", context )
      .mb_str(wxConvUTF8));
#else
   return ngettext();
#endif
}

void * nyq_make_opaque_string( int size, unsigned char *src ){
    LVAL dst;
    unsigned char * dstp;
    dst = new_string((int)(size+2));
    dstp = getstring(dst);

    /* copy the source to the destination */
    while (size-- > 0)
        *dstp++ = *src++;
    *dstp = '\0';

    return (void*)dst;
}

void * nyq_reformat_aud_do_response(const wxString & Str) {
   LVAL dst;
   LVAL message;
   LVAL success;
   wxString Left = Str.BeforeLast('\n').BeforeLast('\n').ToAscii();
   wxString Right = Str.BeforeLast('\n').AfterLast('\n').ToAscii();
   message = cvstring(Left);
   success = Right.EndsWith("OK") ? s_true : nullptr;
   dst = cons(message, success);
   return (void *)dst;
}

#include "../../commands/ScriptCommandRelay.h"


/* xlc_aud_do -- interface to C routine aud_do */
/**/
LVAL xlc_aud_do(void)
{
// Based on string-trim...
    unsigned char *leftp;
    LVAL src,dst;

    /* get the string */
    src = xlgastring();
    xllastarg();

    /* setup the string pointer */
    leftp = getstring(src);

    // Go call my real function here...
    dst = (LVAL)ExecForLisp( (char *)leftp );

    //dst = cons(dst, (LVAL)1);
    /* return the new string */
    return (dst);
}

static void RegisterFunctions()
{
   // Add functions to XLisp.  Do this only once,
   // before the first call to nyx_init.
   static bool firstTime = true;
   if (firstTime) {
      firstTime = false;

      // All function names must be UP-CASED
      static const FUNDEF functions[] = {
         { "_", SUBR, gettext },
         { "_C", SUBR, gettextc },
         { "NGETTEXT", SUBR, ngettext },
         { "NGETTEXTC", SUBR, ngettextc },
         { "AUD-DO",  SUBR, xlc_aud_do },
       };

      xlbindfunctions( functions, WXSIZEOF( functions ) );
   }
}
