/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveClip.h

  ?? Dominic Mazzoni
  ?? Markus Meyer

*******************************************************************/

#ifndef __AUDACITY_WAVECLIP__
#define __AUDACITY_WAVECLIP__

#include "Audacity.h"
#include "SampleFormat.h"
#include "Sequence.h"
#include "widgets/ProgressDialog.h"
#include "ondemand/ODTaskThread.h"
#include "xml/XMLTagHandler.h"

#include "Experimental.h"
#ifdef EXPERIMENTAL_USE_REALFFTF
#include "RealFFTf.h"
#endif

#include <wx/gdicmn.h>
#include <wx/longlong.h>
#include <wx/list.h>
#include <wx/msgdlg.h>

class Envelope;
class WaveCache;
class SpecCache;

class SpecPxCache {
public:
   SpecPxCache(int cacheLen)
   {
      len = cacheLen;
      values = new float[len];
      valid = false;
   }

   ~SpecPxCache()
   {
      delete[] values;
   }

   sampleCount  len;
   float       *values;
   bool         valid;
};

class WaveClip;

WX_DECLARE_USER_EXPORTED_LIST(WaveClip, WaveClipList, AUDACITY_DLL_API);
WX_DEFINE_USER_EXPORTED_ARRAY_PTR(WaveClip*, WaveClipArray, class AUDACITY_DLL_API);

class AUDACITY_DLL_API WaveClip : public XMLTagHandler
{
private:
   // It is an error to copy a WaveClip without specifying the DirManager.
   // We define these break-inducing single-arg methods so that
   // if some developer makes the mistake of calling a single-arg copy
   // constructor rather than the one below (that requires a DirManager*),
   // rather than it going to C++-generated default copy constructor,
   // it goes here and the error is made clear to that developer.
   WaveClip(const WaveClip&)
   {
      wxFAIL_MSG(wxT("It is an error to copy a WaveClip without specifying the DirManager."));
   };
   WaveClip& operator=(const WaveClip& orig)
   {
      WaveClip bogus(orig);
      return *this;
   }

public:
   // typical constructor
   WaveClip(DirManager *projDirManager, sampleFormat format, int rate);

   // essentially a copy constructor - but you must pass in the
   // current project's DirManager, because we might be copying
   // from one project to another
   WaveClip(WaveClip& orig, DirManager *projDirManager);

   virtual ~WaveClip();

   void ConvertToSampleFormat(sampleFormat format);

   void TimeToSamplesClip(double t0, sampleCount *s0) const;
   int GetRate() const { return mRate; }

   // Set rate without resampling. This will change the length of the clip
   void SetRate(int rate);

   // Resample clip. This also will set the rate, but without changing
   // the length of the clip
   bool Resample(int rate, ProgressDialog *progress = NULL);

   void SetOffset(double offset);
   double GetOffset() const { return mOffset; }
   void Offset(double delta) { SetOffset(GetOffset() + delta); }
   double GetStartTime() const;
   double GetEndTime() const;
   sampleCount GetStartSample() const;
   sampleCount GetEndSample() const;
   sampleCount GetNumSamples() const { return mSequence->GetNumSamples(); }

   // One and only one of the following is true for a given t (unless the clip
   // has zero length -- then BeforeClip() and AfterClip() can both be true).
   // Within() is true if the time is substantially within the clip
   bool WithinClip(double t) const;
   bool BeforeClip(double t) const;
   bool AfterClip(double t) const;

   bool GetSamples(samplePtr buffer, sampleFormat format,
                   sampleCount start, sampleCount len) const;
   bool SetSamples(samplePtr buffer, sampleFormat format,
                   sampleCount start, sampleCount len);

   Envelope* GetEnvelope() { return mEnvelope; }
   BlockArray* GetSequenceBlockArray() { return mSequence->GetBlockArray(); }

   // Get low-level access to the sequence. Whenever possible, don't use this,
   // but use more high-level functions inside WaveClip (or add them if you
   // think they are useful for general use)
   Sequence* GetSequence() { return mSequence; }

   /** WaveTrack calls this whenever data in the wave clip changes. It is
    * called automatically when WaveClip has a chance to know that something
    * has changed, like when member functions SetSamples() etc. are called. */
   void MarkChanged() { mDirty++; }

   /// Create clip from copy, discarding previous information in the clip
   bool CreateFromCopy(double t0, double t1, WaveClip* other);

   /** Getting high-level data from the for screen display and clipping
    * calculations and Contrast */
   bool GetWaveDisplay(float *min, float *max, float *rms,int* bl, sampleCount *where,
                       int numPixels, double t0, double pixelsPerSecond, bool &isLoadingOD);
   bool GetSpectrogram(float *buffer, sampleCount *where,
                       int numPixels,
                       double t0, double pixelsPerSecond,
                       bool autocorrelation);
   bool GetMinMax(float *min, float *max, double t0, double t1);
   bool GetRMS(float *rms, double t0, double t1);

   // Set/clear/get rectangle that this WaveClip fills on screen. This is
   // called by TrackArtist while actually drawing the tracks and clips.
   void ClearDisplayRect();
   void SetDisplayRect(const wxRect& r);
   void GetDisplayRect(wxRect* r);

   /** Whenever you do an operation to the sequence that will change the number
    * of samples (that is, the length of the clip), you will want to call this
    * function to tell the envelope about it. */
   void UpdateEnvelopeTrackLen();

   /// You must call Flush after the last Append
   bool Append(samplePtr buffer, sampleFormat format,
               sampleCount len, unsigned int stride=1,
               XMLWriter* blockFileLog = NULL);
   /// Flush must be called after last Append
   bool Flush();

   bool AppendAlias(wxString fName, sampleCount start,
                    sampleCount len, int channel,bool useOD);

   bool AppendCoded(wxString fName, sampleCount start,
                            sampleCount len, int channel, int decodeType);

   /// This name is consistent with WaveTrack::Clear. It performs a "Cut"
   /// operation (but without putting the cutted audio to the clipboard)
   bool Clear(double t0, double t1);

   /// Clear, and add cut line that starts at t0 and contains everything until t1.
   bool ClearAndAddCutLine(double t0, double t1);

   /// Paste data from other clip, resampling it if not equal rate
   bool Paste(double t0, WaveClip* other);

   /** Insert silence - note that this is an efficient operation for large
    * amounts of silence */
   bool InsertSilence(double t, double len);

   /// Get access to cut lines list
   WaveClipList* GetCutLines() { return &mCutLines; }

   /** Find cut line at (approximately) this position. Returns true and fills
    * in cutLineStart and cutLineEnd (if specified) if a cut line at this
    * position could be found. Return false otherwise. */
   bool FindCutLine(double cutLinePosition,
                    double* cutLineStart = NULL,
                    double *cutLineEnd = NULL);

   /** Expand cut line (that is, re-insert audio, then delete audio saved in
    * cut line). Returns true if a cut line could be found and sucessfully
    * expanded, false otherwise */
   bool ExpandCutLine(double cutLinePosition);

   /// Remove cut line, without expanding the audio in it
   bool RemoveCutLine(double cutLinePosition);
   void RemoveAllCutLines();

   /// Offset cutlines right to time 't0' by time amount 'len'
   void OffsetCutLines(double t0, double len);

   /// Lock all blockfiles
   void Lock();
   void CloseLock(); //similar to Lock but should be called when the project closes.
   /// Unlock all blockfiles
   void Unlock();

   ///Delete the wave cache - force redraw.  Thread-safe
   void DeleteWaveCache();

   ///Adds an invalid region to the wavecache so it redraws that portion only.
   void AddInvalidRegion(long startSample, long endSample);

   //
   // XMLTagHandler callback methods for loading and saving
   //

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual void HandleXMLEndTag(const wxChar *tag);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   virtual void WriteXML(XMLWriter &xmlFile);

   // Cache of values to colour pixels of Spectrogram - used by TrackArtist
   SpecPxCache    *mSpecPxCache;

   // AWD, Oct 2009: for pasting whitespace at the end of selection
   bool GetIsPlaceholder() { return mIsPlaceholder; };
   void SetIsPlaceholder(bool val) { mIsPlaceholder = val; };

protected:
   wxRect mDisplayRect;

   double mOffset;
   int mRate;
   int mDirty;
   bool mIsCutLine;
   Sequence *mSequence;
   Envelope *mEnvelope;

   WaveCache    *mWaveCache;
   ODLock       mWaveCacheMutex;
   SpecCache    *mSpecCache;
#ifdef EXPERIMENTAL_USE_REALFFTF
   // Variables used for computing the spectrum
   HFFT          hFFT;
   float         *mWindow;
   int           mWindowType;
   int           mWindowSize;
#endif
   samplePtr     mAppendBuffer;
   sampleCount   mAppendBufferLen;

   // Cut Lines are nothing more than ordinary wave clips, with the
   // offset relative to the start of the clip.
   WaveClipList mCutLines;

   // AWD, Oct. 2009: for whitespace-at-end-of-selection pasting
   bool mIsPlaceholder;
};

#endif
