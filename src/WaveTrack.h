/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_WAVETRACK__
#define __AUDACITY_WAVETRACK__

#include "Track.h"
#include "SampleFormat.h"
#include "widgets/ProgressDialog.h"

#include <vector>
#include <wx/gdicmn.h>
#include <wx/longlong.h>
#include <wx/thread.h>

#include "WaveTrackLocation.h"

class SpectrogramSettings;
class WaveformSettings;
class TimeWarper;

class CutlineHandle;
class SampleHandle;
class EnvelopeHandle;

class Sequence;
class WaveClip;

// Array of pointers that assume ownership
using WaveClipHolder = std::shared_ptr< WaveClip >;
using WaveClipHolders = std::vector < WaveClipHolder >;
using WaveClipConstHolders = std::vector < std::shared_ptr< const WaveClip > >;

// Temporary arrays of mere pointers
using WaveClipPointers = std::vector < WaveClip* >;
using WaveClipConstPointers = std::vector < const WaveClip* >;

//
// Tolerance for merging wave tracks (in seconds)
//
#define WAVETRACK_MERGE_POINT_TOLERANCE 0.01

/// \brief Structure to hold region of a wavetrack and a comparison function
/// for sortability.
struct Region
{
   Region() : start(0), end(0) {}
   Region(double start_, double end_) : start(start_), end(end_) {}

   double start, end;

   //used for sorting
   bool operator < (const Region &b) const
   {
      return this->start < b.start;
   }
};

using Regions = std::vector < Region >;

class Envelope;

class AUDACITY_DLL_API WaveTrack final : public PlayableTrack {
public:

   //
   // Constructor / Destructor / Duplicator
   //

   WaveTrack(const std::shared_ptr<DirManager> &projDirManager,
             sampleFormat format = (sampleFormat)0,
             double rate = 0);
   WaveTrack(const WaveTrack &orig);

   // overwrite data excluding the sample sequence but including display
   // settings
   void Reinit(const WaveTrack &orig);

private:
   void Init(const WaveTrack &orig);

   Track::Holder Duplicate() const override;

   friend class TrackFactory;

 public:

   typedef WaveTrackLocation Location;
   using Holder = std::shared_ptr<WaveTrack>;

   virtual ~WaveTrack();

   std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject, int currentTool, bool bMultiTool)
      override;

   double GetOffset() const override;
   void SetOffset(double o) override;
   virtual ChannelType GetChannelIgnoringPan() const;
   ChannelType GetChannel() const override;
   virtual void SetPanFromChannelType() override;

   /** @brief Get the time at which the first clip in the track starts
    *
    * @return time in seconds, or zero if there are no clips in the track
    */
   double GetStartTime() const override;

   /** @brief Get the time at which the last clip in the track ends, plus
    * recorded stuff
    *
    * @return time in seconds, or zero if there are no clips in the track.
    */
   double GetEndTime() const override;

   //
   // Identifying the type of track
   //

   //
   // WaveTrack parameters
   //

   double GetRate() const;
   void SetRate(double newRate);

   // Multiplicative factor.  Only converted to dB for display.
   float GetGain() const;
   void SetGain(float newGain);

   // -1.0 (left) -> 1.0 (right)
   float GetPan() const;
   void SetPan(float newPan) override;

   // Takes gain and pan into account
   float GetChannelGain(int channel) const;

   // Old gain is used in playback in linearly interpolating 
   // the gain.
   float GetOldChannelGain(int channel) const;
   void SetOldChannelGain(int channel, float gain);

   void DoSetMinimized(bool isMinimized) override;

   int GetWaveColorIndex() const { return mWaveColorIndex; };
   void SetWaveColorIndex(int colorIndex);

   sampleFormat GetSampleFormat() const { return mFormat; }
   void ConvertToSampleFormat(sampleFormat format);

   const SpectrogramSettings &GetSpectrogramSettings() const;
   SpectrogramSettings &GetSpectrogramSettings();
   SpectrogramSettings &GetIndependentSpectrogramSettings();
   void SetSpectrogramSettings(std::unique_ptr<SpectrogramSettings> &&pSettings);

   const WaveformSettings &GetWaveformSettings() const;
   WaveformSettings &GetWaveformSettings();
   WaveformSettings &GetIndependentWaveformSettings();
   void SetWaveformSettings(std::unique_ptr<WaveformSettings> &&pSettings);
   void UseSpectralPrefs( bool bUse=true );
   //
   // High-level editing
   //

   Track::Holder Cut(double t0, double t1) override;

   // If forClipboard is true,
   // and there is no clip at the end time of the selection, then the result
   // will contain a "placeholder" clip whose only purpose is to make
   // GetEndTime() correct.  This clip is not re-copied when pasting.
   Track::Holder Copy(double t0, double t1, bool forClipboard = true) const override;
   Track::Holder CopyNonconst(double t0, double t1) /* not override */;

   void Clear(double t0, double t1) override;
   void Paste(double t0, const Track *src) override;
   // May assume precondition: t0 <= t1
   void ClearAndPaste(double t0, double t1,
                              const Track *src,
                              bool preserve = true,
                              bool merge = true,
                              const TimeWarper *effectWarper = NULL) /* not override */;

   void Silence(double t0, double t1) override;
   void InsertSilence(double t, double len) override;

   void SplitAt(double t) /* not override */;
   void Split(double t0, double t1) /* not override */;
   // Track::Holder CutAndAddCutLine(double t0, double t1) /* not override */;
   // May assume precondition: t0 <= t1
   void ClearAndAddCutLine(double t0, double t1) /* not override */;

   Track::Holder SplitCut(double t0, double t1) /* not override */;
   // May assume precondition: t0 <= t1
   void SplitDelete(double t0, double t1) /* not override */;
   void Join(double t0, double t1) /* not override */;
   // May assume precondition: t0 <= t1
   void Disjoin(double t0, double t1) /* not override */;

   // May assume precondition: t0 <= t1
   void Trim(double t0, double t1) /* not override */;

   // May assume precondition: t0 <= t1
   void HandleClear(double t0, double t1, bool addCutLines, bool split);

   void SyncLockAdjust(double oldT1, double newT1) override;

   /** @brief Returns true if there are no WaveClips in the specified region
    *
    * @return true if no clips in the track overlap the specified time range,
    * false otherwise.
    */
   bool IsEmpty(double t0, double t1) const;

   /** @brief Append the sample data to the WaveTrack. You must call Flush()
    * after the last Append.
    *
    * If there is an existing WaveClip in the WaveTrack then the data is
    * appended to that clip. If there are no WaveClips in the track, then a NEW
    * one is created.
    */
   void Append(samplePtr buffer, sampleFormat format,
               size_t len, unsigned int stride=1,
               XMLWriter* blockFileLog=NULL);
   /// Flush must be called after last Append
   void Flush();

   void AppendAlias(const FilePath &fName, sampleCount start,
                    size_t len, int channel,bool useOD);

   ///for use with On-Demand decoding of compressed files.
   ///decodeType should be an enum from ODDecodeTask that specifies what
   ///Type of encoded file this is, such as eODFLAC
   //vvv Why not use the ODTypeEnum typedef to enforce that for the parameter?
   void AppendCoded(const FilePath &fName, sampleCount start,
                            size_t len, int channel, int decodeType);

   ///gets an int with OD flags so that we can determine which ODTasks should be run on this track after save/open, etc.
   unsigned int GetODFlags() const;

   ///Invalidates all clips' wavecaches.  Careful, This may not be threadsafe.
   void ClearWaveCaches();

   ///Adds an invalid region to the wavecache so it redraws that portion only.
   void  AddInvalidRegion(sampleCount startSample, sampleCount endSample);

   ///
   /// MM: Now that each wave track can contain multiple clips, we don't
   /// have a continous space of samples anymore, but we simulate it,
   /// because there are alot of places (e.g. effects) using this interface.
   /// This interface makes much sense for modifying samples, but note that
   /// it is not time-accurate, because the "offset" is a double value and
   /// therefore can lie inbetween samples. But as long as you use the
   /// same value for "start" in both calls to "Set" and "Get" it is
   /// guaranteed that the same samples are affected.
   ///
   bool Get(samplePtr buffer, sampleFormat format,
                   sampleCount start, size_t len,
                   fillFormat fill = fillZero, bool mayThrow = true, sampleCount * pNumCopied = nullptr) const;
   void Set(samplePtr buffer, sampleFormat format,
                   sampleCount start, size_t len);

   // Fetch envelope values corresponding to uniformly separated sample times
   // starting at the given time.
   void GetEnvelopeValues(double *buffer, size_t bufferLen,
                         double t0) const;

   // May assume precondition: t0 <= t1
   std::pair<float, float> GetMinMax(
      double t0, double t1, bool mayThrow = true) const;
   // May assume precondition: t0 <= t1
   float GetRMS(double t0, double t1, bool mayThrow = true) const;

   //
   // MM: We now have more than one sequence and envelope per track, so
   // instead of GetSequence() and GetEnvelope() we have the following
   // function which give the sequence and envelope which is under the
   // given X coordinate of the mouse pointer.
   //
   WaveClip* GetClipAtX(int xcoord);
   Sequence* GetSequenceAtX(int xcoord);
   Envelope* GetEnvelopeAtX(int xcoord);

   WaveClip* GetClipAtSample(sampleCount sample);
   WaveClip* GetClipAtTime(double time);

   //
   // Getting information about the track's internal block sizes
   // and alignment for efficiency
   //

   // This returns a possibly large or negative value
   sampleCount GetBlockStart(sampleCount t) const;

   // These return a nonnegative number of samples meant to size a memory buffer
   size_t GetBestBlockSize(sampleCount t) const;
   size_t GetMaxBlockSize() const;
   size_t GetIdealBlockSize();

   //
   // XMLTagHandler callback methods for loading and saving
   //

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   void HandleXMLEndTag(const wxChar *tag) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) const override;

   // Returns true if an error occurred while reading from XML
   bool GetErrorOpening() override;

   //
   // Lock and unlock the track: you must lock the track before
   // doing a copy and paste between projects.
   //

   bool Lock() const;
   bool Unlock() const;

   struct WaveTrackLockDeleter {
      inline void operator () (const WaveTrack *pTrack) { pTrack->Unlock(); }
   };
   using LockerBase = std::unique_ptr<
      const WaveTrack, WaveTrackLockDeleter
   >;

   // RAII object for locking.
   struct Locker : private LockerBase
   {
      friend LockerBase;
      Locker (const WaveTrack *pTrack)
         : LockerBase{ pTrack }
      { pTrack->Lock(); }
      Locker(Locker &&that) : LockerBase{std::move(that)} {}
      Locker &operator= (Locker &&that) {
         (LockerBase&)(*this) = std::move(that);
         return *this;
      }
   };

   bool CloseLock(); //similar to Lock but should be called when the project closes.
   // not balanced by unlocking calls.

   /** @brief Convert correctly between an (absolute) time in seconds and a number of samples.
    *
    * This method will not give the correct results if used on a relative time (difference of two
    * times). Each absolute time must be converted and the numbers of samples differenced:
    *    sampleCount start = track->TimeToLongSamples(t0);
    *    sampleCount end = track->TimeToLongSamples(t1);
    *    sampleCount len = (sampleCount)(end - start);
    * NOT the likes of:
    *    sampleCount len = track->TimeToLongSamples(t1 - t0);
    * See also WaveTrack::TimeToLongSamples().
    * @param t0 The time (floating point seconds) to convert
    * @return The number of samples from the start of the track which lie before the given time.
    */
   sampleCount TimeToLongSamples(double t0) const;
   /** @brief Convert correctly between an number of samples and an (absolute) time in seconds.
    *
    * @param pos The time number of samples from the start of the track to convert.
    * @return The time in seconds.
    */
   double LongSamplesToTime(sampleCount pos) const;

   // Get access to the (visible) clips in the tracks, in unspecified order
   // (not necessarioy sequenced in time).
   WaveClipHolders &GetClips() { return mClips; }
   const WaveClipConstHolders &GetClips() const
      { return reinterpret_cast< const WaveClipConstHolders& >( mClips ); }

   // Get access to all clips (in some unspecified sequence),
   // including those hidden in cutlines.
   class AllClipsIterator
      : public ValueIterator< WaveClip * >
   {
   public:
      // Constructs an "end" iterator
      AllClipsIterator () {}

      // Construct a "begin" iterator
      explicit AllClipsIterator( WaveTrack &track )
      {
         push( track.mClips );
      }

      WaveClip *operator * () const
      {
         if (mStack.empty())
            return nullptr;
         else
            return mStack.back().first->get();
      }

      AllClipsIterator &operator ++ ();

      // Define == well enough to serve for loop termination test
      friend bool operator ==
         (const AllClipsIterator &a, const AllClipsIterator &b)
      { return a.mStack.empty() == b.mStack.empty(); }

      friend bool operator !=
         (const AllClipsIterator &a, const AllClipsIterator &b)
      { return !( a == b ); }

   private:

      void push( WaveClipHolders &clips );

      using Iterator = WaveClipHolders::iterator;
      using Pair = std::pair< Iterator, Iterator >;
      using Stack = std::vector< Pair >;

      Stack mStack;
   };

   IteratorRange< AllClipsIterator > GetAllClips()
   {
      return { AllClipsIterator{ *this }, AllClipsIterator{ } };
   }

   // Create NEW clip and add it to this track. Returns a pointer
   // to the newly created clip.
   WaveClip* CreateClip();

   /** @brief Get access to the most recently added clip, or create a clip,
   *  if there is not already one.  THIS IS NOT NECESSARILY RIGHTMOST.
   *
   *  @return a pointer to the most recently added WaveClip
   */
   WaveClip* NewestOrNewClip();

   /** @brief Get access to the last (rightmost) clip, or create a clip,
   *  if there is not already one.
   *
   *  @return a pointer to a WaveClip at the end of the track
   */
   WaveClip* RightmostOrNewClip();

   // Get the linear index of a given clip (-1 if the clip is not found)
   int GetClipIndex(const WaveClip* clip) const;

   // Get the nth clip in this WaveTrack (will return NULL if not found).
   // Use this only in special cases (like getting the linked clip), because
   // it is much slower than GetClipIterator().
   WaveClip *GetClipByIndex(int index);
   const WaveClip* GetClipByIndex(int index) const;

   // Get number of clips in this WaveTrack
   int GetNumClips() const;

   // Add all wave clips to the given array 'clips' and sort the array by
   // clip start time. The array is emptied prior to adding the clips.
   WaveClipPointers SortedClipArray();
   WaveClipConstPointers SortedClipArray() const;

   // Before calling 'Offset' on a clip, use this function to see if the
   // offsetting is allowed with respect to the other clips in this track.
   // This function can optionally return the amount that is allowed for offsetting
   // in this direction maximally.
   bool CanOffsetClip(WaveClip* clip, double amount, double *allowedAmount=NULL);

   // Before moving a clip into a track (or inserting a clip), use this
   // function to see if the times are valid (i.e. don't overlap with
   // existing clips).
   bool CanInsertClip(WaveClip* clip, double &slideBy, double &tolerance);

   // Remove the clip from the track and return a SMART pointer to it.
   // You assume responsibility for its memory!
   std::shared_ptr<WaveClip> RemoveAndReturnClip(WaveClip* clip);

   // Append a clip to the track
   void AddClip(std::shared_ptr<WaveClip> &&clip); // Call using std::move

   // Merge two clips, that is append data from clip2 to clip1,
   // then remove clip2 from track.
   // clipidx1 and clipidx2 are indices into the clip list.
   void MergeClips(int clipidx1, int clipidx2);

   // Cache special locations (e.g. cut lines) for later speedy access
   void UpdateLocationsCache() const;

   // Get cached locations
   const std::vector<Location> &GetCachedLocations() const { return mDisplayLocationsCache; }

   // Expand cut line (that is, re-insert audio, then DELETE audio saved in cut line)
   void ExpandCutLine(double cutLinePosition, double* cutlineStart = NULL, double* cutlineEnd = NULL);

   // Remove cut line, without expanding the audio in it
   bool RemoveCutLine(double cutLinePosition);

   // This track has been merged into a stereo track.  Copy shared parameters
   // from the NEW partner.
   void Merge(const Track &orig) override;

   // Resample track (i.e. all clips in the track)
   void Resample(int rate, ProgressDialog *progress = NULL);

   //
   // AutoSave related
   //
   // Retrieve the unique autosave ID
   int GetAutoSaveIdent();
   // Set the unique autosave ID
   void SetAutoSaveIdent(int id);

   //
   // The following code will eventually become part of a GUIWaveTrack
   // and will be taken out of the WaveTrack class:
   //


   typedef int WaveTrackDisplay;
   enum WaveTrackDisplayValues : int {

      // DO NOT REORDER OLD VALUES!  Replace obsoletes with placeholders.

      Waveform = 0,
      MinDisplay = Waveform,

      obsoleteWaveformDBDisplay,

      Spectrum,

      obsolete1, // was SpectrumLogDisplay
      obsolete2, // was SpectralSelectionDisplay
      obsolete3, // was SpectralSelectionLogDisplay
      obsolete4, // was PitchDisplay

      // Add values here, and update MaxDisplay.

      MaxDisplay = Spectrum,

      NoDisplay,            // Preview track has no display
   };

   // Only two types of sample display for now, but
   // others (eg sinc interpolation) may be added later.
   enum SampleDisplay {
      LinearInterpolate = 0,
      StemPlot
   };

   // Various preset zooming levels.
   enum ZoomPresets {
      kZoomToFit = 0,
      kZoomToSelection,
      kZoomDefault,
      kZoomMinutes,
      kZoomSeconds,
      kZoom5ths,
      kZoom10ths,
      kZoom20ths,
      kZoom50ths,
      kZoom100ths,
      kZoom500ths,
      kZoomMilliSeconds,
      kZoomSamples,
      kZoom4To1,
      kMaxZoom,
   };

   // Handle remapping of enum values from 2.1.0 and earlier
   static WaveTrackDisplay ConvertLegacyDisplayValue(int oldValue);

   // Handle restriction of range of values of the enum from future versions
   static WaveTrackDisplay ValidateWaveTrackDisplay(WaveTrackDisplay display);

   int GetLastScaleType() const { return mLastScaleType; }
   void SetLastScaleType() const;

   int GetLastdBRange() const { return mLastdBRange; }
   void SetLastdBRange() const;

   WaveTrackDisplay GetDisplay() const { return mDisplay; }
   void SetDisplay(WaveTrackDisplay display) { mDisplay = display; }

   void GetDisplayBounds(float *min, float *max) const;
   void SetDisplayBounds(float min, float max) const;
   void GetSpectrumBounds(float *min, float *max) const;
   void SetSpectrumBounds(float min, float max) const;

   // For display purposes, calculate the y coordinate where the midline of
   // the wave should be drawn, if display minimum and maximum map to the
   // bottom and top.  Maybe that is out of bounds.
   int ZeroLevelYCoordinate(wxRect rect) const;

 protected:
   //
   // Protected variables
   //

   WaveClipHolders mClips;

   sampleFormat  mFormat;
   int           mRate;
   float         mGain;
   float         mPan;
   int           mWaveColorIndex;
   float         mOldGain[2];


   //
   // Data that should be part of GUIWaveTrack
   // and will be taken out of the WaveTrack class:
   //
   mutable float         mDisplayMin;
   mutable float         mDisplayMax;
   mutable float         mSpectrumMin;
   mutable float         mSpectrumMax;

   WaveTrackDisplay mDisplay;
   mutable int   mLastScaleType; // last scale type choice
   mutable int           mLastdBRange;
   mutable std::vector <Location> mDisplayLocationsCache;

   //
   // Protected methods
   //

private:

   TrackKind GetKind() const override { return TrackKind::Wave; }

   //
   // Private variables
   //

   wxCriticalSection mFlushCriticalSection;
   wxCriticalSection mAppendCriticalSection;
   double mLegacyProjectFileOffset;
   int mAutoSaveIdent;

   std::unique_ptr<SpectrogramSettings> mpSpectrumSettings;
   std::unique_ptr<WaveformSettings> mpWaveformSettings;

   std::weak_ptr<CutlineHandle> mCutlineHandle;
   std::weak_ptr<SampleHandle> mSampleHandle;
   std::weak_ptr<EnvelopeHandle> mEnvelopeHandle;

protected:
   std::shared_ptr<TrackControls> DoGetControls() override;
   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;
};

// This is meant to be a short-lived object, during whose lifetime,
// the contents of the WaveTrack are known not to change.  It can replace
// repeated calls to WaveTrack::Get() (each of which opens and closes at least
// one block file).
class WaveTrackCache {
public:
   WaveTrackCache()
      : mBufferSize(0)
      , mOverlapBuffer()
      , mNValidBuffers(0)
   {
   }

   explicit WaveTrackCache(const std::shared_ptr<const WaveTrack> &pTrack)
      : mBufferSize(0)
      , mOverlapBuffer()
      , mNValidBuffers(0)
   {
      SetTrack(pTrack);
   }
   ~WaveTrackCache();

   const std::shared_ptr<const WaveTrack>& GetTrack() const { return mPTrack; }
   void SetTrack(const std::shared_ptr<const WaveTrack> &pTrack);

   // Uses fillZero always
   // Returns null on failure
   // Returned pointer may be invalidated if Get is called again
   // Do not DELETE[] the pointer
   constSamplePtr Get(
      sampleFormat format, sampleCount start, size_t len, bool mayThrow);

private:
   void Free();

   struct Buffer {
      Floats data;
      sampleCount start;
      sampleCount len;

      Buffer() : start(0), len(0) {}
      void Free() { data.reset(); start = 0; len = 0; }
      sampleCount end() const { return start + len; }

      void swap ( Buffer &other )
      {
         data .swap ( other.data );
         std::swap( start, other.start );
         std::swap( len, other.len );
      }
   };

   std::shared_ptr<const WaveTrack> mPTrack;
   size_t mBufferSize;
   Buffer mBuffers[2];
   GrowableSampleBuffer mOverlapBuffer;
   int mNValidBuffers;
};

#endif // __AUDACITY_WAVETRACK__
