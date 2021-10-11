/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_WAVETRACK__
#define __AUDACITY_WAVETRACK__

#include "Track.h"
#include "SampleCount.h"

#include <vector>
#include <functional>
#include <wx/longlong.h>

#include "WaveTrackLocation.h"

class ProgressDialog;

class SampleBlockFactory;
using SampleBlockFactoryPtr = std::shared_ptr<SampleBlockFactory>;

class SpectrogramSettings;
class WaveformSettings;
class TimeWarper;

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

   WaveTrack(
      const SampleBlockFactoryPtr &pFactory, sampleFormat format, double rate);
   WaveTrack(const WaveTrack &orig);

   // overwrite data excluding the sample sequence but including display
   // settings
   void Reinit(const WaveTrack &orig);

private:
   void Init(const WaveTrack &orig);

   Track::Holder Clone() const override;

   friend class WaveTrackFactory;

   wxString MakeClipCopyName(const wxString& originalName) const;
   wxString MakeNewClipName() const;
 public:

   typedef WaveTrackLocation Location;
   using Holder = std::shared_ptr<WaveTrack>;

   virtual ~WaveTrack();

   double GetOffset() const override;
   void SetOffset(double o) override;
   virtual ChannelType GetChannelIgnoringPan() const;
   ChannelType GetChannel() const override;
   virtual void SetPanFromChannelType() override;

   bool LinkConsistencyCheck() override;

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

   int GetWaveColorIndex() const { return mWaveColorIndex; };
   void SetWaveColorIndex(int colorIndex);

   sampleCount GetPlaySamplesCount() const;
   sampleCount GetSequenceSamplesCount() const;

   sampleFormat GetSampleFormat() const { return mFormat; }
   void ConvertToSampleFormat(sampleFormat format,
      const std::function<void(size_t)> & progressReport = {});

   const SpectrogramSettings &GetSpectrogramSettings() const;
   SpectrogramSettings &GetSpectrogramSettings();
   SpectrogramSettings &GetIndependentSpectrogramSettings();
   void SetSpectrogramSettings(std::unique_ptr<SpectrogramSettings> &&pSettings);

   const WaveformSettings &GetWaveformSettings() const;
   WaveformSettings &GetWaveformSettings();
   void SetWaveformSettings(std::unique_ptr<WaveformSettings> &&pSettings);
   void UseSpectralPrefs( bool bUse=true );
   //
   // High-level editing
   //

   Track::Holder Cut(double t0, double t1) override;

   // Make another track copying format, rate, color, etc. but containing no
   // clips
   // It is important to pass the correct factory (that for the project
   // which will own the copy) in the unusual case that a track is copied from
   // another project or the clipboard.  For copies within one project, the
   // default will do.
   Holder EmptyCopy(const SampleBlockFactoryPtr &pFactory = {} ) const;

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
    *
    * @return true if at least one complete block was created
    */
   bool Append(constSamplePtr buffer, sampleFormat format,
               size_t len, unsigned int stride=1);
   /// Flush must be called after last Append
   void Flush();

   ///Invalidates all clips' wavecaches.  Careful, This may not be threadsafe.
   void ClearWaveCaches();

   ///
   /// MM: Now that each wave track can contain multiple clips, we don't
   /// have a continuous space of samples anymore, but we simulate it,
   /// because there are a lot of places (e.g. effects) using this interface.
   /// This interface makes much sense for modifying samples, but note that
   /// it is not time-accurate, because the "offset" is a double value and
   /// therefore can lie inbetween samples. But as long as you use the
   /// same value for "start" in both calls to "Set" and "Get" it is
   /// guaranteed that the same samples are affected.
   ///

   //! Retrieve samples from a track in floating-point format, regardless of the storage format
   /*!
    @param buffer receives the samples
    @param start starting sample, relative to absolute time zero (not to the track's offset value)
    @param len how many samples to get.  buffer is assumed sufficiently large
    @param fill how to assign values for sample positions between clips
    @param mayThrow if false, fill buffer with zeros when there is failure to retrieve samples; else throw
    @param[out] pNumWithinClips Report how many samples were copied from within clips, rather
       than filled according to fillFormat; but these were not necessarily one contiguous range.
    */
   bool GetFloats(float *buffer, sampleCount start, size_t len,
      fillFormat fill = fillZero, bool mayThrow = true,
      sampleCount * pNumWithinClips = nullptr) const
   {
      //! Cast the pointer to pass it to Get() which handles multiple destination formats
      return Get(reinterpret_cast<samplePtr>(buffer),
         floatSample, start, len, fill, mayThrow, pNumWithinClips);
   }

   //! Retrieve samples from a track in a specified format
   /*!
    @copydetails WaveTrack::GetFloats()
    @param format sample format of the destination buffer
    */
   bool Get(samplePtr buffer, sampleFormat format,
      sampleCount start, size_t len,
      fillFormat fill = fillZero,
      bool mayThrow = true,
      // Report how many samples were copied from within clips, rather than
      // filled according to fillFormat; but these were not necessarily one
      // contiguous range.
      sampleCount * pNumWithinClips = nullptr) const;

   void Set(constSamplePtr buffer, sampleFormat format,
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
   // function which give the sequence and envelope which contains the given
   // time.
   //
   Sequence* GetSequenceAtTime(double time);
   Envelope* GetEnvelopeAtTime(double time);

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

   bool CloseLock(); //should be called when the project closes.
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
   /** @brief Convert correctly between a number of samples and an (absolute) time in seconds.
    *
    * @param pos The time number of samples from the start of the track to convert.
    * @return The time in seconds.
    */
   double LongSamplesToTime(sampleCount pos) const;

   // Get access to the (visible) clips in the tracks, in unspecified order
   // (not necessarily sequenced in time).
   WaveClipHolders &GetClips() { return mClips; }
   const WaveClipConstHolders &GetClips() const
      { return reinterpret_cast< const WaveClipConstHolders& >( mClips ); }

   // Get mutative access to all clips (in some unspecified sequence),
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
      friend bool operator == (
         const AllClipsIterator &a, const AllClipsIterator &b)
      { return a.mStack.empty() == b.mStack.empty(); }

      friend bool operator != (
         const AllClipsIterator &a, const AllClipsIterator &b)
      { return !( a == b ); }

   private:

      void push( WaveClipHolders &clips );

      using Iterator = WaveClipHolders::iterator;
      using Pair = std::pair< Iterator, Iterator >;
      using Stack = std::vector< Pair >;

      Stack mStack;
   };

   // Get const access to all clips (in some unspecified sequence),
   // including those hidden in cutlines.
   class AllClipsConstIterator
      : public ValueIterator< const WaveClip * >
   {
   public:
      // Constructs an "end" iterator
      AllClipsConstIterator () {}

      // Construct a "begin" iterator
      explicit AllClipsConstIterator( const WaveTrack &track )
         : mIter{ const_cast< WaveTrack& >( track ) }
      {}

      const WaveClip *operator * () const
      { return *mIter; }

      AllClipsConstIterator &operator ++ ()
      { ++mIter; return *this; }

      // Define == well enough to serve for loop termination test
      friend bool operator == (
         const AllClipsConstIterator &a, const AllClipsConstIterator &b)
      { return a.mIter == b.mIter; }

      friend bool operator != (
         const AllClipsConstIterator &a, const AllClipsConstIterator &b)
      { return !( a == b ); }

   private:
      AllClipsIterator mIter;
   };

   IteratorRange< AllClipsIterator > GetAllClips()
   {
      return { AllClipsIterator{ *this }, AllClipsIterator{ } };
   }
   
   IteratorRange< AllClipsConstIterator > GetAllClips() const
   {
      return { AllClipsConstIterator{ *this }, AllClipsConstIterator{ } };
   }
   
   // Create NEW clip and add it to this track. Returns a pointer
   // to the newly created clip. Optionally initial offset and
   // clip name may be provided
   WaveClip* CreateClip(double offset = .0, const wxString& name = wxEmptyString);

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

   //! Decide whether the clips could be offset (and inserted) together without overlapping other clips
   /*!
   @return true if possible to offset by `(allowedAmount ? *allowedAmount : amount)`
    */
   bool CanOffsetClips(
      const std::vector<WaveClip*> &clips, //!< not necessarily in this track
      double amount, //!< signed
      double *allowedAmount = nullptr /*!<
         [out] if null, test exact amount only; else, largest (in magnitude) possible offset with same sign */
   );

   // Before moving a clip into a track (or inserting a clip), use this
   // function to see if the times are valid (i.e. don't overlap with
   // existing clips).
   bool CanInsertClip(WaveClip* clip, double &slideBy, double &tolerance) const;

   // Remove the clip from the track and return a SMART pointer to it.
   // You assume responsibility for its memory!
   std::shared_ptr<WaveClip> RemoveAndReturnClip(WaveClip* clip);

   //! Append a clip to the track; which must have the same block factory as this track; return success
   bool AddClip(const std::shared_ptr<WaveClip> &clip);

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

   int GetLastScaleType() const { return mLastScaleType; }
   void SetLastScaleType() const;

   int GetLastdBRange() const { return mLastdBRange; }
   void SetLastdBRange() const;

   void GetDisplayBounds(float *min, float *max) const;
   void SetDisplayBounds(float min, float max) const;
   void GetSpectrumBounds(float *min, float *max) const;
   void SetSpectrumBounds(float min, float max) const;

   // For display purposes, calculate the y coordinate where the midline of
   // the wave should be drawn, if display minimum and maximum map to the
   // bottom and top.  Maybe that is out of bounds.
   int ZeroLevelYCoordinate(wxRect rect) const;

   class IntervalData final : public Track::IntervalData {
   public:
      explicit IntervalData( const std::shared_ptr<WaveClip> &pClip )
      : pClip{ pClip }
      {}
      std::shared_ptr<const WaveClip> GetClip() const { return pClip; }
      std::shared_ptr<WaveClip> &GetClip() { return pClip; }
   private:
      std::shared_ptr<WaveClip> pClip;
   };

   Track::Holder PasteInto( AudacityProject & ) const override;

   ConstIntervals GetIntervals() const override;
   Intervals GetIntervals() override;

   //! Returns nullptr if clip with such name was not found
   const WaveClip* FindClipByName(const wxString& name) const;

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

   mutable int   mLastScaleType; // last scale type choice
   mutable int           mLastdBRange;
   mutable std::vector <Location> mDisplayLocationsCache;

   //
   // Protected methods
   //

private:

   void PasteWaveTrack(double t0, const WaveTrack* other);

   TrackKind GetKind() const override { return TrackKind::Wave; }

   //
   // Private variables
   //

   SampleBlockFactoryPtr mpFactory;

   wxCriticalSection mFlushCriticalSection;
   wxCriticalSection mAppendCriticalSection;
   double mLegacyProjectFileOffset;

   std::unique_ptr<SpectrogramSettings> mpSpectrumSettings;
   std::unique_ptr<WaveformSettings> mpWaveformSettings;
};

//! A short-lived object, during whose lifetime, the contents of the WaveTrack are assumed not to change.
/*! It can replace repeated calls to WaveTrack::Get() (each of which opens and closes at least one block).
 */
class AUDACITY_DLL_API WaveTrackCache {
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

   //! Retrieve samples as floats from the track or from the memory cache
   /*! Uses fillZero always
    @return null on failure; this object owns the memory; may be invalidated if GetFloats() is called again
   */
   const float *GetFloats(sampleCount start, size_t len, bool mayThrow);

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

#include <unordered_set>
class SampleBlock;
using SampleBlockID = long long;
using SampleBlockIDSet = std::unordered_set<SampleBlockID>;
class TrackList;
using BlockVisitor = std::function< void(SampleBlock&) >;
using BlockInspector = std::function< void(const SampleBlock&) >;

// Function to visit all sample blocks from a list of tracks.
// If a set is supplied, then only visit once each unique block ID not already
// in that set, and accumulate those into the set as a side-effect.
// The visitor function may be null.
void VisitBlocks(TrackList &tracks, BlockVisitor visitor,
   SampleBlockIDSet *pIDs = nullptr);

// Non-mutating version of the above
void InspectBlocks(const TrackList &tracks, BlockInspector inspector,
   SampleBlockIDSet *pIDs = nullptr);

class ProjectRate;

class AUDACITY_DLL_API WaveTrackFactory final
   : public ClientData::Base
{
 public:
   static WaveTrackFactory &Get( AudacityProject &project );
   static const WaveTrackFactory &Get( const AudacityProject &project );
   static WaveTrackFactory &Reset( AudacityProject &project );
   static void Destroy( AudacityProject &project );

   WaveTrackFactory( const ProjectRate &rate,
      const SampleBlockFactoryPtr &pFactory)
      : mRate{ rate }
      , mpFactory(pFactory)
   {
   }
   WaveTrackFactory( const WaveTrackFactory & ) PROHIBITED;
   WaveTrackFactory &operator=( const WaveTrackFactory & ) PROHIBITED;

   const SampleBlockFactoryPtr &GetSampleBlockFactory() const
   { return mpFactory; }

 private:
   const ProjectRate &mRate;
   SampleBlockFactoryPtr mpFactory;
 public:
   std::shared_ptr<WaveTrack> DuplicateWaveTrack(const WaveTrack &orig);
   std::shared_ptr<WaveTrack> NewWaveTrack(
      sampleFormat format = (sampleFormat)0,
      double rate = 0);
};

#endif // __AUDACITY_WAVETRACK__
