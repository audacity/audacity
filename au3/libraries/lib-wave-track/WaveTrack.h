/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_WAVETRACK__
#define __AUDACITY_WAVETRACK__

#include "ClipInterface.h"
#include "PlaybackDirection.h"
#include "Prefs.h"
#include "SampleCount.h"
#include "SampleFormat.h"
#include "SampleTrack.h"
#include "WideSampleSequence.h"

#include <functional>
#include <optional>
#include <vector>
#include <wx/thread.h>
#include <wx/longlong.h>

class AudacityProject;
class BlockArray;

namespace BasicUI {
class ProgressDialog;
}

class SampleBlockFactory;
using SampleBlockFactoryPtr = std::shared_ptr<SampleBlockFactory>;

class TimeWarper;

class ClipInterface;
class Sequence;
class WaveClip;
class WaveClipChannel;
using WaveChannelInterval = WaveClipChannel;
class AudioSegmentSampleView;

//! Clips are held by shared_ptr, not for sharing, but to allow weak_ptr
using WaveClipHolder = std::shared_ptr<WaveClip>;
using WaveClipConstHolder = std::shared_ptr<const WaveClip>;
using WaveClipHolders = std::vector<WaveClipHolder>;
using WaveClipConstHolder = std::shared_ptr<const WaveClip>;
using WaveClipConstHolders = std::vector<WaveClipConstHolder>;

using ClipConstHolders = std::vector<std::shared_ptr<const ClipInterface> >;

// Temporary arrays of mere pointers
using WaveClipPointers = std::vector < WaveClip* >;
using WaveClipConstPointers = std::vector < const WaveClip* >;

using ChannelSampleView = std::vector<AudioSegmentSampleView>;
using ChannelGroupSampleView = std::vector<ChannelSampleView>;

using TimeInterval = std::pair<double, double>;
using ProgressReporter = std::function<void (double)>;

//
// Tolerance for merging wave tracks (in seconds)
//
#define WAVETRACK_MERGE_POINT_TOLERANCE 0.01

class Envelope;
class WaveTrack;

struct WaveTrackMessage {
    WaveClipHolder pClip{};
    const enum Type {
        New, //!< newly created and empty
        Deserialized, //!< being read from project file
        Inserted, //!< (partly) copied from another clip, or moved from a track
    } type {};
};

class WAVE_TRACK_API WaveChannel final : public Channel, public WideSampleSequence
{
public:
    explicit WaveChannel(WaveTrack& owner);
    ~WaveChannel() override;

    inline WaveTrack& GetTrack();
    inline const WaveTrack& GetTrack() const;

    AudioGraph::ChannelType GetChannelType() const override;

    size_t NChannels() const override;

    //! Takes volume and pan into account
    float GetChannelVolume(int channel) const override;

    //! This fails if any clip overlapping the range has non-unit stretch ratio!
    bool DoGet(
        size_t iChannel, size_t nBuffers, const samplePtr buffers[], sampleFormat format, sampleCount start, size_t len, bool backwards,
        fillFormat fill = FillFormat::fillZero, bool mayThrow = true,
        // Report how many samples were copied from within clips, rather than
        // filled according to fillFormat; but these were not necessarily one
        // contiguous range.
        sampleCount* pNumWithinClips = nullptr) const override;

    double GetStartTime() const override;
    double GetEndTime() const override;
    double GetRate() const override;
    bool HasTrivialEnvelope() const override;
    void GetEnvelopeValues(
        double* buffer, size_t bufferLen, double t0, bool backwards) const override;
    sampleFormat WidestEffectiveFormat() const override;

    ChannelGroup& DoGetChannelGroup() const override;

    std::shared_ptr<WaveClipChannel> GetInterval(size_t iInterval);
    std::shared_ptr<const WaveClipChannel> GetInterval(size_t iInterval) const;

    IteratorRange<IntervalIterator<WaveClipChannel> > Intervals();
    IteratorRange<IntervalIterator<const WaveClipChannel> > Intervals() const;

    using WideSampleSequence::GetFloats;

    //! "narrow" overload fetches from the unique channel
    bool GetFloats(float* buffer, sampleCount start, size_t len,
                   fillFormat fill = FillFormat::fillZero, bool mayThrow = true,
                   sampleCount* pNumWithinClips = nullptr) const
    {
        constexpr auto backwards = false;
        return GetFloats(
            0, 1, &buffer, start, len, backwards, fill, mayThrow, pNumWithinClips);
    }

    /*!
     * @brief Request channel samples within [t0, t1), not knowing in advance how
     * many this will be.
     *
     * @details The stretching of intersecting intervals influences the number of
     * samples fitting into [t0, t1), i.e., half as many for twice as large a
     * stretch ratio, due to a larger spacing of the raw samples.
     *
     * @pre samples in [t0, t1) can be counted with `size_t`
     */
    ChannelSampleView GetSampleView(double t0, double t1, bool mayThrow) const;

    //! Random-access assignment of a range of samples
    [[nodiscard]] bool Set(constSamplePtr buffer, sampleFormat format, sampleCount start, size_t len, sampleFormat effectiveFormat = widestSampleFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
                           );

    //! Random-access assignment of a range of samples
    [[nodiscard]] bool SetFloats(const float* buffer,
                                 sampleCount start, size_t len,
                                 sampleFormat effectiveFormat = widestSampleFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
                                 )
    {
        return Set(reinterpret_cast<constSamplePtr>(buffer), floatSample,
                   start, len, effectiveFormat);
    }

    bool AppendBuffer(constSamplePtr buffer, sampleFormat format, size_t len, unsigned stride, sampleFormat effectiveFormat);

    /*!
     If there is an existing WaveClip in the WaveTrack that owns the channel,
     then the data are appended to that clip. If there are no WaveClips in the
     track, then a new one is created.
     @return true if at least one complete block was created
     */
    bool Append(constSamplePtr buffer, sampleFormat format, size_t len);

    //! A hint for sizing of well aligned fetches
    inline size_t GetBestBlockSize(sampleCount t) const;
    //! A hint for sizing of well aligned fetches
    inline size_t GetIdealBlockSize();
    //! A hint for maximum returned by either of GetBestBlockSize,
    //! GetIdealBlockSize
    inline size_t GetMaxBlockSize() const;

    inline sampleFormat GetSampleFormat() const;

private:
    WaveTrack& mOwner;
};

class WAVE_TRACK_API WaveTrack final : public WritableSampleTrack, public Observer::Publisher<WaveTrackMessage>
{
    struct CreateToken {};
public:
    static const char* WaveTrack_tag;

    using Interval = WaveClip;
    using IntervalHolder = std::shared_ptr<Interval>;
    using IntervalHolders = std::vector<IntervalHolder>;
    using IntervalConstHolder = std::shared_ptr<const Interval>;
    using IntervalConstHolders = std::vector<IntervalConstHolder>;

    /// \brief Structure to hold region of a wavetrack and a comparison function
    /// for sortability.
    struct Region
    {
        Region()
            : start(0), end(0) {}
        Region(double start_, double end_)
            : start(start_), end(end_) {}

        double start, end;

        //used for sorting
        bool operator <(const Region& b) const
        {
            return this->start < b.start;
        }
    };

    using Regions = std::vector < Region >;

    static wxString GetDefaultAudioTrackNamePreference();

    static double ProjectNyquistFrequency(const AudacityProject& project);

    //
    // Constructor / Destructor / Duplicator
    //

    // Construct and also build all attachments
    static WaveTrack* New(AudacityProject& project);

    //! Don't call directly, but use Create
    WaveTrack(CreateToken&&, const SampleBlockFactoryPtr& pFactory, sampleFormat format, double rate);

    using Holder = std::shared_ptr<WaveTrack>;

    //! Factory builds all AttachedTrackObjects
    static Holder Create(
        const SampleBlockFactoryPtr& pFactory, sampleFormat format, double rate);

    const SampleBlockFactoryPtr& GetSampleBlockFactory() const
    { return mpFactory; }

    size_t NChannels() const override;

    auto GetChannel(size_t iChannel)
    {
        return this->ChannelGroup::GetChannel<WaveChannel>(iChannel);
    }

    auto GetChannel(size_t iChannel) const
    {
        return this->ChannelGroup::GetChannel<const WaveChannel>(iChannel);
    }

    auto Channels()
    {
        return this->ChannelGroup::Channels<WaveChannel>();
    }

    auto Channels() const
    {
        return this->ChannelGroup::Channels<const WaveChannel>();
    }

    AudioGraph::ChannelType GetChannelType() const override;

    //! Overwrite data excluding the sample sequence but including display
    //! settings
    void Init(const WaveTrack& orig);
private:
    std::ptrdiff_t FindClip(const Interval& clip);

    void RemoveClip(std::ptrdiff_t distance);

    Track::Holder Clone(bool backup) const override;

    friend class WaveTrackFactory;

    wxString MakeClipCopyName(const wxString& originalName) const;
    wxString MakeNewClipName() const;

public:

    virtual ~WaveTrack();

    void MoveTo(double o) override;
    void ShiftBy(double t0, double delta) override;

    bool LinkConsistencyFix(bool doFix) override;

    //! Implement WideSampleSequence
    double GetStartTime() const override;
    //! Implement WideSampleSequence
    double GetEndTime() const override;

    //
    // Identifying the type of track
    //

    //
    // WaveTrack parameters
    //

    double GetRate() const override;
    ///!brief Sets the new rate for the track without resampling it
    /*!
     @pre newRate > 0
     */
    void SetRate(double newRate);

    // Multiplicative factor.  Only converted to dB for display.
    float GetVolume() const;
    void SetVolume(float newVolume);

    // -1.0 (left) -> 1.0 (right)
    float GetPan() const;
    void SetPan(float newPan);

    //! Takes volume and pan into account
    float GetChannelVolume(int channel) const override;

    sampleCount GetVisibleSampleCount() const;

    sampleFormat GetSampleFormat() const override;

    void ConvertToSampleFormat(sampleFormat format, const std::function<void(size_t)>& progressReport = {});

    //
    // High-level editing
    //

    Track::Holder Cut(double t0, double t1) override;

    //! Make another track copying format, rate, etc. but containing no
    //! clips; with the specified number of channels.
    /*!
     It is important to pass the correct factory (that for the project
     which will own the copy) in the unusual case that a track is copied from
     another project or the clipboard.  For copies within one project, the
     default will do.
     */
    Holder EmptyCopy(size_t nChannels, const SampleBlockFactoryPtr& pFactory = {}) const;

    //! Make another channel group copying format, rate, etc. but
    //! containing no clips; with as many channels as in `this`
    /*!
     It is important to pass the correct factory (that for the project
     which will own the copy) in the unusual case that a track is copied from
     another project or the clipboard.  For copies within one project, the
     default will do.
     */
    Holder EmptyCopy(const SampleBlockFactoryPtr& pFactory = {})
    const;

    //! Simply discard any right channel
    void MakeMono();

    /*!
     @pre `!GetOwner()`
     */
    Holder MonoToStereo();

    /*
     @return a vector of mono tracks, which are in the list that owns `this`
     @pre `GetOwner()`
     */
    std::vector<Holder> SplitChannels();

    //! @pre `NChannels() == 2`
    void SwapChannels();

    // If forClipboard is true,
    // and there is no clip at the end time of the selection, then the result
    // will contain a "placeholder" clip whose only purpose is to make
    // GetEndTime() correct.  This clip is not re-copied when pasting.
    Track::Holder Copy(double t0, double t1, bool forClipboard = true)
    const override;

    void Clear(double t0, double t1) override;
    void Paste(double t0, const Track& src) override;
    using Track::Paste; // Get the non-virtual overload too

    /*!
     May assume precondition: t0 <= t1
     If the source has one channel and this has more, then replicate source
     @pre `src.NChannels() == 1 || src.NChannels() == NChannels()`
     */
    void ClearAndPaste(
        double t0, double t1, const WaveTrack& src, bool preserve = true, bool merge = true, const TimeWarper* effectWarper = nullptr,
        bool clearByTrimming = false) /* not override */;

    void Silence(double t0, double t1, ProgressReporter reportProgress) override;
    void InsertSilence(double t, double len) override;

    void Split(double t0, double t1);

    std::pair<IntervalHolder, IntervalHolder> SplitAt(double t);

    /*!
     May assume precondition: t0 <= t1
     */
    void ClearAndAddCutLine(double t0, double t1) /* not override */;

    /*!
     @post result: `result->NChannels() == NChannels()`
     */
    Holder SplitCut(double t0, double t1) /* not override */;

    // May assume precondition: t0 <= t1
    void SplitDelete(double t0, double t1) /* not override */;
    void Join(
        double t0, double t1, const ProgressReporter& reportProgress) /* not override */;
    // May assume precondition: t0 <= t1
    void Disjoin(double t0, double t1) /* not override */;

    // May assume precondition: t0 <= t1
    void Trim(double t0, double t1) /* not override */;

    /*
     * @param interval Entire track is rendered if nullopt, else only samples
     * within [interval->first, interval->second), in which case clips are split
     * at these boundaries before rendering - if rendering is needed.
     *
     * @pre `!interval.has_value() || interval->first <= interval->second`
     */
    void ApplyPitchAndSpeed(
        std::optional<TimeInterval> interval, ProgressReporter reportProgress);

    void SyncLockAdjust(double oldT1, double newT1) override;

    /** @brief Returns true if there are no WaveClips in the specified region
     *
     * @return true if no clips in the track overlap the specified time range,
     * false otherwise.
     */
    bool IsEmpty(double t0, double t1) const;

    /*!
     If there is an existing WaveClip in the WaveTrack,
     then the data are appended to that clip. If there are no WaveClips in the
     track, then a new one is created.
     @pre `iChannel < NChannels()`
     @return true if at least one complete block was created
     */
    bool Append(size_t iChannel, constSamplePtr buffer, sampleFormat format, size_t len, unsigned int stride = 1,
                sampleFormat effectiveFormat = widestSampleFormat)
    override;

    void Flush() override;

    void RepairChannels() override;

    //! @name PlayableSequence implementation
    //! @{
    const ChannelGroup* FindChannelGroup() const override;
    bool GetMute() const override;
    bool GetSolo() const override;
    //! @}

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

    //! This fails if any clip overlapping the range has non-unit stretch ratio!
    bool DoGet(
        size_t iChannel, size_t nBuffers, const samplePtr buffers[], sampleFormat format, sampleCount start, size_t len, bool backwards,
        fillFormat fill = FillFormat::fillZero, bool mayThrow = true,
        // Report how many samples were copied from within clips, rather than
        // filled according to fillFormat; but these were not necessarily one
        // contiguous range.
        sampleCount* pNumWithinClips = nullptr) const override;

    /*!
     * @brief Request samples within [t0, t1), not knowing in advance how
     * many this will be.
     *
     * @details The stretching of intersecting intervals influences the number of
     * samples fitting into [t0, t1), i.e., half as many for twice as large a
     * stretch ratio, due to a larger spacing of the raw samples.
     *
     * @post result: `result.size() == NChannels()`
     * @pre samples in [t0, t1) can be counted with `size_t`
     */
    ChannelGroupSampleView
    GetSampleView(double t0, double t1, bool mayThrow = true) const;

    sampleFormat WidestEffectiveFormat() const override;

    bool HasTrivialEnvelope() const override;

    void GetEnvelopeValues(
        double* buffer, size_t bufferLen, double t0, bool backwards) const override;

    //
    // Getting information about the track's internal block sizes
    // and alignment for efficiency
    //

    // These return a nonnegative number of samples meant to size a memory buffer
    size_t GetBestBlockSize(sampleCount t) const;
    size_t GetMaxBlockSize() const;
    size_t GetIdealBlockSize();

    //
    // XMLTagHandler callback methods for loading and saving
    //

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    void HandleXMLEndTag(const std::string_view& tag) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;
    void WriteXML(XMLWriter& xmlFile) const override;

    // Returns true if an error occurred while reading from XML
    std::optional<TranslatableString> GetErrorOpening() const override;

    //
    // Lock and unlock the track: you must lock the track before
    // doing a copy and paste between projects.
    //

    IntervalHolder GetLeftmostClip();
    IntervalConstHolder GetLeftmostClip() const;

    IntervalHolder GetRightmostClip();
    IntervalConstHolder GetRightmostClip() const;

    /**
     * @brief Get access to the (visible) clips in the tracks, in unspecified
     * order.
     */
    ClipConstHolders GetClipInterfaces() const;

    //! Create new clip that uses this track's factory but do not add it to the
    //! track
    /*!
     Returns a pointer to the newly created clip. Optionally initial offset and
     clip name may be provided, and a clip from which to copy all sample data.
     The clip is not owned by the track.  Use InsertInterval to make it so.
     @param offset desired sequence (not play) start time
     */
    IntervalHolder
    CreateClip(double offset = .0, const wxString& name = wxEmptyString, const Interval* pToCopy = nullptr, bool copyCutlines = true);

    //! Create new clip and add it to this track.
    /*!
     Returns a pointer to the newly created clip, using this track's block
     factory but copying all else from the given clip, except possibly the
     cutlines.
     */
    IntervalHolder CopyClip(const Interval& toCopy, bool copyCutlines);

    /*!
    @pre t0 <= t1
    */
    IntervalConstHolders GetSortedClipsIntersecting(double t0, double t1) const;

private:
    void CopyWholeClip(
        const Interval& clip, double t0, bool forClipboard, bool backup);
    void CopyPartOfClip(
        const Interval& clip, double t0, double t1, bool forClipboard, bool backup);
    void FinishCopy(double t0, double t1, bool forClipboard);

    //! Return all WaveClips sorted by clip play start time.
    IntervalConstHolders SortedClipArray() const;
    IntervalConstHolder GetClipAtTime(double time) const;

    void CreateRight();

    //! Create a new clip that can be inserted later into the track
    /*!
     Returns a pointer to the newly created clip. Optionally initial offset and
     clip name may be provided

     @post result: `result->NChannels() == track.NChannels()`
     */
    WaveClipHolder DoCreateClip(
        double offset = .0, const wxString& name = wxEmptyString) const;

public:
    /** @brief Get access to the most recently added clip, or create a clip,
    *  if there is not already one.  THIS IS NOT NECESSARILY RIGHTMOST.
    *
    *  @return a pointer to the most recently added WaveClip
    */
    IntervalHolder NewestOrNewClip();

    /** @brief Get access to the last (rightmost) clip, or create a clip,
    *  if there is not already one.
    *
    *  @return a pointer to a WaveClip at the end of the track
    */
    IntervalHolder RightmostOrNewClip();

    // Get number of clips in this WaveTrack
    int GetNumClips() const;

public:
    //! Return all WaveClips sorted by clip play start time.
    IntervalHolders SortedIntervalArray();
    //! Return all WaveClips sorted by clip play start time.
    IntervalConstHolders SortedIntervalArray() const;

    //! Decide whether the clips could be offset (and inserted) together without overlapping other clips
    /*!
    @return true if possible to offset by `(allowedAmount ? *allowedAmount : amount)`
     */
    bool CanOffsetClips(
        const std::vector<Interval*>& movingClips, //!< not necessarily in this track
        double amount, //!< signed
        double* allowedAmount = nullptr /*!<
         [out] if null, test exact amount only; else, largest (in magnitude) possible offset with same sign */
        );

    // Before moving a clip into a track (or inserting a clip), use this
    // function to see if the times are valid (i.e. don't overlap with
    // existing clips).
    bool
    CanInsertClip(const Interval& clip, double& slideBy, double tolerance) const;

    // Merge two clips, that is append data from clip2 to clip1,
    // then remove clip2 from track.
    // clipidx1 and clipidx2 are indices into the clip list.
    bool MergeClips(int clipidx1, int clipidx2);

    // Resample track (i.e. all clips in the track)
    void Resample(int rate, BasicUI::ProgressDialog* progress = NULL);

    //! Random-access assignment of a range of samples
    /*!
     @param buffers a span of pointers of size `NChannels()`
     @pre each of buffers is non-null
     */
    [[nodiscard]] bool SetFloats(const float* const* buffers, sampleCount start, size_t len, sampleFormat effectiveFormat = widestSampleFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
                                 );

    const TypeInfo& GetTypeInfo() const override;
    static const TypeInfo& ClassTypeInfo();

    ///@return Interval that starts after(before) the beginning of the passed interval
    IntervalConstHolder GetNextInterval(
        const Interval& interval, PlaybackDirection searchDirection) const;

    /*!
     * @copydoc GetNextInterval(const Interval&, PlaybackDirection) const
     */
    IntervalHolder
    GetNextInterval(const Interval& interval, PlaybackDirection searchDirection);

    IntervalHolder GetIntervalAtTime(double t);

    auto Intervals() { return ChannelGroup::Intervals<Interval>(); }
    auto Intervals() const { return ChannelGroup::Intervals<const Interval>(); }

    /*
     @param newClip false if clip has contents from another clip or track
     @pre interval is not already owned by this or any other track
     */
    void InsertInterval(const IntervalHolder& interval, bool newClip, bool allowEmpty = false);

    void RemoveInterval(const IntervalHolder& interval);

    Track::Holder PasteInto(AudacityProject& project, TrackList& list)
    const override;

    bool HasClipNamed(const wxString& name) const;

    size_t NIntervals() const override;

    IntervalHolder GetClip(size_t iInterval);
    IntervalConstHolder GetClip(size_t iInterval) const;

    //!< used only during deserialization
    void SetLegacyFormat(sampleFormat format);

private:
    /*!
      Sets project tempo on clip upon push. Use this instead of
      `NarrowClips().push_back`
      @returns true on success
      @param backup whether the duplication is for backup purposes while opening
      a project, instead of other editing operations
      */
    bool InsertClip(WaveClipHolders& clips, WaveClipHolder clip, bool newClip, bool backup, bool allowEmpty);

    void CopyClips(WaveClipHolders& clips, SampleBlockFactoryPtr pFactory, const WaveClipHolders& orig, bool backup);

    //! Steal channel attachments from other, then destroy the track attachment
    //! slot
    void MergeChannelAttachments(WaveTrack&& other);

    //! Erase all attachments for a given index
    void EraseChannelAttachments(size_t index);

public:
    //! Get the linear index of a given clip (== number of clips if not found)
    int GetClipIndex(const Interval& clip) const;

private:
    // May assume precondition: t0 <= t1
    void HandleClear(
        double t0, double t1, bool addCutLines, bool split, bool clearByTrimming = false);

    /*
     * @brief Copy/Paste operations must preserve beat durations, but time
     * boundaries are expressed in seconds. For pasting to work, source and
     * destination tracks must therefore have equal tempo.
     * @pre Preconditions of `ClearAndPaste`
     * @pre `GetProjectTempo().has_value() && GetProjectTempo() ==
     * src.GetProjectTempo()`
     */
    void ClearAndPasteAtSameTempo(
        double t0, double t1, const WaveTrack& src, bool preserve, bool merge, const TimeWarper* effectWarper, bool clearByTrimming);

    //! @pre All clips intersecting [t0, t1) have unit stretch ratio
    static void JoinOne(WaveTrack& track, double t0, double t1);
    static void WriteOneXML(const WaveChannel& channel, XMLWriter& xmlFile, size_t iChannel, size_t nChannels);
    void ExpandOneCutLine(double cutLinePosition, double* cutlineStart, double* cutlineEnd);
    void ApplyPitchAndSpeedOnIntervals(
        const std::vector<IntervalHolder>& intervals, const ProgressReporter& reportProgress);
    /*!
     @pre `oldOne->NChannels() == newOne->NChannels()`
     @pre newOne and oldOne are the same, or else newOne is not already owned by
     this or any other track
     */
    void
    ReplaceInterval(const IntervalHolder& oldOne, const IntervalHolder& newOne);

    std::shared_ptr<WideChannelGroupInterval> DoGetInterval(size_t iInterval)
    override;
    std::shared_ptr<::Channel> DoGetChannel(size_t iChannel) override;

    WaveClipHolders& NarrowClips();
    const WaveClipHolders& NarrowClips() const;

    //
    // Protected variables
    //

    //! @invariant non-null
    WaveChannel mChannel;
    //! may be null
    std::optional<WaveChannel> mRightChannel;

    /*!
     * Do not call `mClips.push_back` directly. Use `InsertClip` instead.
     * @invariant all are non-null
     */
    WaveClipHolders mClips;

    mutable int mLegacyRate{ 0 }; //!< used only during deserialization
    sampleFormat mLegacyFormat{ undefinedSample }; //!< used only during deserialization

private:
    //Updates rate parameter only in WaveTrackData
    void DoSetRate(double newRate);
    [[nodiscard]] Holder DuplicateWithOtherTempo(double newTempo) const;

    bool GetOne(const WaveClipHolders& clips, size_t iChannel, samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
                bool backwards, fillFormat fill, bool mayThrow, sampleCount* pNumWithinClips) const;

    void DoSetPan(float value);
    void DoSetVolume(float value);

    /*
     @pre `other.NChannels() == 1 || other.NChannels() == NChannels()`
     */
    void PasteWaveTrack(double t0, const WaveTrack& other, bool merge);
    /*
     * @copybrief ClearAndPasteAtSameTempo
     * @pre Preconditions of `PasteWaveTrack`
     * @pre `GetProjectTempo().has_value() && GetProjectTempo() ==
     * other.GetProjectTempo()`
     */
    void
    PasteWaveTrackAtSameTempo(double t0, const WaveTrack& other, bool merge);

    //! Whether all clips of an unzipped leader track have a common rate
    bool RateConsistencyCheck() const;
    //! Whether all tracks in unzipped group and all clips have a common sample
    //! format
    bool FormatConsistencyCheck() const;

    void ApplyPitchAndSpeedOne(
        double t0, double t1, const ProgressReporter& reportProgress);

public:
    //! Convert channel-major storage to interval-major,
    //! replacing two tracks with one in the owning TrackList
    /*!
     @pre `GetOwner()`
     @pre next track in the list exists, is a WaveTrack, has one channel only
     @pre `NChannels() == 1`
     @pre if mustAlign, then clips are aligned across the tracks
     @param mustAlign if false, clips may be of different number or not aligned.
     Do not use the resulting track normally!
     */
    void ZipClips(bool mustAlign = true);

private:
    SampleBlockFactoryPtr mpFactory;

    wxCriticalSection mFlushCriticalSection;
    wxCriticalSection mAppendCriticalSection;
    double mLegacyProjectFileOffset{ 0 };

    friend WaveChannel; // so it can Publish
};

ENUMERATE_TRACK_TYPE(WaveTrack);

WaveTrack& WaveChannel::GetTrack()
{
    auto& result = static_cast<WaveTrack&>(DoGetChannelGroup());
    return result;
}

const WaveTrack& WaveChannel::GetTrack() const
{
    auto& result = static_cast<const WaveTrack&>(DoGetChannelGroup());
    return result;
}

size_t WaveChannel::GetBestBlockSize(sampleCount t) const
{
    return GetTrack().GetBestBlockSize(t);
}

size_t WaveChannel::GetIdealBlockSize()
{
    return GetTrack().GetIdealBlockSize();
}

size_t WaveChannel::GetMaxBlockSize() const
{
    return GetTrack().GetMaxBlockSize();
}

sampleFormat WaveChannel::GetSampleFormat() const
{
    return GetTrack().GetSampleFormat();
}

class ProjectRate;

class WAVE_TRACK_API WaveTrackFactory final : public ClientData::Base
{
public:
    static WaveTrackFactory& Get(AudacityProject& project);
    static const WaveTrackFactory& Get(const AudacityProject& project);
    static WaveTrackFactory& Reset(AudacityProject& project);
    static void Destroy(AudacityProject& project);

    WaveTrackFactory(
        const ProjectRate& rate,
        const SampleBlockFactoryPtr& pFactory)
        : mRate{rate}
        , mpFactory(pFactory)
    {
    }

    WaveTrackFactory(const WaveTrackFactory&) = delete;
    WaveTrackFactory& operator=(const WaveTrackFactory&) = delete;

    const SampleBlockFactoryPtr& GetSampleBlockFactory() const
    { return mpFactory; }

    /**
     * \brief Creates an unnamed empty WaveTrack with default sample format and default rate
     * \return Orphaned WaveTrack
     */
    std::shared_ptr<WaveTrack> Create();

    /**
     * \brief Creates an unnamed empty WaveTrack with custom sample format and custom rate
     * \param format Desired sample format
     * \param rate Desired sample rate
     * \return Orphaned WaveTrack
     */
    std::shared_ptr<WaveTrack> Create(sampleFormat format, double rate);

    /**
     * \brief Creates a new track with project's default rate and format and the
     * given number of channels.
     * @pre `nChannels > 0`
     * @pre `nChannels <= 2`
     */
    WaveTrack::Holder Create(size_t nChannels);

    /**
     * \brief Creates tracks with project's default rate and format and the
     * given number of channels.
     */
    TrackListHolder CreateMany(size_t nChannels);

    /**
     * \brief Creates a new \p track with specified \p format and
     * \p rate and number of channels
     * @pre `nChannels > 0`
     * @pre `nChannels <= 2`
     */
    WaveTrack::Holder Create(size_t nChannels, sampleFormat format, double rate);

    /**
     * \brief Creates tracks with specified \p format and
     * \p rate and number of channels
     */
    TrackListHolder CreateMany(size_t nChannels, sampleFormat format, double rate);

    /**
     * \brief Creates an empty copy of \p proto with the specified number
     * of channels.
     */
    WaveTrack::Holder Create(size_t nChannels, const WaveTrack& proto);

private:
    std::shared_ptr<WaveTrack> DoCreate(
        size_t nChannels, sampleFormat format, double rate);

    const ProjectRate& mRate;
    SampleBlockFactoryPtr mpFactory;
};

extern WAVE_TRACK_API BoolSetting
    EditClipsCanMove
;

extern WAVE_TRACK_API StringSetting AudioTrackNameSetting;

WAVE_TRACK_API bool GetEditClipsCanMove();

// Generate a registry for serialized data
#include "XMLMethodRegistry.h"
using WaveTrackIORegistry = XMLMethodRegistry<WaveTrack>;
DECLARE_XML_METHOD_REGISTRY(WAVE_TRACK_API, WaveTrackIORegistry);

#endif // __AUDACITY_WAVETRACK__
