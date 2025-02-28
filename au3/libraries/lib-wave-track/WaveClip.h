/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveClip.h

  ?? Dominic Mazzoni
  ?? Markus Meyer

*******************************************************************/
#ifndef __AUDACITY_WAVECLIP__
#define __AUDACITY_WAVECLIP__

#include "Channel.h"
#include "ClientData.h"
#include "CRTPBase.h"
#include "SampleFormat.h"
#include "ClipInterface.h"
#include "XMLTagHandler.h"
#include "SampleCount.h"
#include "AudioSegmentSampleView.h"

#include <wx/longlong.h>

#include <cassert>
#include <functional>
#include <optional>
#include <vector>

class BlockArray;
class Envelope;
class sampleCount;
class SampleBlock;
class SampleBlockFactory;
using SampleBlockFactoryPtr = std::shared_ptr<SampleBlockFactory>;
class Sequence;
class wxFileNameWrapper;
namespace BasicUI {
class ProgressDialog;
}

class WaveClip;

// Array of pointers that assume ownership
using WaveClipHolder = std::shared_ptr<WaveClip>;
using WaveClipConstHolder = std::shared_ptr<const WaveClip>;
using WaveClipHolders = std::vector <WaveClipHolder>;
using WaveClipConstHolders = std::vector<WaveClipConstHolder>;
using ProgressReporter = std::function<void (double)>;

struct WAVE_TRACK_API WaveClipListener;
CRTP_BASE(WaveClipListenerBase, struct,
          ClientData::Cloneable<WaveClipListener>);
struct WaveClipListener : WaveClipListenerBase {
    virtual ~WaveClipListener() = 0;
    virtual void MarkChanged() noexcept = 0;
    virtual void Invalidate() = 0;

    // Default implementation does nothing
    virtual void WriteXMLAttributes(XMLWriter& writer) const;

    // Default implementation just returns false
    virtual bool HandleXMLAttribute(
        const std::string_view& attr, const XMLAttributeValueView& valueView);

    //! Append the other's attachments to this, assuming concrete subclasses are
    //! the same
    /*!
     Default implementation does nothing
     @param aligned whether the strong invariant condition on the clip may be
     assumed
     @pre `typeid(*this) == typeid(other)`
     */
    virtual void MakeStereo(WaveClipListener&& other, bool aligned);

    //! Default implementation does nothing
    virtual void SwapChannels();

    //! Erase attachment at a given index, if it existed, moving later-indexed
    //! attachments to earlier indices
    /*!
     Default implementation does nothing
     */
    virtual void Erase(size_t index);
};

class WAVE_TRACK_API WaveClipChannel : public ChannelInterval, public ClipTimes
{
public:
    WaveClipChannel(WaveClip& clip, size_t iChannel)
        : mClip{clip}
        , miChannel{iChannel}
    {}
    ~WaveClipChannel() override;

    WaveClip& GetClip() { return mClip; }
    const WaveClip& GetClip() const { return mClip; }

    size_t GetChannelIndex() const { return miChannel; }

    Envelope& GetEnvelope();
    const Envelope& GetEnvelope() const;

    bool Intersects(double t0, double t1) const;
    double Start() const;
    double End() const;

    /*!
     * @brief Request interval samples within [t0, t1). `t0` and `t1` are
     * truncated to the interval start and end. Stretching influences the number
     * of samples fitting into [t0, t1), i.e., half as many for twice as large a
     * stretch ratio, due to a larger spacing of the raw samples. The actual
     * number of samples available from the returned view is queried through
     * `AudioSegmentSampleView::GetSampleCount()`.
     *
     * @pre samples in [t0, t1) can be counted with `size_t`
     */
    AudioSegmentSampleView
    GetSampleView(double t0, double t1, bool mayThrow) const;

    /*!
     * @brief  t ∈ [...)
     */
    bool WithinPlayRegion(double t) const;
    double SamplesToTime(sampleCount s) const noexcept;
    bool HasPitchOrSpeed() const;

    double GetTrimLeft() const;
    double GetTrimRight() const;

    bool GetSamples(samplePtr buffer, sampleFormat format, sampleCount start, size_t len, bool mayThrow = true) const;

    AudioSegmentSampleView GetSampleView(
        sampleCount start, size_t length, bool mayThrow) const;

    const Sequence& GetSequence() const;

    constSamplePtr GetAppendBuffer() const;
    size_t GetAppendBufferLen() const;

    const BlockArray* GetSequenceBlockArray() const;

    /*!
     Getting high-level data for one channel for screen display and clipping
     calculations and Contrast
     */
    std::pair<float, float> GetMinMax(double t0, double t1, bool mayThrow) const;

    /*!
     @copydoc GetMinMax
     */
    float GetRMS(double t0, double t1, bool mayThrow) const;

    //! Real start time of the clip, quantized to raw sample rate (track's rate)
    sampleCount GetPlayStartSample() const;

    //! Real end time of the clip, quantized to raw sample rate (track's rate)
    sampleCount GetPlayEndSample() const;

    /*!
     @param start relative to clip play start sample
     */
    void SetSamples(constSamplePtr buffer, sampleFormat format, sampleCount start, size_t len, sampleFormat effectiveFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
                    );

    void WriteXML(XMLWriter& xmlFile) const;

    // implement ClipTimes
    sampleCount GetVisibleSampleCount() const override;
    int GetRate() const override;
    double GetPlayStartTime() const override;
    double GetPlayEndTime() const override;
    double GetPlayDuration() const;
    sampleCount TimeToSamples(double time) const override;
    double GetStretchRatio() const override;

    friend inline bool operator ==(
        const WaveClipChannel& a, const WaveClipChannel& b)
    { return &a.mClip == &b.mClip && a.miChannel == b.miChannel; }
    friend inline bool operator !=(
        const WaveClipChannel& a, const WaveClipChannel& b)
    { return !(a == b); }

private:
    WaveClip& mClip;
    const size_t miChannel;
};

struct CentShiftChange
{
    explicit CentShiftChange(int newValue)
        : newValue(newValue)
    {
    }

    const int newValue;
};

struct PitchAndSpeedPresetChange
{
    explicit PitchAndSpeedPresetChange(PitchAndSpeedPreset newValue)
        : newValue(newValue)
    {
    }

    const PitchAndSpeedPreset newValue;
};

struct StretchRatioChange
{
    explicit StretchRatioChange(double newValue)
        : newValue(newValue)
    {
    }

    const double newValue;
};

struct WaveClipDtorCalled
{
};

class WAVE_TRACK_API WaveClip final : public ClipInterface, public WideChannelGroupInterval, public XMLTagHandler, public ClientData::Site<
        WaveClip, WaveClipListener, ClientData::DeepCopying>, public Observer::Publisher<CentShiftChange>,
    public Observer::Publisher<PitchAndSpeedPresetChange>, public Observer::Publisher<StretchRatioChange>,
    public Observer::Publisher<WaveClipDtorCalled>
{
private:
    struct CreateToken {
        bool emptyCopy = false;
    };

    // It is an error to copy a WaveClip without specifying the
    // sample block factory.

    WaveClip(const WaveClip&) = delete;
    WaveClip& operator=(const WaveClip&) = delete;

    //! typical constructor
    /*!
     @param width how many sequences
     @pre `width > 0`
     @post `NChannels() == width`
     */
    WaveClip(size_t width, const SampleBlockFactoryPtr& factory, sampleFormat format, int rate);

    //! essentially a copy constructor - but you must pass in the
    //! current sample block factory, because we might be copying
    //! from one project to another
    /*!
     @post `NChannels() == orig.NChannels()`
     @post `!copyCutlines || NumCutLines() == orig.NumCutLines()`
     */
    WaveClip(const WaveClip& orig,
             const SampleBlockFactoryPtr& factory,
             bool copyCutlines)
        : WaveClip{orig, factory, copyCutlines, {}}
    {}

    //! @brief Copy only a range from the given WaveClip
    /*!
     @pre CountSamples(t1, t0) > 0
     @post `NChannels() == orig.NChannels()`
     */
    WaveClip(const WaveClip& orig, const SampleBlockFactoryPtr& factory, bool copyCutlines, double t0, double t1);

    static int64_t NewID();

public:
    static const char* WaveClip_tag;

    using Attachments = Site<WaveClip, WaveClipListener, ClientData::DeepCopying>;

    //! Cteate a new clip
    /*!
     @param width how many sequences
     @pre `width > 0`
     @post `NChannels() == width`
     */
    static WaveClip* New(size_t width, const SampleBlockFactoryPtr& factory, sampleFormat format, int rate)
    {
        WaveClip* clip = new WaveClip(width, factory, format, rate);
        clip->mId = NewID();
        return clip;
    }

    static std::shared_ptr<WaveClip> NewShared(size_t width, const SampleBlockFactoryPtr& factory,
                                               sampleFormat format, int rate)
    {
        return std::shared_ptr<WaveClip>(New(width, factory, format, rate));
    }

    //! Create a new clip as copy origin
    /*!
     @post `NChannels() == orig.NChannels()`
     @post `!copyCutlines || NumCutLines() == orig.NumCutLines()`
     */
    static WaveClip* NewFrom(
        const WaveClip& orig, const SampleBlockFactoryPtr& factory,
        bool copyCutlines, bool backup)
    {
        WaveClip* clip = new WaveClip(orig, factory, copyCutlines);
        if (!backup) {
            clip->mId = NewID();
        }
        return clip;
    }

    static std::shared_ptr<WaveClip> NewSharedFrom(
        const WaveClip& orig, const SampleBlockFactoryPtr& factory,
        bool copyCutlines, bool backup)
    {
        return std::shared_ptr<WaveClip>(
            NewFrom(orig, factory, copyCutlines, backup));
    }

    static WaveClip* NewFromRange(
        const WaveClip& orig, const SampleBlockFactoryPtr& factory,
        bool copyCutlines, bool backup, double t0, double t1)
    {
        WaveClip* clip = new WaveClip(orig, factory, copyCutlines, t0, t1);
        if (!backup) {
            clip->mId = NewID();
        }
        return clip;
    }

    static std::shared_ptr<WaveClip> NewSharedFromRange(
        const WaveClip& orig, const SampleBlockFactoryPtr& factory,
        bool copyCutlines, bool backup, double t0, double t1)
    {
        return std::shared_ptr<WaveClip>(
            NewFromRange(orig, factory, copyCutlines, backup, t0, t1));
    }

    virtual ~WaveClip();

    int64_t GetId() const;
    void SetId(int64_t id);

    // Satisfying WideChannelGroupInterval
    double Start() const override;
    double End() const override;
    std::shared_ptr<ChannelInterval> DoGetChannel(size_t iChannel) override;

    using Channel = WaveClipChannel;

    auto Channels()
    {
        return
            WideChannelGroupInterval::Channels<Channel>();
    }

    auto Channels() const
    {
        return
            WideChannelGroupInterval::Channels<const Channel>();
    }

    //! Check weak invariant conditions on mSequences and mCutlines
    /*! Conditions are
     `mSequences.size() > 0`
     all sequences are non-null
     all sequences have the same sample formats
        and sample block factory
     all cutlines satisfy the strong invariant
     */
    bool CheckInvariants() const;

    /*!
     A precondition for some mutating operations
     CheckInvariants() is true, and also all sequences have the same length
     */
    bool StrongInvariant() const;

    //! When `StrongInvariant()` is false, violate an assertion in debug, but
    //! in release, establish it (or fail, propagating an exception)
    /*! @excsafety{Strong} */
    void AssertOrRepairStrongInvariant();

    //! Assert or repair strong invariant before mutating the sequence;
    //! assert the strong invariant again at exit
    struct StrongInvariantScope {
        explicit StrongInvariantScope(WaveClip& clip);
        ~StrongInvariantScope();
    private:
        WaveClip& mClip;
    };

    //! How many Sequences the clip contains.
    size_t NChannels() const override;

    void ConvertToSampleFormat(sampleFormat format, const std::function<void(size_t)>& progressReport = {});

    int GetRate() const override
    {
        return mRate;
    }

    // Set rate without resampling. This will change the length of the clip
    void SetRate(int rate);
    void SetRawAudioTempo(double tempo);
    void SetClipTempo(double tempo);

    PitchAndSpeedPreset GetPitchAndSpeedPreset() const;

    //! Stretches from left to the absolute time (if in expected range)
    void StretchLeftTo(double to);
    //! Sets from the right to the absolute time (if in expected range)
    void StretchRightTo(double to);
    void StretchBy(double ratio);

    double GetStretchRatio() const override;

    //! Checks for stretch-ratio equality, accounting for rounding errors.
    //! @{
    bool HasEqualPitchAndSpeed(const WaveClip& other) const;
    bool HasPitchOrSpeed() const;
    //! @}

    //! Enabling stretch to match tempo
    bool GetStretchToMatchProjectTempo() const;
    void SetStretchToMatchProjectTempo(bool enabled);

    /*
     * @post `true` if `TimeAndPitchInterface::MinCent <= cents && cents <=
     * TimeAndPitchInterface::MaxCent`
     */
    bool SetCentShift(int cents);
    int GetCentShift() const override;
    [[nodiscard]] Observer::Subscription
    SubscribeToCentShiftChange(std::function<void(int)> cb) const override;

    void SetPitchAndSpeedPreset(PitchAndSpeedPreset preset);
    [[nodiscard]] virtual Observer::Subscription
    SubscribeToPitchAndSpeedPresetChange(
        std::function<void(PitchAndSpeedPreset)> cb) const override;

    // Resample clip. This also will set the rate, but without changing
    // the length of the clip
    void Resample(int rate, BasicUI::ProgressDialog* progress = nullptr);

    double GetSequenceStartTime() const noexcept;
    void SetSequenceStartTime(double startTime);
    double GetSequenceEndTime() const;
    //! Returns the index of the first sample of the underlying sequence
    sampleCount GetSequenceStartSample() const;
    //! Returns the total number of samples in all underlying sequences
    //! (but not counting the cutlines)
    sampleCount GetSequenceSamplesCount() const;

    //! Closed-begin of play region. Always a multiple of the track's sample
    //! period, whether the clip is stretched or not.
    double GetPlayStartTime() const noexcept override;
    void SetPlayStartTime(double time);

    //! Open-end of play region. Always a multiple of the track's sample
    //! period, whether the clip is stretched or not.
    double GetPlayEndTime() const override;

    //! Always a multiple of the track's sample period, whether the clip is
    //! stretched or not.
    double GetPlayDuration() const;

    bool IsEmpty() const;

    //! Real start time of the clip, quantized to raw sample rate (track's rate)
    sampleCount GetPlayStartSample() const;
    //! Real end time of the clip, quantized to raw sample rate (track's rate)
    sampleCount GetPlayEndSample() const;

    /*!
     * Returns a number of raw samples, not accounting for stretching.
     */
    sampleCount GetVisibleSampleCount() const override;

    //! Sets the play start offset in seconds from the beginning of the underlying sequence
    void SetTrimLeft(double trim);
    //! Returns the play start offset in seconds from the beginning of the underlying sequence
    double GetTrimLeft() const noexcept;

    //! Sets the play end offset in seconds from the ending of the underlying sequence
    void SetTrimRight(double trim);
    //! Returns the play end offset in seconds from the ending of the underlying sequence
    double GetTrimRight() const noexcept;

    //! Moves play start position by deltaTime
    void TrimLeft(double deltaTime);
    //! Moves play end position by deltaTime
    void TrimRight(double deltaTime);
    //! Same as `TrimRight`, but expressed as quarter notes
    void TrimQuarternotesFromRight(double quarters);

    //! Sets the the left trimming to the absolute time (if that is in bounds)
    void TrimLeftTo(double to);
    //! Sets the the right trimming to the absolute time (if that is in bounds)
    void TrimRightTo(double to);

    /*! @excsafety{No-fail} */
    void ShiftBy(double delta) noexcept;

    //! The play region is an open-closed interval, [...), where "[ =
    //! GetPlayStartTime()", and ") = GetPlayEndTime()."

    /*!
     * @brief [ < t and t < ), such that if the track were split at `t`, it would
     * split this clip in two of lengths > 0.
     */
    bool SplitsPlayRegion(double t) const;
    /*!
     * @brief  t ∈ [...)
     */
    bool WithinPlayRegion(double t) const;
    /*!
     * @brief  t < [
     */
    bool BeforePlayRegion(double t) const;
    /*!
     * @brief  t <= [
     */
    bool AtOrBeforePlayRegion(double t) const;
    /*!
     * @brief  ) <= t
     */
    bool AfterPlayRegion(double t) const;
    /*!
     * @brief t0 and t1 both ∈ [...)
     * @pre t0 <= t1
     */
    bool EntirelyWithinPlayRegion(double t0, double t1) const;
    /*!
     * @brief t0 xor t1 ∈ [...)
     * @pre t0 <= t1
     */
    bool PartlyWithinPlayRegion(double t0, double t1) const;
    /*!
     * @brief [t0, t1) ∩ [...) != ∅
     * @pre t0 <= t1
     */
    bool IntersectsPlayRegion(double t0, double t1) const;
    /*!
     * @brief t0 <= [ and ) <= t1, such that removing [t0, t1) from the track
     * deletes this clip.
     * @pre t0 <= t1
     */
    bool CoversEntirePlayRegion(double t0, double t1) const;

    //! Counts number of sample times within t0 and t1 region. t0 and t1 are
    //! rounded to the nearest clip sample boundary, i.e. relative to clips
    //! start time offset.
    //! @returns Number of samples within t0 and t1 if t1 > t0, 0 otherwise
    sampleCount CountSamples(double t0, double t1) const;

    /*!
     * @brief Request up to `length` samples. The actual number of samples
     * available from the returned view is queried through
     * `AudioSegmentSampleView::GetSampleCount()`.
     *
     * @param start index of first clip sample from play start
     * @pre `iChannel < NChannels()`
     */
    AudioSegmentSampleView GetSampleView(
        size_t iChannel, sampleCount start, size_t length, bool mayThrow = true) const override;

    /*!
     * @brief Request interval samples within [t0, t1). `t0` and `t1` are
     * truncated to the clip's play start and end. Stretching influences the
     * number of samples fitting into [t0, t1), i.e., half as many for twice as
     * large a stretch ratio, due to a larger spacing of the raw samples. The
     * actual number of samples available from the returned view is queried
     * through `AudioSegmentSampleView::GetSampleCount()`.
     *
     * @pre `iChannel < NChannels()`
     * @pre stretched samples in [t0, t1) can be counted in a `size_t`
     */
    AudioSegmentSampleView GetSampleView(
        size_t iChannel, double t0, double t1, bool mayThrow = true) const;

    //! Get samples from one channel
    /*!
     @param ii identifies the channel
     @param start relative to clip play start sample
     @pre `ii < NChannels()`
     */
    bool GetSamples(size_t ii, samplePtr buffer, sampleFormat format, sampleCount start, size_t len, bool mayThrow = true) const;

    //! Get (non-interleaved) samples from all channels
    /*!
     assume as many buffers available as NChannels()
     @param start relative to clip play start sample
     */
    bool GetSamples(samplePtr buffers[], sampleFormat format, sampleCount start, size_t len, bool mayThrow = true) const;

    //! @param ii identifies the channel
    /*!
     @pre `ii < NChannels()`
     @pre `StrongInvariant()`
     @post `StrongInvariant()`
     @param start relative to clip play start sample
     */
    void SetSamples(size_t ii, constSamplePtr buffer, sampleFormat format, sampleCount start, size_t len, sampleFormat effectiveFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
                    );

    //! @}

    Envelope& GetEnvelope() noexcept { return *mEnvelope; }
    const Envelope& GetEnvelope() const noexcept { return *mEnvelope; }

    //! @pre `p`
    void SetEnvelope(std::unique_ptr<Envelope> p);

    //! @param ii identifies the channel
    /*!
     @pre `ii < NChannels()`
     */
    const BlockArray* GetSequenceBlockArray(size_t ii) const;

    //! Get low-level access to a sequence. Whenever possible, don't use this,
    //! but use more high-level functions inside WaveClip (or add them if you
    //! think they are useful for general use)
    /*!
     @pre `ii < NChannels()`
     */
    Sequence* GetSequence(size_t ii)
    {
        assert(ii < NChannels());
        return mSequences[ii].get();
    }

    /*!
     @copydoc GetSequence
     */
    const Sequence* GetSequence(size_t ii) const { return mSequences[ii].get(); }

    /** Getting high-level data for one channel for screen display and clipping
     * calculations and Contrast */
    /*!
     @param ii identifies the channel
     @pre `ii < NChannels()`
     */
    std::pair<float, float> GetMinMax(size_t ii, double t0, double t1, bool mayThrow) const;
    /*!
     @copydoc GetMinMax
     */
    float GetRMS(size_t ii, double t0, double t1, bool mayThrow) const;

    /** Whenever you do an operation to the sequence that will change the number
     * of samples (that is, the length of the clip), you will want to call this
     * function to tell the envelope about it. */
    void UpdateEnvelopeTrackLen();

    /*!
     * @pre `iChannel < NChannels()`
     */
    std::shared_ptr<SampleBlock>
    AppendToChannel(size_t iChannel, constSamplePtr buffer, sampleFormat format, size_t len);

    //! For use in importing pre-version-3 projects to preserve sharing of
    //! blocks; no dithering applied
    //! @pre `NChannels() == 1`
    std::shared_ptr<SampleBlock>
    AppendLegacyNewBlock(constSamplePtr buffer, sampleFormat format, size_t len);

    //! For use in importing pre-version-3 projects to preserve sharing of
    //! blocks
    /*!
     @pre `NChannels() == 1`
     */
    void AppendLegacySharedBlock(const std::shared_ptr<SampleBlock>& pBlock);

    //! Append (non-interleaved) samples to some or all channels
    //! You must call Flush after the last Append
    /*!
     For stereo clips, typically this is invoked on left, then right channels,
     either alternating (as when recording) or in two batches (channel-major
     pattern of effect processing), which violates the strong invariant
     condition, then restores it (either repeatedly, or once).

     @return true if at least one complete block was created
     In case of failure or exceptions, the clip contents are unchanged but
     un-flushed data are lost

     @pre `iChannel < NChannels()`
     @pre `iChannel + nChannels <= NChannels()`
     */
    bool Append(size_t iChannel, size_t nChannels, constSamplePtr buffers[], sampleFormat format, size_t len, unsigned int stride, sampleFormat effectiveFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
                );

    //! Append (non-interleaved) samples to all channels
    //! You must call Flush after the last Append
    /*!
     @return true if at least one complete block was created
     assume as many buffers available as NChannels()
     In case of failure or exceptions, the clip contents are unchanged but
     un-flushed data are lost

     @pre `StrongInvariant()`
     @post `StrongInvariant()`
     */
    bool Append(constSamplePtr buffers[], sampleFormat format, size_t len, unsigned int stride, sampleFormat effectiveFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
                );

    //! Flush must be called after last Append
    /*!
     In case of exceptions, the clip contents are unchanged but
     un-flushed data are lost
     */
    void Flush();

    //! Ensure that all sequences have the same sample count
    void RepairChannels();

    /// This name is consistent with WaveTrack::Clear. It performs a "Cut"
    /// operation (but without putting the cut audio to the clipboard)
    void Clear(double t0, double t1);

    /// Removes samples starting from the left boundary of the clip till
    /// t, if it's inside the play region. Also removes trimmed (hidden)
    /// data, if present. Changes offset to make remaining samples stay
    /// at their old place. Destructive operation.
    void ClearLeft(double t);
    /// Removes samples starting from t (if it's inside the clip),
    /// till the right boundary. Also removes trimmed (hidden)
    /// data, if present. Destructive operation.
    void ClearRight(double t);

    //! Clear, and add cut line that starts at t0 and contains everything until t1
    //! if there is at least one clip sample between t0 and t1, noop otherwise.
    /*!
     @pre `StrongInvariant()`
     @post `StrongInvariant()`
     */
    void ClearAndAddCutLine(double t0, double t1);

    //! @pre `NChannels() == pClip->NChannels()`
    void AddCutLine(WaveClipHolder pClip);

    /*!
     * @return true and succeed if and only if `this->NChannels() ==
     * other.NChannels()` and either this is empty or `this->GetStretchRatio() ==
     * other.GetStretchRatio()`.

     @pre `StrongInvariant()`
     @pre `other.StrongInvariant()`
     @post `StrongInvariant()`

     This says, same widths and ratios are sufficient for success
     @post result: `this->NChannels() != other.NChannels() ||
        this->GetStretchRatio() != other.GetStretchRatio() || result`
     */
    bool Paste(double t0, const WaveClip& other);

    //! Insert silence - note that this is an efficient operation for large
    //! amounts of silence
    /*!
     @pre `StrongInvariant()`
     @post `StrongInvariant()`
     */
    void InsertSilence(double t, double len, double* pEnvelopeValue = nullptr);

    /** Insert silence at the end, and causes the envelope to ramp
        linearly to the given value */
    void AppendSilence(double len, double envelopeValue);

    /// Get access to cut lines list
    const WaveClipHolders& GetCutLines() { return mCutLines; }
    const WaveClipConstHolders& GetCutLines() const
    { return reinterpret_cast< const WaveClipConstHolders& >(mCutLines); }
    size_t NumCutLines() const { return mCutLines.size(); }

    /** Find cut line at (approximately) this position. Returns true and fills
     * in cutLineStart and cutLineEnd (if specified) if a cut line at this
     * position could be found. Return false otherwise. */
    bool FindCutLine(double cutLinePosition, double* cutLineStart = nullptr, double* cutLineEnd = nullptr) const;

    /** Expand cut line (that is, re-insert audio, then DELETE audio saved in
     * cut line). Returns true if a cut line could be found and successfully
     * expanded, false otherwise */
    void ExpandCutLine(double cutLinePosition);

    /// Remove cut line, without expanding the audio in it
    bool RemoveCutLine(double cutLinePosition);

    /// Offset cutlines right to time 't0' by time amount 'len'
    void OffsetCutLines(double t0, double len);

    //! Should be called upon project close.  Not balanced by unlocking calls.
    /*! @excsafety{No-fail} */
    void CloseLock() noexcept;

    //
    // XMLTagHandler callback methods for loading and saving
    //

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    void HandleXMLEndTag(const std::string_view& tag) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

    //! Wave clip data has always been written by channel-major iteration and
    //! is still done so for compatibility.  Therefore, the first argument.
    /*!
     @param ii identifies the channel
     @pre `ii < NChannels()`
     */
    void WriteXML(size_t ii, XMLWriter& xmlFile) const;

    // AWD, Oct 2009: for pasting whitespace at the end of selection
    bool GetIsPlaceholder() const { return mIsPlaceholder; }
    void SetIsPlaceholder(bool val) { mIsPlaceholder = val; }

    void SetName(const wxString& name);
    const wxString& GetName() const;

    int64_t GetGroupId() const;
    void SetGroupId(int64_t id);

    void SetColor(const wxString& color);
    const wxString& GetColor() const;

    // TimeToSamples and SamplesToTime take clip stretch ratio into account.
    // Use them to convert time / sample offsets.
    sampleCount TimeToSamples(double time) const override;
    double SamplesToTime(sampleCount s) const noexcept;

    //! Silences the 'length' amount of samples starting from 'offset'(relative to the play start)
    /*!
     @pre `StrongInvariant()`
     @post `StrongInvariant()`
     */
    void SetSilence(sampleCount offset, sampleCount length);

    //! Get one channel of the append buffer
    /*!
     @param ii identifies the channel
     @pre `ii < NChannels()`
     */
    constSamplePtr GetAppendBuffer(size_t ii) const;
    /*!
     @param ii identifies the channel
     @pre `ii < NChannels()`
     */
    size_t GetAppendBufferLen(size_t ii) const;

    void
    OnProjectTempoChange(const std::optional<double>& oldTempo, double newTempo);

    SampleFormats GetSampleFormats() const;

    size_t CountBlocks() const;

    //! Reduce width
    /*!
     @post `NChannels() == 1`
     */
    void DiscardRightChannel();

    //! @pre `NChannels() == 2`
    void SwapChannels();

    //! A stereo WaveClip becomes mono, keeping the left side and returning a
    //! new clip with the right side samples
    /*!
     @pre `NChannels() == 2`
     @post `NChannels() == 1`
     @post result: `result->NChannels() == 1`
     */
    std::shared_ptr<WaveClip> SplitChannels();

    //! Steal the right side data from other
    //! All cutlines are lost in `this`!  Cutlines are not copied from other.
    /*!
     Stating sufficient preconditions for the postondition.  Even stronger
     preconditions on matching offset, trims, and rates could be stated.

     @pre `NChannels() == 1`
     @pre `other.NChannels() == 1`
     @pre `GetSampleFormats() == other.GetSampleFormats()`
     @pre `GetSampleBlockFactory() == other.GetSampleBlockFactory()`
     @pre `!mustAlign || GetNumSamples() == other.GetNumSamples()`

     @post `!mustAlign || StrongInvariant()`
     */
    void MakeStereo(WaveClip&& other, bool mustAlign);

    // These return a nonnegative number of samples meant to size a memory buffer
    size_t GetBestBlockSize(sampleCount t) const;
    size_t GetMaxBlockSize() const;

    //! essentially a copy constructor - but you must pass in the
    //! current sample block factory, because we might be copying
    //! from one project to another
    /*!
     This is effectively private because CreateToken is private, but must be
     public to cooperate with make_shared.

     The clip so constructed does NOT (yet) satisfy the class invariants!

     @param emptyCopy if true, don't make sequences

     @post `NChannels() == (token.emptyCopy ? 0 : orig.NChannels())`
     @post `!copyCutlines || NumCutLines() == orig.NumCutLines()`
     */
    WaveClip(const WaveClip& orig, const SampleBlockFactoryPtr& factory, bool copyCutlines, CreateToken token);

private:
    static void TransferSequence(WaveClip& origClip, WaveClip& newClip);
    static void FixSplitCutlines(
        WaveClipHolders& myCutlines, WaveClipHolders& newCutlines);

    size_t GreatestAppendBufferLen() const;

    //! Called by mutating operations; notifies listeners
    /*! @excsafety{No-fail} */
    void MarkChanged() noexcept;

    // Always gives non-negative answer, not more than sample sequence length
    // even if t0 really falls outside that range
    sampleCount TimeToSequenceSamples(double t) const;
    bool StretchRatioEquals(double value) const;
    sampleCount GetNumSamples() const;
    const SampleBlockFactoryPtr& GetFactory() const;
    std::vector<std::unique_ptr<Sequence> > GetEmptySequenceCopies() const;
    void StretchCutLines(double ratioChange);
    double SnapToTrackSample(double time) const noexcept;

    //! Fix consistency of cutlines and envelope after deleting from Sequences
    /*!
     This is like a finally object
     */
    class ClearSequenceFinisher
    {
    public:
        ClearSequenceFinisher() noexcept = default;
        /*
         @param t0 start of deleted range
         @param t1 end of deleted range
         @param clip_t0 t0 clamped to previous play region
         @param clip_t1 t1 clamped to previous play region
         */
        ClearSequenceFinisher(WaveClip* pClip,
                              double t0, double t1, double clip_t0, double clip_t1) noexcept
            : pClip{pClip}
            , t0{t0}, t1{t1}, clip_t0{clip_t0}, clip_t1{clip_t1}
        {}
        ClearSequenceFinisher& operator =(ClearSequenceFinisher&& other)
        {
            *this = other;
            other.pClip = nullptr;
            return *this;
        }

        ~ClearSequenceFinisher() noexcept;
        void Commit() noexcept { committed = true; }

    private:
        ClearSequenceFinisher&
        operator =(const ClearSequenceFinisher& other) = default;
        WaveClip* pClip{};
        double t0{}, t1{}, clip_t0{}, clip_t1{};
        bool committed = false;
    };

    //! This name is consistent with WaveTrack::Clear. It performs a "Cut"
    //! operation (but without putting the cut audio to the clipboard)
    /*!
     @pre `StrongInvariant()`
     @post `StrongInvariant()`
     */
    [[nodiscard]] ClearSequenceFinisher ClearSequence(double t0, double t1);

    //! Restores state when an update loop over mSequences fails midway
    struct Transaction {
        explicit Transaction(WaveClip& clip);
        ~Transaction();
        void Commit() { committed = true; }

        WaveClip& clip;
        std::vector<std::unique_ptr<Sequence> > sequences;
        const double mTrimLeft,
                     mTrimRight;
        bool committed{ false };
    };

    int64_t mId = -1;

    //! Real-time durations, i.e., stretching the clip modifies these.
    //! @{
    double mSequenceOffset { 0 };
    double mTrimLeft { 0 };
    double mTrimRight { 0 };
    //! @}

    PitchAndSpeedPreset mPitchAndSpeedPreset { PitchAndSpeedPreset::Default };
    int mCentShift { 0 };

    // Used in GetStretchRatio which computes the factor, by which the sample
    // interval is multiplied, to get a realtime duration.
    double mClipStretchRatio = 1.;
    std::optional<double> mRawAudioTempo;
    std::optional<double> mClipTempo;
    bool mStretchToMatchProjectTempo = true;

    //! Sample rate of the raw audio, i.e., before stretching.
    int mRate;

    /*!
     @invariant `CheckInvariants()`
     */
    std::vector<std::unique_ptr<Sequence> > mSequences;
    //! Envelope is unique, not per-sequence, and always non-null
    std::unique_ptr<Envelope> mEnvelope;

    //! Cut Lines are nothing more than ordinary wave clips, with the
    //! offset relative to the start of the clip.
    /*!
     @invariant all are non-null
     */
    WaveClipHolders mCutLines {};

    // AWD, Oct. 2009: for whitespace-at-end-of-selection pasting
    bool mIsPlaceholder { false };

    wxString mName;

    int64_t mGroupId = -1;

    wxString mColor;
};

#endif
