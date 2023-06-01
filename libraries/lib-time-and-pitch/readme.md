# `WaveClip` API

We'll want clip-level time stretching to be done on the fly. This means that the time-to-sample (index) ratio is now clip-dependent.
At this time, we can expect clip indices to be calculated using `n = (t - t_clipBegin) * SR`. Clip indices may be thought of as _relative samples_, meanwhile track 

Since we want time stretching to be non-destructive, we store the clip samples in their original timing. With time stretching, though, the constant relationship _R_ `sampleIndex = t * sampleRate` doesn't holds anymore. Now an ambiguity arises when dealing with sample indices : are those of stretched samples (where _R_ holds) and those of original samples. For example :
Method | Description | Should be stretched ? |
-------|-------------------|-----|
`WaveClip::GetSequenceStartSample()` | | No :  |
`WaveClip::GetSequenceEndSample()` | Unused | Doesn't matter since unused |
`WaveClip::GetSequenceSamplesCount()` | Used in `EstimateCopyBytesCount` | No |
`WaveClip::TimeToSequenceSamples(t)` | Used in `WaveTrack` to git track RMS by weighting clip RMSs | Yes - although individual clip RMS don't change whether stretched or not, their contribution in this case must be weighted by their perceived length.|
`long WaveTrack::GetBlockStart(long smp)` && `::GetBestBlockSize(long smp)` | Used by `SampleTrackCache` to know hom many samples are available from `smp` till the end of the memory block. | Probably, if `smp` is an _R_ conversion of _t_. |
`long WaveClip::ToSequenceSamples(long smp)` | Used in `WaveTrack::GetBlockStart` | Consistently with `WaveTrack::GetBlockStart()`, yes |
`long WaveClip::GetPlayStartSample()` | Get 1st sample of untrimmed region | Totally unsure ; depends on use, really. |
`Value-Two` | Long explanation||
`etc` | Long explanation||

-  `sampleCount ToSequenceSamples(sampleCount s) const;`
   Would it be possible to have the `WaveClip` interface only deal with time in seconds (`double`) ? This would reduce the ambiguity, when it comes to sample indices, of whether we are talking of time-stretched samples or not.

With the table below we list the public methods that use samples, describe what they are about, and see if all of them could be either gotten rid of or reworked with time unit seconds.

| Method                                                     | Description                                                                  | Uses                                                                                                                             | Alternative                                                                                               |
| ---------------------------------------------------------- | ---------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------- |
| `smp TimeToSequenceSamples(double)`                        |                                                                              | By WaveTrack to calculate cross-clip RMS                                                                                         | GetStretchedSquaredSum() and GetStretchedLength() make it even easier for the calculation                 |
| `smp ToSequenceSamples(smp)`                               |                                                                              | By WaveTrack to get num samples into data                                                                                        | GetStretchedSquaredSum() and GetStretchedLength() make it even easier for the calculation                 |
| `smp GetBlockStart(cursorPosSmp)`                          | gets block begin sample if cursor is within playable region                  |                                                                                                                                  |                                                                                                           |
| `smp CountSamples(double t0, double t1)`                   | gets num samples within intersection of [t0, t1] and trimmed clip boundaries | By WaveTrack and internally to see if intersection is empty or not => could be replaced with `bool IntersectsPlayRegion(t0, t1)` |                                                                                                           |
| `bool GetSamples(..., sampleCount start, size_t len, ...)` |                                                                              | By `WaveTrack::Disjoin` and `::Get`                                                                                              | `size_t GetSamples(..., double start, double length, ...)` ? Could avoid need for `GetBlockStart` & Co. ? |
| `GetSequenceEndSample()`                                   |                                                                              | Unused                                                                                                                           |                                                                                                           |

# Sequence Diagram

```mermaid
sequenceDiagram
    actor User
    participant MainThread
    participant AudioThread
    participant SampleHandle
    participant ProjectAudioManager
    participant AudioIO
    participant PlaybackPolicy
    participant MixerSource
    participant SampleTrackCache
    participant WaveTrack
    participant WaveClip

    User -->> MainThread:Reposition Cursor
    MainThread ->>MixerSource:Reposition(time)
    Note over MixerSource, WaveTrack: Done for each WaveTrack<br/>because of possible<br/>sample rate discrepancies
    MixerSource ->> WaveTrack:TimeToLongSamples(time)
    Note over WaveTrack:Time-warp to sample pos
    WaveTrack -->> MixerSource:start

    User -->> MainThread:Play
    MainThread->>ProjectAudioManager:PlayPlayRegion()
    Note over ProjectAudioManager:over all tracks
    ProjectAudioManager->>WaveTrack:GetBeginTime()
    Note over WaveTrack:Difficulty: time pos of clip B<br/>depends on stretch of clip A<br/>hence warping should be here
    WaveTrack->>WaveClip:GetPlayStartTime()
    WaveClip-->>WaveTrack:t
    Note over WaveTrack:t0 = warp(t)
    WaveTrack-->>ProjectAudioManager:t0
    Note over ProjectAudioManager:same for end time<br/>yielding t1
    ProjectAudioManager->>AudioIO:StartStream(min(t0s), max(t1s))
    AudioIO--)AudioThread:TrackBufferExchange
    Note over AudioIO:waiting...
    AudioThread->>AudioIO:ProcessPlaybackSlices()
    AudioIO->>PlaybackPolicy:GetPlaybackSlice()
    Note over PlaybackPolicy:converts t0 and t1 to samples
    PlaybackPolicy-->>AudioIO:toProduce = abs(t1 - t0)*SR
    AudioIO ->> MixerSource:Acquire(toProduce)
    MixerSource->>SampleTrackCache:GetFloats(start, toProduce)
    Note over SampleTrackCache:Gets discontinuous samples from<br/>clips,  and copies sequence into mOverlapBuffer,<br/>which it returns pointer to.
    SampleTrackCache ->> WaveTrack:GetBestBlockSize(start0)
    Note over WaveTrack:if start0 is within clip,<br/>remaining samples till clip end,<br/>otherwise 2^18
    WaveTrack -->> SampleTrackCache:len0
    SampleTrackCache->>WaveTrack:GetFloats(start0, len0)
    WaveTrack->>WaveTrack:Get(start0, len0)
    WaveTrack-->>SampleTrackCache:true/false
    Note over SampleTrackCache:...and so on.
    SampleTrackCache-->>AudioThread:""
    AudioThread--)AudioIO:done

    User -->> MainThread:draw sample
    MainThread ->> SampleHandle:Click(event)
    SampleHandle ->> WaveTrack:TimeToLongSamples(event.x)
    WaveTrack -->> SampleHandle:start = event.x*SR
    SampleHandle ->> WaveTrack:Set(start, 1, y)
```

# Threading Model
```mermaid
sequenceDiagram
participant MainThread
participant AudioIO
MainThread ->> AudioIO:StartStream
AudioIO->>AudioIO:mAudioThreadShouldCallTrackBufferExchangeOnce = true
```