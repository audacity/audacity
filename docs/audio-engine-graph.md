## Audio graph

```mermaid
flowchart LR
    T1["track chain (w/ fx)"] --> Q1[LockFreeQueueNode] --> M1["MeterTapNode"]
    T2["track chain (w/ fx)"] --> Q2[LockFreeQueueNode] --> M2["MeterTapNode"]
    W((a worker per track)) -.->|render into| Q1 & Q2
    S3[PlayheadMappingNode] --> Q3[LockFreeQueueNode] --> E3[EndOfTrackNode] --> IM3[SequentialMixNode]
    I3[MonitoredInputNode] --> IM3 --> FX3[EffectsNode] --> MS3[MuteSoloNode] --> GP3[GainPanNode] --> M3["MeterTapNode"]
    W3((worker)) -.->|reads into| Q3
    M1 & M2 & M3 -->|one after the other| Mix[SequentialMixNode] --> MFX["EffectsNode (master)"] --> MMeter["MeterTapNode (master)"] --> MSend[MeterSendNode] -->|callback| D(device)
    style Q1 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style M1 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style Q2 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style M2 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style S3 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style Q3 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style E3 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style IM3 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style I3 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style FX3 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style MS3 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style GP3 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style M3 fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style Mix fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style MFX fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style MMeter fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style MSend fill:#a5c9a1,stroke:#4f7a4a,color:#000
```

Here the playback tracks' chains, mute/solo and volume/pan included, stop before their meter's tap, which the callback reads
after the queue, where the device is. The monitored track's chain runs in the callback from its `EndOfTrackNode` on: its
mute/solo and volume/pan are heard at once.

Here is the track chain:

```mermaid
flowchart LR
    C[sequence's graph] --> S[PlayheadMappingNode] --> End[EndOfTrackNode] --> FX[EffectsNode] --> MS[MuteSoloNode] --> GP[GainPanNode] --> Meter[MeterTapNode]
    style S fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style End fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style FX fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style MS fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style GP fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style Meter fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style C fill:#dcdcdc,stroke:#707070,color:#000
```

### Legend

Color and shape convention throughout the graphs:

```mermaid
flowchart LR
    Ours["ours<br/>ISequenceProcessingNode"] -->|audio| Adapter["ours<br/>tracktion::graph::Node"] -->|audio| Lib("tracktion_graph's<br/>tracktion::graph::Node") -->|audio| Dev("JUCE's<br/>juce::AudioIODevice")
    Thread((thread)) -.->|renders into| Ours
    Existing["ours, existing<br/>(clip-fades branch)"] -->|audio| Ours
    style Existing fill:#dcdcdc,stroke:#707070,color:#000
    style Ours fill:#a5c9a1,stroke:#4f7a4a,color:#000
    style Adapter fill:#a3bcd4,stroke:#4a6f94,color:#000
    style Lib fill:#7395b8,stroke:#2f5378,color:#000
```

What a node needs beyond its input, it gets from a context it's given at construction:

- `IEffectsContext`: processes a group's effects on buffers, in the current block's processing scope; their latency.
- `IMuteSoloContext`: whether any track solos, this block.
- `IEndOfTrackContext`: where the play range ends, unless looping.

## New building blocks

| Block                                                       | Role                                                                                                                                                                                                                                       |
| ----------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `Au4AudioEngine : IAudioEngine`                             | Counterpart to `Au3AudioEngine`, on the devices its `IDeviceOpener` opens: one instance per audio library                                                                                                                                  |
| `AudioEngineSwitcher` (`src/audio`)                         | The exported `IAudioEngine` and `IAudioDriverController`: forwards to the engine chosen in the audio preferences or the playback toolbar                                                                                                   |
| `IDevice`, `IDeviceOpener`, `IHardwareVolume` (`src/audio`) | An open audio device, which calls back an `IDeviceCallback`; what opens them as the audio settings say; their hardware input volume                                                                                                        |
| `PortAudioDevice` (`src/portaudiodevices`)                  | PortAudio's, opened as AudioIO opens its streams (`PortAudioStream`)                                                                                                                                                                       |
| `JuceDevice` (`src/jucedevices`)                            | JUCE's                                                                                                                                                                                                                                     |
| `PortAudioStream`, `DeviceMixer` (au3, from AudioIO)        | Opens PortAudio streams as the audio settings say, minding each host's quirks; the hardware input volume and playthrough, through portmixer                                                                                                |
| `CaptureSink` (au3, from AudioIO)                           | Moves the captured input into the sequences being recorded; dropouts, latency compensation                                                                                                                                                 |
| `IoCallback`                                                | Implements `IDeviceCallback` while playing or recording: builds the chains, renders the mix, the master's and the monitored track's chains, and waits for the tracks' workers; applies the playback volume, reports what plays             |
| `MonitoringCallback`                                        | Idle monitoring: meters the input, plays it through, raw or through the effects                                                                                                                                                            |
| `RecordingCapture`                                          | Records the device's input as AudioIO does: a `CaptureSink` filled by the device callback, drained by a worker; sound activated recording                                                                                                  |
| `CaptureInput`                                              | The device's input as captured: with the input volume applied, and metered                                                                                                                                                                 |
| `PlayheadMapper : IPlayheadMapper`                          | Maps linear stream indices to the timeline positions the playhead reads: seeks, loops                                                                                                                                                      |
| `PlayheadMappingNode` (au3)                                 | Reads a sequence's graph by stream index, where an `IPlayheadMapper` says                                                                                                                                                                  |
| `ResamplingNode` (au3)                                      | Converts a sequence's rate to the device's                                                                                                                                                                                                 |
| `LockFreeQueueNode` (au3)                                   | Renders its input ahead on its own worker thread, into a lock-free queue read by stream index; on a seek, drops what it queued                                                                                                             |
| `Track`                                                     | Not a node: a track's chain as the callback keeps it: its output for the mix, its queue, its meter tap                                                                                                                                     |
| `BlockContext`, `BlockContextNode`                          | The state of the block being rendered, which the chains' nodes share (`IEffectsContext`, `IMuteSoloContext`, `IEndOfTrackContext`); one per worker rendering a whole chain, one for the device callback; set around each block by the node |
| `EndOfTrackNode`                                            | Silences the track where it ends: past the play range's end unless looping, or where a lead-in recording starts; before the effects, so that their tails ring out                                                                          |
| `EffectsNode`                                               | A group's realtime effects (a track's or the master's): reads its input ahead by their latency and primes them when reading jumps                                                                                                          |
| `MuteSoloNode`                                              | Silences the track as mute/solo say, whether another track solos coming from its `IMuteSoloContext`                                                                                                                                        |
| `GainPanNode`                                               | Volume/pan; where a mono track becomes stereo                                                                                                                                                                                              |
| `MeterTapNode`                                              | Keeps what passes, and its stream index, for a meter                                                                                                                                                                                       |
| `MeterSender`                                               | Not a node: once the device callback has rendered a block, sends to the meters what the taps kept of it, stamped with when it's heard                                                                                                      |
| `SequentialMixNode`                                         | Sums its inputs one after the other: the tracks, or the monitored track's source and the input                                                                                                                                             |
| `MonitoredInput`                                            | Not a node: the input of the block being rendered, with the input volume                                                                                                                                                                   |
| `MonitoredInputNode`                                        | The input as a source, live where its chain renders the block; a `SequentialMixNode` sums it with the monitored track's source                                                                                                             |

## The same in AudioIO

What does each block's job in `Au3AudioEngine`'s playback (`au3/libraries/au3-audio-io` and the libraries it calls). AudioIO
always renders on its audio thread (`AudioIO::AudioThread`) and plays what it rendered through one ring buffer per channel,
as the safe mode does.

In the diagram, the arrows are calls, to code with a class and file of its own; the notes on `AudioIO.cpp` are what it
does itself, inline in its functions (the file has 3127 lines; the functions' lengths are given). In brackets, our block
that does the same.

```mermaid
sequenceDiagram
participant Device as PortAudio stream
box rgba(70,130,180,0.25) AudioIO.cpp
participant CB as AudioIoCallback (callback)
participant AT as AudioIO (audio thread)
end
participant PS as PlaybackSchedule.cpp
participant MX as Mix.cpp
participant RT as RealtimeEffectManager.cpp
participant RB as RingBuffer.cpp
participant CS as CaptureSink.cpp

rect rgba(127,127,127,0.12)
note over AT: FillPlayBuffers → ProcessPlaybackSlices, 293 lines
AT ->> PS: GetPlaybackSlice<br/>[PlayheadMapper]
loop each sequence
AT ->> MX: Mixer::Process<br/>[ResamplingNode, EndOfTrackNode]
end
loop each sequence
AT ->> RT: ProcessingScope::Process(group)<br/>[EffectsNode]
note over AT: discards the effects' latency<br/>SequenceShouldBeSilent [MuteSoloNode]
end
loop each sequence
note over AT: applies GetChannelVolume [GainPanNode]<br/>sums into mMasterBuffers [SequentialMixNode]<br/>puts into Track::mBuffers [MeterTapNode]
end
AT ->> RT: ProcessingScope::Process(MasterGroup)<br/>[EffectsNode (master)]
note over AT: discards the master's latency,<br/>also from Track::mBuffers
AT ->> RB: Put (mPlaybackBuffers)<br/>[LockFreeQueueNode]
AT ->> PS: RepositionPlayback (loops)
AT ->> CS: Drain (DrainRecordBuffers)<br/>[RecordingCapture]
end

rect rgba(127,127,127,0.12)
Device ->> CB: AudioCallback, 149 lines
opt seeking
note over CB: CallbackDoSeek, 67 lines
CB ->> PS: OffsetSequenceTime
CB ->> RB: Discard
end
CB ->> CS: Push (DrainInputBuffers, 77 lines)<br/>[RecordingCapture]
note over CB: DoPlaythrough: the raw input<br/>(no [MonitoredInputNode])
note over CB: FillOutputBuffers, 168 lines
CB ->> RB: Get<br/>[LockFreeQueueNode]
note over CB: playback volume, microfade, clamp [SafeIoCallback]<br/>SendVuOutputMeterData, PushTrackMeterValues<br/>[MeterSendNode, MeterSink]
CB ->> PS: TimeQueue::Consumer<br/>[PlayheadMapper]
CB ->> Device: output
end
```
