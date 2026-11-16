# Main blocks

In the architectures below, always found together:

### "Clip" processing

1. reads audio from our SQLite database (slow)
2. time stretching (possibly heavy)
3. clip fades
4. clip envelope

### "Mixing"

1. per-track effects
2. master effects

# Architecture options

## JUCE only options

### Option 1: Worker

Device just does volume.

One worker thread does mixing + clip processing, behind buffer.

```mermaid
sequenceDiagram

box rgba(150,100,230,0.2) JUCE
participant Device
end
participant stream as JuceWorkerPlaybackStream
participant queue as LockFreeQueueNode
participant Worker@{"type":"control"} as LockFreeQueueNode::mWorker
participant MixNode

rect
Device ->> stream:dual callback (in + out)
stream ->> queue:GetSamples
queue ->> stream:
note over stream:global volume
stream ->> Device:
end

rect
Worker ->> queue:Produce
queue ->> MixNode:GetSamples
note over MixNode:see Mixing<br/>(source: PlaybackScheduleNode)
MixNode ->> queue:
end
```

### Option 2: Device

Device does mixing.

```mermaid
sequenceDiagram

box rgba(150,100,230,0.2) JUCE
participant Device
end
participant stream as JuceDevicePlaybackStream
participant MixNode
participant queue as LockFreeQueueNode <br/>(one per track)
participant Worker@{"type":"control"} as LockFreeQueueNode::mWorker
participant PlaybackScheduleNode

rect
Device ->> stream:dual callback (in + out)
stream ->> MixNode:GetSamples
note over MixNode:see Mixing<br/>(source: each track's LockFreeQueueNode)
MixNode ->> stream:
note over stream:global volume
stream ->> Device:
end

rect
Worker ->> queue:Produce
queue ->> PlaybackScheduleNode:GetSamples
note over PlaybackScheduleNode:pulls from current graph
PlaybackScheduleNode ->> queue:
end
```

### Common to both options

This block is common to both JUCE variants:

```mermaid
sequenceDiagram

participant caller as Caller
participant MixNode
participant TrackNode
participant SequenceRenderer
participant source as TrackNode's source

caller ->> MixNode:GetSamples
loop for each track
MixNode ->> TrackNode:GetSamples
TrackNode ->> source:GetSamples
source ->> TrackNode:
TrackNode ->> SequenceRenderer:render
note over SequenceRenderer:effects<br/>mute/solo<br/>volume/pan
SequenceRenderer ->> TrackNode:
TrackNode ->> MixNode:
end
note over MixNode:sums<br/>master effects
MixNode ->> caller:
```

## Tracktion option

```mermaid
sequenceDiagram
box rgba(150,100,230,0.2) JUCE
participant Device
end
participant stream as TracktionPlaybackStream
box rgba(230,110,90,0.2) tracktion_graph
participant graph as t::g::LockFreeMultiThreadedNodePlayer
participant sum as t::g::SummingNode
end
participant MasterNode
participant SequenceNode
participant SequenceRenderer
participant queue as LockFreeQueueNode
participant Worker@{"type":"control"} as LockFreeQueueNode::mWorker
participant PlaybackScheduleNode

rect
Device ->> stream:dual callback (in + out)
stream ->> graph:process
loop each track in parallel
graph ->> SequenceNode:process
SequenceNode ->> queue:GetSamples
queue ->> SequenceNode:
SequenceNode ->> SequenceRenderer:render
note over SequenceRenderer:effects<br/>mute/solo<br/>volume/pan
SequenceRenderer ->> SequenceNode:
SequenceNode ->> graph:
end
graph ->> sum:process
note over sum:latency compensation<br/>then sum
sum ->> graph:
graph ->> MasterNode:process
note over MasterNode:master effects
MasterNode ->> graph:

graph ->> stream:
note over stream: Now that no thread of the pool renders anymore ...
loop for each track
stream ->> SequenceNode:sendToMeter
SequenceNode ->> SequenceRenderer:sendToMeter
note over SequenceRenderer:au::audio::IAudioMeter
SequenceRenderer ->> SequenceNode:
SequenceNode ->> stream:
end
note over stream:report played range
note over stream:send to main IAudioMeter
note over stream:global volume
stream ->> Device:
end

rect
Worker ->> queue:Produce
queue ->> PlaybackScheduleNode: GetSamples
note over PlaybackScheduleNode:pulls from current graph
PlaybackScheduleNode ->> queue:
end
```

## Tracktion worker option (proposed)

Device just does volume.

One worker thread, with tracktion's pool, does mixing + clip processing, tracks in parallel, behind buffer. As Option 1, but
`MixNode` sums its tracks through a tracktion player instead of one after the other. New: `TrackGraphNode`, the glue
from tracktion's graph to our `TrackNode`.

```mermaid
sequenceDiagram

box rgba(150,100,230,0.2) JUCE
participant Device
end
participant stream as JuceWorkerPlaybackStream
participant queue as LockFreeQueueNode
participant Worker@{"type":"control"} as LockFreeQueueNode::mWorker
participant MixNode
box rgba(230,110,90,0.2) tracktion_graph
participant player as t::g::LockFreeMultiThreadedNodePlayer
participant sum as t::g::SummingNode
end
participant glue as TrackGraphNode (new)
participant TrackNode
participant SequenceRenderer
participant PlaybackScheduleNode

rect
Device ->> stream:dual callback (in + out)
stream ->> queue:GetSamples
queue ->> stream:
note over stream:global volume
stream ->> Device:
end

rect
Worker ->> queue:Produce
queue ->> MixNode:GetSamples
MixNode ->> player:process
loop each track in parallel, on the pool's threads
player ->> glue:process
glue ->> TrackNode:GetSamples
TrackNode ->> PlaybackScheduleNode:GetSamples
note over PlaybackScheduleNode:clip processing
PlaybackScheduleNode ->> TrackNode:
TrackNode ->> SequenceRenderer:render
note over SequenceRenderer:effects<br/>mute/solo<br/>volume/pan
SequenceRenderer ->> TrackNode:
TrackNode ->> glue:
glue ->> player:
end
player ->> sum:process
note over sum:sum, without latency nodes:<br/>each TrackNode compensates its own
sum ->> player:
player ->> MixNode:
note over MixNode:master effects<br/>meters
MixNode ->> queue:
end
```
