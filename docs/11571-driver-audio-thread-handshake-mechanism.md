# [11571](https://github.com/audacity/audacity/issues/11571)

## Intro

Issue [Audacity freezes when adding a track](https://github.com/audacity/audacity/issues/11571) exposed a deadlock, that was never uncovered before (although foreseen by Paul Licameli), but due to programmatic-fast start/stop monitoring event sequence finally was reproduced.

We draw here an explanation to strengthen the intuitive understanding of an otherwise rather complex construct. This is a simplification.

## Happy path

```mermaid
sequenceDiagram
participant MainThread@{"type":"control"}
participant AudioIO
participant AudioThread@{"type":"control"}

note over MainThread, AudioThread:Startup
MainThread ->> AudioIO: mACK = none

note over MainThread, AudioThread:Running ...
AudioThread ->> AudioIO: IsMonitoring()
AudioIO -->> AudioThread: false
note over AudioThread:do nothing

MainThread ->> AudioIO: StartMonitoring()
AudioIO -->> MainThread:

AudioThread ->> AudioIO: IsMonitoring()
AudioIO -->> AudioThread: true
note over AudioThread:Ok, then ...
AudioThread ->> AudioIO: mACK = stop
AudioIO -->> AudioThread:

MainThread ->> AudioIO: StopMonitoring
loop while mACK != stop
note over MainThread:already satisfied
end
MainThread ->> AudioIO:mACK = none
AudioIO -->> MainThread:
```

## Deadlock

`StopMonitoring()` comes after `StartMonitoring()`, _before_ AudioThread can set `mACK = stop`

```mermaid
sequenceDiagram
participant MainThread@{"type":"control"}
participant AudioIO
participant AudioThread@{"type":"control"}

MainThread ->> AudioIO: StartMonitoring()
AudioIO -->> MainThread:
MainThread ->> AudioIO: StopMonitoring()
loop while mACK != stop
MainThread ->> MainThread:spins forever
end
AudioThread ->> AudioIO: IsMonitoring()
AudioIO -->> AudioThread: false
note over AudioThread:do nothing
```

## StartMonitoring handshake

```mermaid
sequenceDiagram
participant MainThread@{"type":"control"}
participant AudioIO
participant AudioThread@{"type":"control"}

note over AudioIO:mACK = none
MainThread ->> AudioIO: StartMonitoring()
note over AudioIO:this time we shake hands
AudioIO ->> AudioIO: mStart = true
loop while mACK != start
MainThread ->> MainThread:spin
alt if mStart == true
AudioThread ->> AudioIO: mACK = start
AudioIO -->> AudioThread:
end
end
AudioIO ->> AudioIO: mACK = none
AudioIO -->> MainThread:
MainThread ->> AudioIO: StopMonitoring()
```

## Notes

Those diagrams are a simplification. The handshake mechanism in fact primarily applies to start/stop of playback or recording, ie., of buffer exchange between the audio thread (where audio is processed) and the driver thread (soundcard IO).

In fact, for audio monitoring, one needs an _open stream_ but not the audio thread at all, and even less buffer exchange. There are hence two ways the deadlock can be resolved:

- extending the handshake mechanism to monitoring - small diff but adds complexity
- decoupling monitoring from the audio thread completely - bigger diff, but final state should be simpler.
