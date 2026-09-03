# A/V sync measurement fixtures

Test material and a procedure for measuring how far the picture drifts from the
sound, end to end, speaker to screen.

The point of measuring first is that every acceptance criterion for the video
work is otherwise self-referential: a video panel that asks the playhead where
it is, and is judged by whether it agrees with the playhead, will pass happily
while sitting a constant 60 ms behind what the user actually hears. Only a
measurement taken outside the application can catch that, and it has to be taken
before the panel exists so there is a baseline to compare against.

## Generating

```
./make_fixtures.py
```

Needs `ffmpeg` and `ffprobe` on `PATH`; otherwise standard library only. Writes
to `fixtures/`, which is ignored by git — regenerate rather than commit, the
clips are large and the manifest records everything needed to reproduce them.

```
./make_fixtures.py --duration 60          # longer clip, for drift over time
./make_fixtures.py --fps 25               # just one frame rate
./make_fixtures.py --click-ms 2           # widen the click for phone capture
./make_fixtures.py --verify               # check fixtures against the manifest
```

`--verify` decodes the lossless reference back, locates the impulses, samples
the flash patch luma, and compares both against `manifest.json`. Run it after
generating on a new machine. An unchecked fixture is worse than no fixture,
because every later measurement inherits its error without saying so.

## What is in a clip

Each clip is 1280x720, 48 kHz stereo, with a marker once per second:

- a **flash patch** — a 360 px white square, lit for exactly one frame
- a **burnt-in frame counter** and timecode, large enough to read off a
  slow-motion capture
- a **single-sample impulse** in the audio, on the exact first sample of the
  marker frame

`manifest.json` records the exact sample index of every impulse, so a
measurement is compared against a number rather than against an impression.

The audio reference is lossless on purpose. A perceptual encoder spreads a
single-sample impulse across its entire window, roughly 20 ms, which is wider
than the error being measured — so `sync25.mkv` (AV1 + FLAC) is the file to
measure sync against. The other containers are not sync references; they are a
regression corpus for the importer, described below.

## Measuring

Play a fixture in the application under test with the audio going to real
speakers, and capture the screen and the sound together. Any of these works:

- **Phone slow-motion**, 240 fps, framing the flash patch with the phone's own
  microphone picking up the click. Cheap, and at 240 fps one captured frame is
  4.2 ms, which is finer than a video frame at any normal rate. Use
  `--click-ms 2` so the click survives the phone's microphone AGC.
- **Photodiode taped to the flash patch, plus a microphone**, both into a
  two-channel scope or a second audio interface. The most accurate option, and
  the only one that resolves below a millisecond.
- **A second machine capturing HDMI**, if the display chain is what is being
  characterised.

Positive offset means the picture is late — the flash arrives after the click.

Measure at each marker across the clip rather than once, and record the spread
as well as the mean. A constant offset is a latency figure and can be
compensated; an offset that grows across the clip is drift and cannot.

## Baseline results

Fill this in before writing any of the video code, and again at the end of each
milestone. Host API matters as much as the platform, so record it.

| Platform | Host API | Device | Buffer | Mean offset | Spread | Drift over 60 s | Date |
|---|---|---|---|---|---|---|---|
| Linux | ALSA | | | | | | |
| Linux | PulseAudio | | | | | | |
| Linux | JACK | | | | | | |
| macOS | CoreAudio | | | | | | |
| Windows | WASAPI | | | | | | |
| Windows | MME | | | | | | |
| Windows | ASIO | | | | | | |

The audio path already reports its own latency compensation — the playhead is
published against `steady_clock::now()` plus the hardware playback latency, so
the number above should come out small and constant. If it does not, that is
worth knowing before any of it is blamed on the video decoder.

## The importer start_time corpus

`FFmpegImportFileHandle::Import` reads `AVStream::start_time` and converts it to
seconds by dividing by `AUDACITY_AV_TIME_BASE`, which is fixed at 1000000. That
is only correct when the stream's time base happens to be 1/1000000. It carries
a comment saying the author was unsure what the field meant.

The generator probes each fixture and records what the importer will actually
do. Measured on a 10 second clip at 25 fps:

| File | time_base | start_pts | Should insert | Will insert | Error |
|---|---|---|---|---|---|
| `sync25.mkv` | 1/1000 | 0 | 0 s | 0 s | none |
| `sync25.webm` | 1/1000 | -7 | -0.007 s | -0.000007 s | none, guarded |
| `sync25.mp4` | 1/48000 | 0 | 0 s | 0 s | none |
| `sync25.ts` | 1/90000 | 131280 | 1.458667 s | 0.131280 s | **1.327 s, ~33 frames** |
| `sync25-offset.mp4` | 1/48000 | 70944 | 1.478000 s | 0.070944 s | **1.407 s, ~35 frames** |

Two things worth knowing, both of which contradict the obvious reading of the
code:

**A plain MP4 out of ffmpeg is unaffected.** The AAC encoder priming is carried
in an edit list (`elst`, media time 1024, which is the 21 ms at 48 kHz) and
libavformat applies it while demuxing, so the importer is handed a
`start_time` of 0 and inserts nothing, which is correct. The MP4 form of the
bug needs a genuine container offset, which is what `sync25-offset.mp4` is for.
Files written by other muxers, or read with `-ignore_editlist`, will behave
differently — this is worth re-checking against camera originals rather than
assuming.

**Negative start times are ignored.** The importer guards on
`streamStartTime > 0`, so the Opus pre-skip that WebM reports as -7 ms never
reaches the conversion. That happens to be right, since libavformat has already
accounted for it.

So the bug is real but narrower than the raw code reading suggests: it needs a
non-zero positive `start_time` in a time base finer than microseconds. MPEG-TS
is the common case, and it is a common delivery format for camera and broadcast
material — exactly the kind of thing this feature attracts.
