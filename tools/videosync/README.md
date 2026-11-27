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

| File | time_base | start_pts | Container start | Should insert | Will insert |
|---|---|---|---|---|---|
| `sync25.mkv` | 1/1000 | 0 | 0 | 0 s | 0 s |
| `sync25.webm` | 1/1000 | -7 | -0.007 s | 0 s | 0 s, guarded |
| `sync25.mp4` | 1/48000 | 0 | 0 | 0 s | 0 s |
| `sync25.ts` | 1/90000 | 131280 | 1.458667 s | **0 s** | 0.131280 s |
| `sync25-offset.mp4` | 1/48000 | 70944 | 1.478 s | **0 s** | 0.070944 s |

An earlier version of this table put 1.458667 s and 1.478 s in the "should
insert" column. That was wrong, and worth spelling out because it prescribes
the most damaging of the three possible fixes.

`AVStream::start_time` is the stream's position on the *container's* timeline,
not its offset from the other streams. For MPEG-TS that origin is the
broadcast PCR, which is arbitrary and can sit hours from zero; the 33-bit
counter wraps every 95443 s. Inserting it as silence prepends the container's
clock origin to the audio - for a file muxed an hour into a PCR epoch, an hour
of silence in front of a ten second clip.

What matters is the stream's offset *relative to the container start*, and
measured across a twenty file corpus that value is **zero for every
single-audio-stream file**, including all of the above. libavformat sets the
container start time to the minimum across streams, and has already applied
Opus pre-skip and MP4/Matroska edit lists before the importer sees anything.

It is only non-zero for genuinely multi-stream files. Muxing a second audio
stream two seconds late and reading it back recovers 2.000000 s exactly under
the container-relative rule, in both Matroska and MPEG-TS - where the current
code recovers 0.001979 s and 0.311280 s respectively, and a units-only fix
recovers neither.

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

## videoprobe

`videoprobe.cpp` exercises the seek path the video backend will use — keyframe
seek backwards, flush the decoder, decode forward to the frame whose
presentation interval contains the target — and converts the result with the
same integer YUV to RGB routine that the backend will use.

It is self-checking against the fixture: the flash patch is lit on exactly one
frame per second, so "did the seek land where it claimed" is a measurement.
Targets are visited out of order so every one forces a real seek rather than
being served by decoding forward.

```
g++ -O2 -o videoprobe videoprobe.cpp \
    $(pkg-config --cflags --libs libavformat libavcodec libavutil libswscale)
./videoprobe fixtures/sync25.mkv 25 1.0
```

It links swscale, which the feature itself never will. That is the point: it is
the only place the hand-written colour conversion can be diffed against a
reference implementation, and it is worth keeping for exactly that reason.

### What it establishes

| Fixture | Seek accuracy | Converter vs swscale |
|---|---|---|
| `sync25.mkv` (AV1) | 10/10 exact | mean 0.14, max 1 |
| `sync25.webm` (AV1) | 10/10 exact | mean 0.14, max 1 |
| `sync25.mp4` (H.264) | 10/10 exact | mean 0.14, max 1 |
| `sync25-offset.mp4` | 10/10 exact once anchored | mean 0.00, max 1 |
| `sync25.ts` | fails, see below | mean 0.14, max 1 |

Converter figures are mean and maximum absolute per-channel difference against
`sws_scale`, at native size so the colour matrix is isolated from the scaler.
Downscaled to 640x360 against `SWS_AREA` the maximum is 2. No pixel in any
fixture differs by more than 2, which is rounding.

### Two findings worth carrying into the implementation

**Anchoring to the stream start time is required, not a refinement.**
`sync25-offset.mp4` fails on every target without it and passes on every target
with it. The conversion from content-relative time to a frame timestamp has to
add the stream's `start_time`, because frame timestamps live on the container
timeline and that timeline does not begin at zero.

**Audio and video start at different times within one file.** In `sync25.ts`:

```
video  start_pts 133200 @ 1/90000 = 1.480000 s
audio  start_pts 131280 @ 1/90000 = 1.458667 s
                           difference 21.33 ms
```

21.33 ms is 1024 samples at 48 kHz, which is the AAC encoder priming: the audio
stream begins that much earlier because it opens with priming samples that are
not content.

The anchor is therefore the **video** stream's start time, not the audio's.
That reads backwards at first, since it is the audio Audacity imports, but
libavformat strips the priming while demuxing — through the edit list in MP4 —
so the first imported sample lines up with the video start rather than with the
raw audio start. Anchoring on the audio start instead shifts the picture by the
priming duration, which at 25 fps is over half a frame, and
`ContainerOffsetDoesNotShiftTheContent` in the backend tests fails on exactly
that.

The residual is not bounded at about 21 ms, which an earlier version of this
file claimed. That figure is the gap between the two streams' own start times;
the observable error also includes whatever silence the importer inserts. On
`sync25.ts` the audio starts at 1.458667 s, the video at 1.480000 s, and the
importer prepends 0.131280 s, so picture and sound sit **153 ms** apart.

Correcting the importer to the container-relative rule takes that to 21.3 ms.
Correcting only the units would take it to -1.480 s, which is worse than the
bug. That change is not part of this branch: it alters where every AAC,
MPEG-TS and LAME MP3 import lands on the timeline, which belongs to whoever
owns the importer rather than being a side effect of adding a video panel.

**MPEG-TS needed a further fix, and the mechanism is not what it looks like.**
`av_seek_frame` on MPEG-TS is *exact on the decode timestamp* — it reliably
lands on the largest DTS at or before the target — but it is *blind to
keyframes*, hitting one only by chance. The decoder then has to discard
forward to the next keyframe, so the first frame it can emit may be a whole
group of pictures past the requested time. Near the end of a file the seek can
land past the last frame and produce nothing at all, which showed up as the
panel silently keeping the previous picture.

An earlier version of this file concluded that a keyframe index was the fix.
That was wrong, and the measurements are worth recording so nobody re-derives
it:

- `avformat_seek_file` with a bounded `max_ts` behaves identically to
  `av_seek_frame(BACKWARD)` on MPEG-TS — the same targets miss.
- `AVSEEK_FLAG_ANY` is worse than nothing: it breaks Matroska and MP4 into the
  same failure mode.
- Feeding libavformat a self-scanned index via `av_add_index_entry` changes
  nothing, because the MPEG-TS demuxer exposes `read_timestamp` and takes the
  binary-search path regardless.
- An index is also unavailable exactly when it would be needed most — a user
  who seeks two seconds after attaching a long file — so the ordinary seek
  path has to be correct anyway. That is the whole job.

The fix is to verify and retry: seek, decode, and check whether the first
frame produced actually starts at or before the target. If it does not, seek
again from further back, doubling the backoff. Landing early is only slower,
never wrong, because the decoder walks forward to the target either way; only
landing late is wrong, and that is precisely what the check detects. Files
that already seek correctly take exactly one seek, which the tests assert.
