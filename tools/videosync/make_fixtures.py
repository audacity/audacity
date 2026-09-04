#!/usr/bin/env python3
#
# Audacity: A Digital Audio Editor
#
# Generates the A/V sync measurement fixtures.
#
# Each fixture carries a burnt-in frame counter and a high-contrast flash patch
# that lights up on marker frames, plus an audio track that is silent except for
# a single-sample impulse placed at the exact first sample of each marker frame.
# Point a high-speed camera at the screen while the fixture plays and the offset
# between the flash and the click is the end-to-end audio/video offset, speaker
# to screen. Nothing in the application needs to exist for this to be usable, so
# the baseline can be captured before any of the video code is written.
#
# The audio reference is lossless. A perceptual encoder spreads a single-sample
# impulse across its whole window (~20 ms), which is wider than the error being
# measured, so the .mkv fixture carries FLAC and is the one to measure against.
# The lossy siblings exist for a different purpose: each container reports its
# audio start time in a different time base, and the importer currently converts
# that value as though it were always microseconds, so they are the regression
# corpus for that bug rather than references for sync.
#
# Requires ffmpeg and ffprobe on PATH. Standard library only.
#
# Usage:
#   ./make_fixtures.py                    # write fixtures/ next to this script
#   ./make_fixtures.py --duration 60      # a longer clip for drift measurement
#   ./make_fixtures.py --click-ms 2       # widen the click for phone capture

import argparse
import array
import json
import os
import shutil
import subprocess
import sys
import wave
from fractions import Fraction

HERE = os.path.dirname(os.path.abspath(__file__))
FONT_CANDIDATES = [
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf",
    "/usr/share/fonts/dejavu/DejaVuSansMono-Bold.ttf",
    "/Library/Fonts/Menlo.ttc",
    "/System/Library/Fonts/Menlo.ttc",
    "C:/Windows/Fonts/consolab.ttf",
]

WIDTH, HEIGHT = 1280, 720
SAMPLE_RATE = 48000
CHANNELS = 2
PATCH = 360  # side of the flash patch, in pixels


def find_font():
    for path in FONT_CANDIDATES:
        if os.path.exists(path):
            return path
    sys.exit(
        "No monospace font found. Pass --font with a path to a .ttf/.ttc file."
    )


def require(tool):
    if shutil.which(tool) is None:
        sys.exit("%s not found on PATH." % tool)


def run(args):
    proc = subprocess.run(args, capture_output=True, text=True)
    if proc.returncode != 0:
        sys.stderr.write(proc.stderr[-4000:])
        sys.exit("Command failed: %s" % " ".join(args[:6]))
    return proc.stdout


def marker_frames(total_frames, interval):
    return list(range(0, total_frames, interval))


def impulse_index(frame, fps):
    """First audio sample belonging to a video frame, rounded to nearest.

    Exact for integer frame rates. For 30000/1001 the boundary falls between
    samples, so the rounded value is what the manifest records and what any
    measurement must be compared against.
    """
    return int(round(frame * SAMPLE_RATE / float(fps)))


def write_reference_wav(path, total_frames, fps, markers, click_samples):
    total_samples = int(round(total_frames * SAMPLE_RATE / float(fps)))
    buf = array.array("h", bytes(total_samples * CHANNELS * 2))
    placed = []
    for frame in markers:
        start = impulse_index(frame, fps)
        if start + click_samples > total_samples:
            continue
        for s in range(start, start + click_samples):
            buf[s * CHANNELS] = 32767
            buf[s * CHANNELS + 1] = 32767
        placed.append({"frame": frame, "sample": start,
                       "seconds": start / float(SAMPLE_RATE)})
    with wave.open(path, "wb") as w:
        w.setnchannels(CHANNELS)
        w.setsampwidth(2)
        w.setframerate(SAMPLE_RATE)
        w.writeframes(buf.tobytes())
    return placed, total_samples


def video_filter(font, interval, label):
    """Flash patch on marker frames, plus a burnt-in counter and timecode.

    The patch is drawn first so the readouts stay legible on top of it, and the
    counter is large enough to read off a slow-motion capture.
    """
    px = (WIDTH - PATCH) // 2
    esc = lambda s: s.replace(",", r"\,").replace(":", r"\:")
    return ",".join([
        "drawbox=x=%d:y=40:w=%d:h=%d:color=white@1.0:t=fill:enable='%s'"
        % (px, PATCH, PATCH, esc("eq(mod(n,%d),0)" % interval)),
        "drawtext=fontfile=%s:text='%%{frame_num}':fontcolor=white:"
        "fontsize=110:x=(w-text_w)/2:y=440" % font,
        "drawtext=fontfile=%s:text='%%{pts%s}':fontcolor=0xB0B0B0:"
        "fontsize=52:x=(w-text_w)/2:y=570" % (font, r"\:hms"),
        "drawtext=fontfile=%s:text='%s':fontcolor=0x808080:"
        "fontsize=30:x=30:y=30" % (font, label),
        "format=yuv420p",
    ])


# Container matrix. The mkv is the sync reference; the rest exercise the
# start_time conversion path in the FFmpeg importer, each in a different
# time base.
TARGETS = [
    {
        "name": "sync{tag}.mkv",
        "note": "lossless reference - measure sync against this one",
        "vargs": ["-c:v", "libsvtav1", "-crf", "40", "-preset", "8"],
        "aargs": ["-c:a", "flac"],
        "muxargs": [],
        "reference": True,
    },
    {
        "name": "sync{tag}.webm",
        "note": "AV1 + Opus; Opus carries a codec pre-skip",
        "vargs": ["-c:v", "libsvtav1", "-crf", "40", "-preset", "8"],
        "aargs": ["-c:a", "libopus", "-b:a", "160k"],
        "muxargs": [],
        "reference": False,
    },
    {
        "name": "sync{tag}.mp4",
        "note": "H.264 + AAC; AAC encoder priming is not stripped on import",
        "vargs": ["-c:v", "libx264", "-crf", "20", "-preset", "veryfast"],
        "aargs": ["-c:a", "aac", "-b:a", "192k"],
        "muxargs": [],
        "reference": False,
    },
    {
        "name": "sync{tag}.ts",
        "note": "MPEG-TS; 1/90000 time base, the largest importer error",
        "vargs": ["-c:v", "libx264", "-crf", "20", "-preset", "veryfast"],
        "aargs": ["-c:a", "aac", "-b:a", "192k"],
        "muxargs": ["-f", "mpegts"],
        "reference": False,
    },
    {
        # Tagged as PQ. The backend refuses high dynamic range rather than
        # decoding it as ordinary gamma, which puts reference white near
        # middle grey and looks merely dark rather than obviously wrong.
        "name": "hdr_pq.mp4",
        "once": True,
        "note": "tagged SMPTE 2084; must be refused, not shown dark",
        "vargs": ["-c:v", "libx264", "-crf", "20", "-preset", "veryfast",
                  "-color_trc", "smpte2084", "-colorspace", "bt2020nc",
                  "-color_primaries", "bt2020"],
        "aargs": ["-c:a", "aac", "-b:a", "128k"],
        "muxargs": [],
        "reference": False,
    },
    {
        # 4:2:2 chroma, which the converter does not handle. Without this the
        # only test of the unsupported-format path is a synthetic one.
        "name": "yuv422.mp4",
        "once": True,
        "note": "yuv422p; must report an unsupported pixel format",
        "vargs": ["-c:v", "libx264", "-crf", "20", "-preset", "veryfast",
                  "-pix_fmt", "yuv422p"],
        "aargs": ["-c:a", "aac", "-b:a", "128k"],
        "muxargs": [],
        "reference": False,
    },
    {
        # A plain MP4 out of ffmpeg reports start_pts 0 because libavformat
        # applies the edit list that carries the AAC priming, so the importer
        # bug never fires on it. Forcing a container offset produces the MP4
        # form of the same failure: a large start_pts in a 1/48000 time base.
        "name": "sync{tag}-offset.mp4",
        "note": "H.264 + AAC with a 1.5 s container offset; MP4 form of the bug",
        "vargs": ["-c:v", "libx264", "-crf", "20", "-preset", "veryfast"],
        "aargs": ["-c:a", "aac", "-b:a", "192k"],
        "muxargs": ["-output_ts_offset", "1.5"],
        "reference": False,
    },
]


def probe(path):
    out = run(["ffprobe", "-v", "error", "-show_streams", "-show_format",
               "-print_format", "json", path])
    return json.loads(out)


def analyse_start_times(path):
    """Record what the importer's start_time handling will do to this file.

    ImportFFmpeg divides the raw start_time by AUDACITY_AV_TIME_BASE (1e6)
    regardless of the stream's own time base, so the silence it inserts is
    wrong by the ratio between the two.
    """
    info = probe(path)
    result = {"audio": None, "video": None}
    for stream in info.get("streams", []):
        kind = stream.get("codec_type")
        if kind not in ("audio", "video") or result.get(kind) is not None:
            continue
        tb = stream.get("time_base", "1/1")
        start_pts = stream.get("start_pts")
        entry = {
            "codec": stream.get("codec_name"),
            "time_base": tb,
            "start_pts": start_pts,
            "start_time_sec": (
                float(Fraction(tb) * start_pts) if start_pts is not None else None
            ),
        }
        if kind == "audio" and start_pts is not None:
            correct = float(Fraction(tb) * start_pts)
            buggy = start_pts / 1e6
            entry["importer_correct_silence_sec"] = correct
            entry["importer_actual_silence_sec"] = buggy
            entry["importer_error_sec"] = correct - buggy
        result[kind] = entry
    return result


def build(args):
    require("ffmpeg")
    require("ffprobe")
    font = args.font or find_font()
    outdir = args.outdir or os.path.join(HERE, "fixtures")
    os.makedirs(outdir, exist_ok=True)

    manifest = {
        "generator": "tools/videosync/make_fixtures.py",
        "width": WIDTH,
        "height": HEIGHT,
        "sample_rate": SAMPLE_RATE,
        "channels": CHANNELS,
        "click_samples": args.click_samples,
        "flash_patch_px": PATCH,
        "clips": [],
    }

    for clipIndex, fps_str in enumerate(args.fps):
        fps = Fraction(fps_str)
        tag = fps_str.replace("/", "_").replace(".", "p")
        total_frames = int(round(args.duration * float(fps)))
        interval = int(round(float(fps)))  # one marker per second
        markers = marker_frames(total_frames, interval)
        label = "AUDACITY A/V SYNC  %s fps  %d Hz" % (fps_str, SAMPLE_RATE)

        wav = os.path.join(outdir, "reference%s.wav" % tag)
        placed, total_samples = write_reference_wav(
            wav, total_frames, fps, markers, args.click_samples)

        clip = {
            "fps": fps_str,
            "fps_float": float(fps),
            "duration_sec": args.duration,
            "total_frames": total_frames,
            "total_samples": total_samples,
            "marker_interval_frames": interval,
            "markers": placed,
            "reference_wav": os.path.basename(wav),
            "files": [],
        }

        for target in TARGETS:
            # Format fixtures describe a pixel format or a transfer function,
            # not a frame rate, so one copy is enough.
            if target.get("once") and clipIndex > 0:
                continue

            name = target["name"].format(tag=tag)
            path = os.path.join(outdir, name)
            cmd = ["ffmpeg", "-y", "-hide_banner", "-loglevel", "error",
                   "-f", "lavfi",
                   "-i", "color=c=black:s=%dx%d:r=%s:d=%s"
                         % (WIDTH, HEIGHT, fps_str, args.duration),
                   "-i", wav,
                   "-vf", video_filter(font, interval, label),
                   "-g", str(interval),
                   "-shortest"]
            cmd += target["vargs"] + target["aargs"] + target["muxargs"] + [path]
            print("  %s" % name)
            run(cmd)
            entry = {
                "file": name,
                "note": target["note"],
                "sync_reference": target["reference"],
                "streams": analyse_start_times(path),
                "size_bytes": os.path.getsize(path),
            }
            clip["files"].append(entry)

        manifest["clips"].append(clip)

    mpath = os.path.join(outdir, "manifest.json")
    with open(mpath, "w") as f:
        json.dump(manifest, f, indent=2)
        f.write("\n")

    gitignore = os.path.join(outdir, ".gitignore")
    if not os.path.exists(gitignore):
        with open(gitignore, "w") as f:
            f.write("# Generated by make_fixtures.py; regenerate rather than commit.\n*\n")

    return manifest, outdir


def report(manifest, outdir):
    print("\nFixtures in %s\n" % outdir)
    for clip in manifest["clips"]:
        print("%s fps, %d frames, %d markers"
              % (clip["fps"], clip["total_frames"], len(clip["markers"])))
        head = "  %-22s %-9s %-12s %14s %14s" % (
            "file", "time_base", "start_pts", "should insert", "will insert")
        print(head)
        print("  " + "-" * (len(head) - 2))
        for entry in clip["files"]:
            audio = entry["streams"].get("audio") or {}
            correct = audio.get("importer_correct_silence_sec")
            actual = audio.get("importer_actual_silence_sec")
            fmt = lambda v: "-" if v is None else "%.6f s" % v
            print("  %-22s %-9s %-12s %14s %14s"
                  % (entry["file"], audio.get("time_base", "-"),
                     audio.get("start_pts", "-"), fmt(correct), fmt(actual)))
        print()


def verify(outdir):
    """Decode the reference clip back and confirm it is what the manifest says.

    A fixture nobody has checked is worse than no fixture, because every later
    measurement inherits its error silently. This decodes the lossless audio and
    locates the impulses, then samples the mean luma of the flash patch, and
    compares both against the manifest.
    """
    mpath = os.path.join(outdir, "manifest.json")
    if not os.path.exists(mpath):
        sys.exit("No manifest in %s; generate the fixtures first." % outdir)
    with open(mpath) as f:
        manifest = json.load(f)

    ok = True
    for clip in manifest["clips"]:
        ref = next((e for e in clip["files"] if e["sync_reference"]), None)
        if ref is None:
            continue
        path = os.path.join(outdir, ref["file"])
        print("%s" % ref["file"])

        # --- audio: impulses land on the exact samples the manifest claims ---
        tmp_wav = os.path.join(outdir, ".verify.wav")
        run(["ffmpeg", "-v", "error", "-y", "-i", path, "-map", "0:a",
             "-c:a", "pcm_s16le", "-f", "wav", tmp_wav])
        with wave.open(tmp_wav, "rb") as w:
            frames, ch = w.getnframes(), w.getnchannels()
            samples = array.array("h")
            samples.frombytes(w.readframes(frames))
        os.remove(tmp_wav)
        found = [i for i in range(frames) if samples[i * ch] > 16000]
        expected = [m["sample"] for m in clip["markers"]]
        audio_ok = found == expected
        ok = ok and audio_ok
        print("  audio  impulses %s  (%d expected, %d found)"
              % ("OK" if audio_ok else "MISMATCH", len(expected), len(found)))
        if not audio_ok:
            print("    expected %s" % expected[:12])
            print("    found    %s" % found[:12])

        # --- video: the flash patch lights on exactly the marker frames ---
        px = (WIDTH - PATCH) // 2
        nframes = min(clip["total_frames"], clip["marker_interval_frames"] * 4)
        tmp_raw = os.path.join(outdir, ".verify.raw")
        run(["ffmpeg", "-v", "error", "-y", "-i", path,
             "-vf", "crop=%d:%d:%d:40,scale=1:1" % (PATCH, PATCH, px),
             "-frames:v", str(nframes), "-f", "rawvideo", "-pix_fmt", "gray",
             tmp_raw])
        with open(tmp_raw, "rb") as f:
            luma = f.read()
        os.remove(tmp_raw)
        lit = [i for i, v in enumerate(luma) if v > 200]
        want = [f for f in range(0, nframes, clip["marker_interval_frames"])]
        video_ok = lit == want
        ok = ok and video_ok
        print("  video  flash    %s  (first %d frames)"
              % ("OK" if video_ok else "MISMATCH", nframes))
        if not video_ok:
            print("    expected %s" % want)
            print("    found    %s" % lit)
        print()

    print("VERIFY %s" % ("PASSED" if ok else "FAILED"))
    return 0 if ok else 1


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--verify", action="store_true",
                   help="check existing fixtures against the manifest")
    p.add_argument("--outdir")
    p.add_argument("--font")
    p.add_argument("--duration", type=float, default=30.0,
                   help="clip length in seconds (default 30)")
    p.add_argument("--fps", nargs="+", default=["25", "30000/1001"],
                   help="frame rates to generate (default: 25 30000/1001)")
    p.add_argument("--click-ms", type=float, default=None,
                   help="widen the click; default is a single sample")
    args = p.parse_args()

    args.click_samples = 1
    if args.click_ms:
        args.click_samples = max(1, int(round(args.click_ms * SAMPLE_RATE / 1000.0)))

    if args.verify:
        require("ffmpeg")
        sys.exit(verify(args.outdir or os.path.join(HERE, "fixtures")))

    manifest, outdir = build(args)
    report(manifest, outdir)
    print("Check them with:  %s --verify" % os.path.basename(__file__))


if __name__ == "__main__":
    main()
