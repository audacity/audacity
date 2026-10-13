# Cloud effects via extensions: handover

Branch `cloud-effect-poc`. Goal of this PoC: two audio.com AI effects, **voice isolation** and **stem separation**, implemented as extension effects that call audio.com synchronously, like regular destructive effects. Small audio snippets only for now (memory is not a concern yet).

## Done

- **Decode API for extensions:** `require("Audacity.AudioCodec").decode(bytes, { format, sampleRate })` returns `{ sampleRate, sampleCount, channels }`, one float32 ArrayBuffer per channel. Both options are optional (`format` = file extension hint, `sampleRate` = resample). Throws on failure.
  - `src/extensions/api/audiocodecapi.{h,cpp}`, registered in `audacityextensionsmodule.cpp`.
  - `IImporter::decode()` in `src/importexport/import/`: writes nothing to the project; runs the normal importer on a temp file (FFmpeg for `.m4a` on Linux).
  - Tested with a throwaway extension effect: 48 kHz stereo AAC decoded natively and resampled to 44.1 kHz.

## Next steps

1. **audio.com fetch for extensions.** A `fetch` that adds the user's audio.com access token, only towards audio.com hosts, so the extension never sees the token. No login flow in the extension for now. Later: only signed extensions may use it (see Decisions).
2. **`wait(ms)` helper.** Extension scripts run in a plain QJSEngine with no `setTimeout`; needed to poll the split job (or use the effect's `poll(task)` hook, called every 50 ms).
3. **Voice isolation extension.** `mode: extract_voice`; replaces the selection with the vocals stem.
4. **Stem separation extension.** `mode: full_split`; keeps the original and adds one track per stem (`writer.addTrack`), as one undoable step.

## audio.com stem separation API (backend-audio-master)

Only AI effect that returns audio. Async, polling only. Accepts any bearer token, or none.

1. `POST /tools/split` `{name, mime, size, mode}` → upload policy with a presigned S3 `PUT` URL and `success`/`fail` hook paths (relative: prepend the API host).
2. `PUT` the audio file (wav, flac, mp3, m4a; ≤ 1 GiB) to that URL.
3. `POST /tools/split/success?id=…`
4. Poll `GET /tools/split/{id}` (≥ 3 s apart) until `progress` is 100 or `error` is set.
5. Download each `result[].audio_url` (`track` = stem name). Output is 128k AAC `.m4a`.

Gotcha: audio longer than 20 min is rejected by the worker, after the upload.

## Extension effects: how they work

- Declared in the manifest: `contributes."audacity.effects": [{ id, title, group: "effect"|"generate"|"analyze"|"tools", script, factory }]`. The manifest also needs an `actions` array (a dummy one is fine).
- The script exports a factory (`exports.createEffect`) returning `{ parameters(), process(settings, projectSession, task) }`, optional `validate`, `poll`, `destroy`.
- If `process` returns a Promise, Audacity waits for it with a progress dialog and Cancel (`task.report(fraction, msg)`, `task.cancelled`).
- Audio: `require("Audacity.Project").beginEdit(name, projectSession)` → read the selection (`openReader`), write back (`createAudioWriter`, `replace`, `addTrack`), `commit()`.
- HTTP: `require("MuseApi.Http").fetch(url, {method, headers, body})`, body as string or ArrayBuffer; response `arrayBuffer()`, `json()`.
- A new extension effect only appears after one restart (registered on the first run).
- A macro (non-effect) extension can't await a Promise: its session ends when `main` returns.

## Decisions

- Effects talk to audio.com directly from the extension; project sync / checkouts are not involved for now.
- Auth is provided by the host, not by extensions: Audacity runs sign-in flows and adds tokens to requests; extensions never see tokens. audio.com is one provider among others (third-party services would declare their own).
- Only signed extensions (signed by Muse) will get the audio.com session; unsigned extensions still install and run, without it. Signing doesn't exist yet in muse extensions.
- Output should be lossless (WavPack, as used by project sync) instead of AAC. Then voice isolation can also add the non-voice stem as a track.

## Open points

- audio.com split endpoints are too permissive (anonymous calls, no ownership checks, no quotas): fine for the PoC, must be fixed before release.
- Large audio: `fetch` holds request and response bodies in memory; a 20-min stereo selection could peak around 1 GB. Needs streaming from/to temp files.
- How the extension gets installed/distributed (built-in, MuseHub, audio.com).

## Testing headlessly

- Scratch profile: XDG-isolated copy, with `known_audio_plugins.json` and `pluginregistry.cfg` copied from the real profile.
- Third-party plugins make startup ask "Validate audio plugins", which blocks offscreen and leaves the effect list empty: in a temporary harness, call `IEffectsProviderInitializer::setStartupPluginValidationPolicy(Skip)` early.
- Apply an extension effect by title: `action://effects/apply?effectId='<title>'`.
- Make a test `.m4a` with `gst-launch-1.0 filesrc location=x.wav ! wavparse ! audioconvert ! avenc_aac ! mp4mux ! filesink location=x.m4a`; serve it with `python3 -m http.server --bind 127.0.0.1`.
