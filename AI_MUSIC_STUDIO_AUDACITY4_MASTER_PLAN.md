# AI MUSIC STUDIO — AUDACITY 4 FORK
## Master Development & Architecture Plan

**Status:** Proposed baseline for a brand-new project  
**Primary platform:** Windows 11 x64  
**Editor foundation:** Audacity 4 source fork  
**Primary composition engine:** YuE2  
**Instrument generation:** Stable Audio 3  
**Stem separation:** UVR / RoFormer family  
**Voice conversion:** RVC  
**General audio-analysis engine:** None selected; Essentia is excluded  
**Core philosophy:** Local-first, non-destructive, source-aware, provider-based.

# 1. Project Decision

Build a new AI-native music production application as a controlled fork of Audacity 4.

Audacity 4 owns conventional editing responsibilities:

- multitrack audio editing;
- clips and waveforms;
- transport;
- recording;
- audio I/O;
- import/export;
- undo/redo;
- effects and plugin hosting where supported;
- project loading/saving;
- accessibility and shortcuts.

AI Music Studio adds:

- YuE2 full-song composition and rendering;
- editable symbolic song plans;
- lyrics-aware workflows;
- RoFormer separation;
- RVC artist-voice conversion;
- Stable Audio 3 instrument replacement/generation;
- AI version management;
- model management;
- job scheduling;
- provenance;
- recovery and diagnostics.

This is not a stock Audacity plugin, web wrapper, ComfyUI frontend, or new audio engine.

# 2. Audacity 4 Baseline

Pin an exact upstream Audacity 4 commit before development begins.

The current Audacity 4 source line uses C++20, CMake, Qt 6/QML, Muse Framework foundations and a modular source structure. The fork must treat upstream synchronization as a first-class engineering concern because Audacity 4 is still undergoing significant structural change.

Initial Windows build baseline:

- Windows 11 x64;
- MSVC 2022;
- CMake;
- Ninja;
- Qt version required by the pinned Audacity revision;
- current provider-specific Python/CUDA runtimes.

Windows is a primary requirement. Production features must not require WSL.

# 3. Licensing

Audacity is GPL software. Treat the distributed editor fork as an open-source GPL application unless specialist legal review determines another compliant structure.

AI model licences are separate from the editor licence. Store machine-readable licence metadata for every provider/model and never imply that the editor's GPL licence grants rights to model weights.

YuE2 must remain behind a provider boundary because its released model weights have separate non-commercial restrictions.

Before public/commercial release, review licences for YuE2, Stable Audio 3, RoFormer/UVR, RVC and individual RVC voice models, codecs, plugin SDKs and any future adapters.

# 4. Product Principles

1. **Local-first.** Core workflows work offline once models are installed.
2. **Source before render.** Preserve useful intermediate artifacts, not just final audio.
3. **Non-destructive.** AI operations create new assets/versions.
4. **AI operations are jobs.** Generation never blocks the UI/audio thread.
5. **Simple defaults, advanced depth.** Hide provider-specific complexity unless requested.
6. **Provider-neutral architecture.** YuE2 is primary, not hard-coded.
7. **Editor remains usable without AI.** Existing audio must always open and play.

# 5. High-Level Architecture

```text
┌──────────────────────────────────────────────────────────┐
│ AI MUSIC STUDIO DESKTOP                                 │
│ Audacity 4 / C++ / Qt / QML                             │
│ Timeline · Tracks · Transport · Effects · Export        │
│ AI Studio · Lyrics · Song Plan · Versions · Models      │
└─────────────────────────┬────────────────────────────────┘
                          │ local authenticated IPC
┌─────────────────────────▼────────────────────────────────┐
│ AI RUNTIME HOST                                          │
│ Jobs · Provider Registry · Model Registry · Diagnostics  │
└──────────┬───────────┬───────────┬───────────────────────┘
           │           │           │
        YuE2       RoFormer      RVC
           │
     Stable Audio 3
```

Heavy AI dependencies must run outside the Audacity process.

# 6. Repository / Fork Strategy

Prefer new isolated Audacity/Muse modules:

```text
src/
├── aicore/
├── aiproject/
├── aijobs/
├── aimodels/
├── aistudio/
├── songplan/
├── stemtools/
├── vocaltools/
└── instrumenttools/
```

Keep AI workers outside the native app:

```text
runtime/
├── host/
├── common/
└── providers/
    ├── yue2/
    ├── roformer/
    ├── rvc/
    └── stable_audio/
```

Avoid modifying legacy `au3/` internals unless no AU4 abstraction can support the requirement.

Every unavoidable upstream patch gets documented in `UPSTREAM_PATCHES.md`.

# 7. Upstream Policy

Use:

```text
origin   = AI Music Studio fork
upstream = audacity/audacity
```

Rules:

- pin a known-good upstream commit per milestone;
- do not continuously chase master;
- keep upstream sync commits separate from feature commits;
- sync only after current milestone passes tests;
- run full regression after each upstream update;
- tag known-good integration points.

# 8. AI Runtime Host

Create a supervised local runtime service responsible for:

- provider discovery;
- model discovery;
- environment health;
- worker launch/restart;
- job scheduling;
- GPU coordination;
- progress/logs;
- cancellation;
- temp assets;
- result manifests;
- hardware detection.

Do not use the user's system Python.

Recommended environment layout:

```text
runtime/environments/
├── yue2/
├── roformer/
├── rvc/
└── stable_audio/
```

# 9. IPC

First implementation:

- bind only to loopback;
- choose a random free port;
- create a random session token;
- authenticate every request;
- use WebSocket/SSE for progress events;
- version all request/response schemas.

No external network binding by default.

# 10. Provider Contracts

```ts
interface SongGenerationProvider {
  id: string;
  displayName: string;
  getCapabilities(): Promise<SongGenerationCapabilities>;
  generatePlan?(request: SongPlanRequest): Promise<SongPlanResult>;
  renderSong(request: SongRenderRequest): Promise<SongRenderResult>;
  cancel?(jobId: string): Promise<void>;
}
```

```ts
interface StemSeparationProvider {
  id: string;
  displayName: string;
  getModels(): Promise<SeparationModelInfo[]>;
  separate(request: StemSeparationRequest): Promise<StemSeparationResult>;
  cancel?(jobId: string): Promise<void>;
}
```

```ts
interface VoiceConversionProvider {
  id: string;
  displayName: string;
  getModels(): Promise<VoiceModelInfo[]>;
  convert(request: VoiceConversionRequest): Promise<VoiceConversionResult>;
  cancel?(jobId: string): Promise<void>;
}
```

```ts
interface InstrumentGenerationProvider {
  id: string;
  displayName: string;
  getCapabilities(): Promise<InstrumentGenerationCapabilities>;
  generate(request: InstrumentGenerationRequest): Promise<InstrumentGenerationResult>;
  cancel?(jobId: string): Promise<void>;
}
```

# 11. AI Project Workspace

Do not immediately modify the internal `.aup4` schema for all AI data.

Use a workspace around the Audacity project first:

```text
My Song/
├── project.aup4
├── ai/
│   ├── manifest.json
│   ├── jobs/
│   ├── plans/
│   ├── generations/
│   ├── separation/
│   ├── conversions/
│   └── instruments/
├── assets/
│   ├── generated/
│   ├── imported/
│   ├── separated/
│   └── converted/
└── cache/
```

Opening normal `.aup4` projects remains supported.

If AI is first used in a plain project, offer to create an AI workspace.

# 12. Provenance

Every generated asset retains:

```ts
type AssetProvenance = {
  assetId: string;
  createdAt: string;
  providerId: string;
  providerVersion?: string;
  modelId: string;
  modelRevision?: string;
  sourceAssetIds: string[];
  jobId: string;
  seed?: number;
  parameters: Record<string, unknown>;
  prompt?: string;
  lyricsId?: string;
  songPlanId?: string;
  outputChecksum: string;
};
```

The normal UI shows a concise summary; Inspector exposes detail.

# 13. Flagship Workflow

```text
Create Project
      ↓
Write / Import Lyrics
      ↓
Describe Song
      ↓
YuE2 Creates Symbolic Plan
      ↓
Inspect / Edit Melody + Chords
      ↓
Render Full Song
      ↓
Audacity Timeline
      ↓
RoFormer Separation
      ↓
RVC Vocal Identity
      ↓
Stable Audio 3 Instrument Alternatives
      ↓
Edit / Mix / Effects
      ↓
Export
```

The initial YuE2 song is the creative master, not disposable scaffolding.

# 14. YuE2 Integration

YuE2 is the primary creative engine.

Required support:

- lyrics;
- style;
- full symbolic planning;
- melody-led planning;
- direct generation;
- supplied score;
- seed;
- plan-only generation;
- exact-plan reuse where supported;
- full-song rendering;
- decoder selection;
- artifact retention;
- progress;
- truncation/failure reporting.

Map provider-specific controls into user-facing concepts:

```text
Composition Mode
[ Plan Song ]
[ Melody-led ]
[ Direct ]
[ Use Existing Plan ]
```

# 15. YuE2 Artifacts

Preserve where available:

- original score/ABC;
- plan;
- semantic tokens;
- acoustic latents;
- effective configuration;
- model identities;
- timings;
- final 48 kHz stereo audio;
- warnings/truncation status.

A generation is immutable. Regeneration creates another record.

# 16. Symbolic Song Plan

Use a normalized application model:

```ts
type SongPlan = {
  id: string;
  sourceProviderId?: string;
  tempo?: number;
  timeSignature?: string;
  key?: string;
  sections: SongSection[];
  chords: ChordEvent[];
  melody: NoteEvent[];
  sourceScoreAssetId?: string;
  sourceFormat?: "abc" | "midi" | "other";
  revision: number;
};
```

Preserve the original provider score unchanged.

# 17. Song Plan UI

Do not build a full MIDI DAW first.

Initial plan editor:

```text
Structure
Intro | Verse 1 | Pre | Chorus | Verse 2 | Chorus | Bridge | Chorus

Chords
| E       | B       | C#m      | A       |

Vocal Melody
────●──●──────●────────────●─────

Lyrics
[Verse 1]
...
```

Required edits:

- chord symbol;
- chord timing;
- note pitch;
- note timing/duration;
- transpose selection;
- section boundary/name;
- lyrics;
- plan validation;
- save as new plan revision.

Later: piano roll, notation, scale tools and multi-part editing.

# 18. Full-Length Generation Rule

The default composition workflow must render full-length tracks.

Do not replace the core workflow with independent section generation.

Whole-song rendering is required for:

- vocal continuity;
- chorus identity;
- phrasing;
- melodic consistency;
- arrangement continuity;
- emotional progression.

# 19. YuE2 Windows / GPU Feasibility Gate

Do not assume the official reference configuration fits the target laptop GPU.

Create a benchmark harness before deep integration.

Measure:

- total generation time;
- plan time;
- semantic stage;
- acoustic stage;
- decode stage;
- peak VRAM;
- peak system RAM;
- temperature;
- output duration;
- truncation;
- subjective quality.

First investigate quality-neutral techniques:

- staged component loading;
- whole-component offload;
- pinned memory;
- efficient attention;
- CUDA Graphs;
- `torch.compile`;
- modern CUDA/PyTorch kernels;
- cache reuse;
- tiled decoding if equivalent;
- precision choices that preserve quality.

Quantisation is experimental until listening tests prove acceptable quality.

Production requirement: native Windows path.

# 20. RoFormer / UVR

Use a provider-based separation model registry, not one hard-coded checkpoint.

Presets:

- Lead Vocal;
- Vocals / Instrumental;
- Drums;
- Bass;
- Other;
- Backing Vocals where supported;
- Best Quality;
- Fast Preview;
- Advanced Model.

After separation retain the original master and import aligned stems.

# 21. RVC

RVC is the confirmed vocal-identity stage.

```text
YuE2 Song
→ RoFormer Lead Vocal
→ RVC Custom Artist Model
→ Final Vocal Version
```

Default UI:

```text
Convert Voice

Source: Lead Vocal
Voice: [Artist Voice ▼]
Preset: [Studio Quality ▼]
Pitch: 0

[ Convert ]
```

Advanced settings may include F0 method, index rate, protect, filter radius, RMS mix, resample rate and output gain.

Every conversion creates a new version.

# 22. Stable Audio 3

Stable Audio 3 is the primary instrument-production stage after YuE2.

Use it for:

- replacement instruments;
- alternative performances;
- additional layers;
- arrangement refinement;
- selected-range generation where appropriate.

Example:

```text
YuE2 Song
→ RoFormer / selected source context
→ Select Guitar
→ Stable Audio 3
→ Guitar Alternative
→ Audition / Replace / Blend
```

# 23. Instrument Generation Context

```ts
type InstrumentGenerationRequest = {
  projectId: string;
  targetInstrument: string;
  prompt: string;
  referenceAssetIds: string[];
  selection?: {
    startTime: number;
    endTime: number;
  };
  musicalContext?: {
    bpm?: number;
    key?: string;
    timeSignature?: string;
    chords?: ChordEvent[];
    sectionName?: string;
  };
  providerOptions?: Record<string, unknown>;
};
```

Only pass fields the provider supports, but retain full context in project provenance.

# 24. Version Groups

Do not force Audacity's track model into full comp lanes immediately.

Create AI Version Groups:

```text
Lead Vocal
├── YuE2 Original
├── RVC Artist A
├── RVC Artist A Alt 2
└── Approved

Guitar
├── YuE2 Stem
├── Stable Audio Clean
└── Stable Audio Driven
```

First implementation may map versions to sibling Audacity tracks while enforcing one active version.

Later migrate to native lanes if Audacity gains an appropriate abstraction.

# 25. AI Studio Panel

Persistent right-side panel:

```text
Create
Plan
Separate
Vocals
Instruments
Jobs
```

Create:
- lyrics;
- song direction;
- composition mode;
- model/seed;
- Generate Plan;
- Render.

Plan:
- structure;
- chords;
- melody;
- revisions;
- validation.

Separate:
- source;
- preset;
- model;
- Separate.

Vocals:
- source;
- RVC model;
- preset;
- Convert.

Instruments:
- target;
- reference;
- range;
- prompt;
- Generate Alternative.

Jobs:
- queue;
- progress;
- elapsed time;
- logs;
- cancel/retry.

# 26. Context Actions

Right-click track/clip:

- Separate Stems;
- Extract Lead Vocal;
- Convert Voice;
- Generate Instrument Alternative;
- Use as AI Reference;
- Show AI Provenance.

Timeline selection:

- Use Range for Instrument Generation;
- Separate Selection;
- Use as AI Reference.

# 27. Lyrics

Lyrics are versioned project objects.

Old generation records always point to the exact lyric revision used to create them.

Later features may add lyric-to-score alignment and playback highlighting.

# 28. Musical Context

Essentia is not used.

For YuE2-created projects:

```text
YuE2 Symbolic Plan
→ tempo
→ metre
→ melody
→ chords
→ structure
→ timing
```

This is authoritative composition data.

For imported material, retain a neutral future analysis/transcription provider slot. No default implementation is chosen here.

# 29. Model Manager

Provide:

- install/import;
- existing-path discovery;
- checksum verification;
- licence display;
- disk usage;
- compatibility;
- uninstall;
- repair;
- defaults;
- model relocation.

Never silently download large models.

# 30. GPU Scheduling

Default policy:

```text
One heavyweight GPU AI job at a time.
```

The runtime should:

- estimate VRAM;
- serialize incompatible jobs;
- unload idle providers when necessary;
- prevent avoidable OOM;
- permit light CPU work concurrently.

Do not optimize concurrency before stability.

# 31. Job System

Persist job metadata and states:

```text
queued
preparing
loading
running
decoding
importing
complete
failed
cancelled
```

A worker crash marks a job interrupted/failed with diagnostics; it must never vanish silently.

# 32. Failure Handling

Structured errors must cover:

- missing model;
- invalid runtime;
- CUDA unavailable;
- OOM;
- licence acceptance;
- malformed lyrics;
- invalid score;
- truncation;
- worker crash;
- cancellation;
- missing/invalid output.

Show:

1. plain-language summary;
2. next action;
3. expandable technical details;
4. log path.

# 33. Crash Safety

Workers write to job-specific temporary directories.

Only after output validation:

1. checksum output;
2. register asset;
3. update AI manifest;
4. import into Audacity.

Use atomic manifest writes and retain a previous manifest backup.

# 34. Audacity Integration Boundary

Prefer current Audacity 4 modules/interfaces such as project, projectscene, trackedit, playback, record, effects, import/export, actions, workspace and QML UI.

Create a narrow adapter:

```ts
interface EditorProjectAdapter {
  projectPath(): string;
  selectedTrackRefs(): TrackRef[];
  selectedTimeRange(): TimeRange | null;
  importAudioAsset(...): Promise<TrackRef>;
  setTrackMute(...): Promise<void>;
  focusTrack(...): Promise<void>;
}
```

AI code should depend on this adapter, not scattered Audacity internals.

# 35. Commands / Actions

Register actions such as:

```text
ai.generatePlan
ai.renderSong
ai.separateStems
ai.extractVocal
ai.convertVoice
ai.generateInstrument
ai.openModelManager
ai.openJobs
```

Menus, shortcuts, context menus and QML invoke the same action layer.

# 36. Workspace

Default AI workspace:

```text
┌──────────────────────────────────────────────────────┐
│ Project / Transport / Export                         │
├──────────────┬────────────────────────┬──────────────┤
│ Tracks       │ Timeline               │ AI Studio    │
├──────────────┴────────────────────────┴──────────────┤
│ Plan / Lyrics / Versions / Job Details              │
└──────────────────────────────────────────────────────┘
```

Do not gratuitously redesign Audacity 4 during early development.

# 37. Mixing / Effects

Use Audacity's existing audio engine, effects and plugin infrastructure.

Do not build a new mixer/audio engine unless a proven product requirement cannot be met with the base editor.

# 38. Realtime Safety

No inference, model loading, heavy disk work or Python execution on realtime audio callbacks.

All runtime communication is asynchronous.

# 39. Undo / Redo

Generation itself is not an undo operation.

Importing the completed result is.

Example:

```text
RVC job completes
→ "Insert RVC Version" transaction
```

Undo removes the inserted timeline state but keeps the generated asset available. Redo reuses it without rerunning RVC.

# 40. AI History vs Editor History

Keep separate:

- Audacity undo history = editor operations;
- AI provenance/history = how generated assets were created.

# 41. Privacy / Security

- core functionality offline;
- localhost-only runtime;
- session authentication;
- sanitize paths;
- validate schemas;
- no arbitrary shell commands from project files;
- no silent uploads;
- no execution of code from model metadata.

Review inherited telemetry deliberately.

# 42. Diagnostics

Expose:

- app version;
- Audacity upstream commit;
- project commit;
- Qt/compiler/OS;
- GPU/driver;
- Python/PyTorch/CUDA per provider;
- model revisions;
- paths;
- recent jobs;
- log locations.

"Copy Diagnostics" must omit lyrics and user audio.

# 43. Testing

Native regression:
- Audacity build/tests;
- open/save;
- playback;
- recording;
- import/export;
- effects.

AI unit:
- manifests;
- schemas;
- job state;
- path handling;
- provenance;
- versions;
- cancellation;
- recovery.

Provider:
- environment health;
- model discovery;
- minimal inference;
- missing model;
- failure schema.

End to end:
- lyrics/style → YuE2 plan → full render → timeline;
- render → RoFormer → stems;
- lead vocal → RVC → new version;
- instrument → Stable Audio → new version;
- save/close/reopen;
- undo/redo without regeneration;
- missing models while existing project remains editable.

# 44. Development Gates

## Gate 0 — Fork Foundation
Windows build, launch, `.aup4` open/save, playback, recording, import/export, pinned upstream.

## Gate 1 — AI Module Shell
AI Studio loads, actions register, runtime may be absent.

## Gate 2 — Runtime Host
Authenticated IPC, worker lifecycle, progress, cancellation, recovery.

## Gate 3 — AI Workspace
Manifest, relative assets, provenance, save/reopen.

## Gate 4 — YuE2 Feasibility
Native Windows proof, full-length generation, benchmark, artifacts, quality and VRAM documented.

## Gate 5 — YuE2 Workflow
Lyrics/style → plan → full render → timeline.

## Gate 6 — Symbolic Editing
Editable chords/melody/sections, new plan revision, validated rerender.

## Gate 7 — RoFormer
Model registry, presets, aligned stems, original master retained.

## Gate 8 — RVC
Voice model manager, presets, converted vocal versions.

## Gate 9 — Stable Audio 3
Contextual instrument generation and version insertion.

## Gate 10 — Version Workflow
A/B, active version, approval, save/reopen, undo/redo.

## Gate 11 — Hardening
Installer, clean machine, offline, repair, upgrade, recovery, licence notices.

# 45. Delivery Phases

1. Establish Audacity fork and CI.
2. Build AI infrastructure/runtime.
3. Build project provenance/workspace.
4. Integrate YuE2.
5. Build symbolic plan editor.
6. Add RoFormer and RVC.
7. Add Stable Audio 3.
8. Polish versions/jobs/model management.
9. Harden Windows installer and recovery.

# 46. First Release Scope

Include:

- Audacity 4 editing foundation;
- Windows 11 x64;
- AI project workspace;
- lyrics;
- YuE2 plan + full song;
- basic symbolic editor;
- RoFormer;
- RVC;
- Stable Audio 3;
- version groups;
- model manager;
- persisted jobs;
- provenance;
- export;
- diagnostics/recovery.

Do not require:

- full MIDI DAW parity;
- score engraving;
- cloud account;
- collaboration;
- mobile;
- marketplace;
- remote GPU;
- complex comping.

# 47. Explicitly Excluded from This New Baseline

- Essentia;
- ACE-Step;
- ACE-Step LoRAs;
- DiffSinger;
- Vevo2;
- VocalRender;
- Strudel;
- Pattern Lab;
- ComfyUI as an end-user runtime dependency.

ComfyUI may remain an R&D test tool only.

# 48. Deferred Research

## YuE2 LoRA/adapter-like adaptation

Keep future support neutral:

```ts
type ModelAdaptation = {
  id: string;
  providerId: string;
  baseModelId: string;
  type: "lora" | "adapter" | "embedding" | "finetune";
  purpose: "artist_performance" | "genre" | "vocal_style" | "production_style";
  path: string;
  strength?: number;
};
```

Potential future path:

```text
YuE2 + Artist Performance Adapter
→ artist-like performance tendencies
→ RoFormer
→ RVC
→ artist voice identity
```

Do not build this before base YuE2 integration is stable.

## Imported-audio transcription

Potential future providers include SheetSage2 and MuScriptor, behind a neutral contract.

## Agentic editing

Future agents should modify structured song-plan objects rather than directly edit waveforms whenever practical.

# 49. Quality Policy

Optimization order:

1. runtime compatibility;
2. kernel/runtime efficiency;
3. caching;
4. model residency/staging;
5. memory transfer;
6. compile/graph optimization;
7. only then quality-affecting compromises.

Do not lower song duration, synthesis quality, decoder quality or model precision solely to improve a benchmark.

# 50. Model Version Policy

Never silently replace model revisions used by old projects.

Generation records retain exact provider/model revision and effective settings.

If an old revision is missing, the project still opens and existing audio remains usable.

# 51. Portability

Portable project export may include:

- audio;
- lyrics;
- symbolic plans;
- stems;
- converted vocals;
- generated instruments;
- AI manifests;
- provider settings;
- model identifiers/checksums.

Do not copy model weights by default.

# 52. Graceful Degradation

Without YuE2:
- existing audio works;
- plan remains visible;
- new generation disabled.

Without RVC:
- converted audio works;
- new conversion disabled.

Without Stable Audio:
- existing generated instruments work;
- new generation disabled.

This is mandatory.

# 53. Initial Codex Audit

Before AI implementation, Codex must produce:

`AUDACITY4_INTEGRATION_AUDIT.md`

It must document:

- pinned Audacity commit;
- module graph;
- project interfaces;
- track/clip interfaces;
- import APIs;
- save/open lifecycle;
- action registration;
- QML/workspace registration;
- background-task patterns;
- extension system;
- settings/logging;
- undo/redo APIs;
- project/file APIs;
- safe locations for new modules;
- remaining AU3 dependencies;
- upstream-churn risk.

It must recommend exact source locations for the AI modules.

No major AI integration begins before this audit.

# 54. First Vertical Slice

Use a tiny test provider, not a production model:

```text
Audacity 4
→ AI Studio
→ Create Job
→ Runtime Host
→ test provider creates local WAV + manifest
→ app imports track
→ provenance saved
→ save/close/reopen
```

Acceptance:

- UI remains responsive;
- cancel works;
- worker crash recovers;
- undo removes insertion;
- redo does not regenerate;
- moving the project folder does not break project-relative assets.

# 55. Second Vertical Slice

YuE2 plan only:

```text
Lyrics + Style
→ YuE2 plan()
→ score/plan artifacts
→ Song Plan UI
```

This validates the source-level composition model before full audio generation.

# 56. Third Vertical Slice

YuE2 full render:

```text
Approved Plan
→ semantic stage
→ acoustic stage
→ decode
→ full song
→ Audacity timeline
```

Only after this is stable add RoFormer, RVC and Stable Audio.

# 57. Definition of Success

A user can:

1. launch a native Windows editor;
2. write lyrics and describe a song;
3. generate an editable musical plan;
4. render a coherent full-length song;
5. edit the result with familiar multitrack tools;
6. separate useful stems;
7. convert the vocal into a custom artist voice;
8. replace/augment instruments with Stable Audio 3;
9. compare alternatives without destroying earlier work;
10. mix/process using Audacity's editor/effects;
11. close/reopen with AI relationships intact;
12. export finished audio completely locally.

# 58. Final Baseline

```text
EDITOR FOUNDATION
Audacity 4
→ editing
→ recording
→ transport
→ effects/plugins
→ project handling
→ export

CREATIVE FOUNDATION
YuE2
→ lyrics/style
→ symbolic planning
→ melody/harmony/form
→ full-song generation

SOURCE DECOMPOSITION
UVR / RoFormer
→ vocals and stems

VOCAL IDENTITY
RVC
→ custom artist voice conversion

INSTRUMENT PRODUCTION
Stable Audio 3
→ replacement stems
→ alternatives
→ refinement

AI MUSIC STUDIO LAYER
→ local provider orchestration
→ song-plan editor
→ job system
→ model manager
→ provenance
→ version groups
→ recovery
→ production UX
```

This document is the baseline for the new project. Future decisions should be evaluated against it rather than inheriting assumptions from any previous prototype.
