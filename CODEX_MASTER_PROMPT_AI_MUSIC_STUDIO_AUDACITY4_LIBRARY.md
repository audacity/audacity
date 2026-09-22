# Codex Master Prompt — AI Music Studio on Audacity 4
## New Project Baseline — Asset Library, YuE2, Artist LoRA, RVC, RoFormer and Stable Audio 3

You are acting as the lead software architect, senior C++/Qt engineer, AI-audio engineer, and implementation planner for a brand-new desktop application.

The project is called **AI Music Studio**.

This is a new project. Do not inherit implementation assumptions from previous prototypes unless they are explicitly restated here.

The application will be built as a controlled fork of **Audacity 4**, using Audacity as the native audio-editor foundation while adding a first-class AI production layer.

The most important architectural principle is:

> **The Library owns the creative assets. The Timeline owns the current arrangement.**

Every generated song, uploaded file, separated stem, converted vocal, generated instrument, transcription, symbolic plan, and rendered mix should remain accessible through the Library even if it is not currently present on the timeline.

The application should feel like a coherent production environment, not a collection of AI tools attached to an editor.

# 1. Core Product Direction

Use Audacity 4 for:

- multitrack audio editing;
- waveform display;
- clip editing;
- transport;
- recording;
- audio I/O;
- effects/plugin hosting;
- project loading/saving;
- undo/redo;
- import/export;
- native Qt/QML desktop UI;
- shortcuts/accessibility;
- conventional editor behaviour.

Build AI Music Studio for:

- shared project/global asset library;
- AI generation;
- AI asset lineage/provenance;
- YuE2 composition;
- YuE2 symbolic plans;
- YuE2 Artist LoRAs;
- YuE2 Style / instrumental LoRAs where supported;
- RoFormer/UVR source separation;
- RVC voice conversion;
- Stable Audio 3 instrument replacement/generation;
- transcription providers such as MuScriptor / SheetSage2;
- AI job queue;
- GPU/resource orchestration;
- model management;
- alternative versions;
- asset comparison;
- contextual AI actions;
- recovery and diagnostics.

Do not rebuild a browser DAW.

Do not introduce a second audio engine if Audacity already provides the required functionality.

# 2. Reference Design Influence — Remiqora

Study:

https://github.com/inikolax/remiqora

Do not copy its architecture blindly.

Use it as a workflow and UX reference for the following concepts:

- one shared media library used by generation, separation, transcription, mixer/editor workflows;
- generated tracks treated as raw production material rather than terminal outputs;
- automatic model/process orchestration;
- mutual exclusion / GPU locking for heavyweight models;
- source → separation → transcription → editor workflow;
- one-click transitions from processing results into the editor;
- progress/cancellation;
- contextual operations on generated or uploaded assets;
- quick preview of separated stems before committing them to the arrangement;
- unified model/tool access.

Do NOT import the following Remiqora decisions into this project merely because they exist there:

- ACE-Step as a production model;
- a custom Vue/Web Audio DAW;
- a separate browser mixer as the main mixing environment;
- browser-side audio rendering as the project foundation.

Audacity 4 replaces that entire editor layer.

# 3. Core Mental Model

The application has two distinct but connected layers:

```text
LIBRARY
= everything the user/project has created or imported

TIMELINE
= the material currently used in the arrangement
```

A Library asset must survive removal from the timeline.

A timeline clip references a Library asset.

Undoing “Add to Timeline” removes the clip, not the Library asset.

Deleting from the Library is a separate explicit operation.

Never conflate:

- remove from timeline;
- hide/deactivate;
- remove from project library;
- permanently delete file from disk.

These must be distinct actions with clear UI language.

# 4. Library Scope

Provide two primary scopes:

```text
Library
├── This Project
└── All Projects
```

Optional filters:

```text
All
Generations
Uploads
Stems
Vocals
Instruments
MIDI / Transcription
Song Plans
Mixdowns
Favourites
```

The project-local view is the default inside an open project.

The global view acts as the creative archive across projects.

Global catalogue metadata may be stored in an application database.

Project-specific provenance must remain portable with the project.

Do not make a project depend on an opaque global database to remain intelligible.

# 5. Asset Model

Create a first-class asset domain model.

```ts
type AssetKind =
  | "audio"
  | "generation"
  | "upload"
  | "recording"
  | "stem"
  | "vocal"
  | "instrument"
  | "midi"
  | "song_plan"
  | "mixdown"
  | "reference";

type AssetOrigin =
  | "uploaded"
  | "recorded"
  | "yue2"
  | "roformer"
  | "rvc"
  | "stable_audio"
  | "transcription"
  | "rendered"
  | "other";

type ProjectAsset = {
  id: string;
  projectId?: string;
  name: string;
  kind: AssetKind;
  origin: AssetOrigin;
  filePath?: string;
  sourceAssetIds: string[];
  durationSeconds?: number;
  sampleRate?: number;
  channels?: number;
  createdAt: string;
  updatedAt?: string;
  provenanceId?: string;
  tags: string[];
  favourite: boolean;
  status:
    | "available"
    | "missing"
    | "offline"
    | "failed"
    | "archived";
};
```

Do not require every asset type to have a physical audio file.

Song plans, MIDI, manifests, and future adaptation metadata may be non-audio assets.

# 6. Asset Lineage / Provenance Graph

Every derived asset must know where it came from.

Example:

```text
YuE2 Master 01
├── Song Plan Rev 03
├── Lead Vocal
│   ├── RVC Artist v1
│   └── RVC Artist v2
├── Drums
├── Bass
└── Guitar
    ├── Stable Audio v1
    └── Stable Audio v2
```

The project should be able to answer:

- What created this asset?
- Which model?
- Which model revision?
- Which LoRA/adaptation?
- Which source assets?
- Which prompt?
- Which lyrics revision?
- Which symbolic plan?
- Which selected timeline range?
- Which settings?
- Which seed?
- Which runtime/provider version?
- Which output checksum?

```ts
type AssetProvenance = {
  id: string;
  assetId: string;
  operation:
    | "song_generation"
    | "separation"
    | "voice_conversion"
    | "instrument_generation"
    | "transcription"
    | "render";
  providerId: string;
  providerVersion?: string;
  modelId: string;
  modelRevision?: string;
  adaptationIds?: string[];
  sourceAssetIds: string[];
  songPlanId?: string;
  lyricsRevisionId?: string;
  prompt?: string;
  seed?: number;
  settings: Record<string, unknown>;
  jobId: string;
  createdAt: string;
  outputChecksum?: string;
};
```

# 7. Library UI

Implement the Library as a dockable native panel beside the Audacity timeline.

Recommended default layout:

```text
┌───────────────────────────────────────────────────────────────┐
│ Project / Transport / Tempo / Export                         │
├──────────────┬───────────────────────────────┬────────────────┤
│ Tracks       │ Timeline                      │ AI Studio      │
│              │                               │                │
├──────────────┴───────────────────────────────┴────────────────┤
│ Library / Lyrics / Plan / Versions / Job Details             │
└───────────────────────────────────────────────────────────────┘
```

The Library must support search, filters, sorting, waveform/thumbnail preview where practical, duration, source/origin, favourite, missing/offline state, drag to timeline, multi-select and contextual actions.

# 8. Contextual Asset Actions

Actions depend on asset capability.

Full mix:
```text
Audition
Add to Timeline
Separate Stems
Transcribe
Use as Reference
Create Cover Source
Show Provenance
Rename
Favourite
Reveal in Folder
```

Vocal stem:
```text
Audition
Add to Timeline
Convert Voice
Transcribe Vocal Melody
Use as Reference
Show Provenance
```

Instrument stem:
```text
Audition
Add to Timeline
Generate Alternative
Transcribe to MIDI
Use as Reference
Show Provenance
```

Song plan:
```text
Open Plan
Duplicate Revision
Edit
Render
Export ABC
Show Provenance
```

Do not expose invalid actions. Create a capability service rather than hard-coding menu options in QML.

# 9. Asset → Timeline Behaviour

Dragging an asset onto the timeline creates a clip/track reference to that asset.

The source Library asset remains immutable unless explicitly replaced at the Library level.

Timeline edits such as trim, split, move, fade, gain and effects must not mutate the original Library media.

# 10. Generated Variants

AI frequently creates multiple candidate outputs.

Treat variants as normal Library assets grouped by relationship.

```ts
type AssetGroup = {
  id: string;
  projectId: string;
  label: string;
  purpose:
    | "generation_variants"
    | "track_versions"
    | "stem_set"
    | "conversion_variants"
    | "other";
  assetIds: string[];
  preferredAssetId?: string;
};
```

Do not discard rejected alternatives automatically.

# 11. Version Groups

Support named production versions:

```text
Lead Vocal
├── YuE2 Original
├── RVC Artist A
├── RVC Artist A Alt 2
└── Approved

Guitar
├── YuE2 Stem
├── Stable Audio Clean
├── Stable Audio Driven
└── Approved
```

Provide switch active version, A/B, favourite, approve, rename, add another version and show provenance.

# 12. Core AI Architecture

Use an external local runtime host.

```text
Audacity 4 / AI Music Studio
        │
        │ local authenticated IPC
        ▼
AI Runtime Host
        │
        ├── YuE2
        ├── RoFormer / UVR
        ├── RVC
        ├── Stable Audio 3
        ├── MuScriptor
        └── SheetSage2
```

Do not load Python AI stacks inside the Audacity process.

# 13. GPU / Process Orchestrator

Borrow the strongest orchestration idea from Remiqora.

The user should choose operations, not manage model processes manually.

```text
GPU State

YuE2              Loaded
RoFormer           Unloaded
RVC                Unloaded
Stable Audio 3     Unloaded

Queue:
1. Separate stems
2. Convert vocal
3. Generate guitar
```

The runtime host should know active provider, estimated VRAM needs, stop/unload incompatible providers, serialize heavyweight GPU jobs, preserve light CPU tasks where safe, manage a GPU lock, recover after worker failure and expose status in Diagnostics.

Default:

```text
One heavyweight GPU job at a time
```

until measured evidence supports safe concurrency.

# 14. Job System

All AI processing uses persisted jobs.

States:

```text
queued
preparing
loading
running
decoding
postprocessing
importing
complete
failed
cancelled
interrupted
```

Support cancellation, retry, detailed logs, crash recovery and restart-safe history where practical.

# 15. AI Studio Workflow

Emphasize continuity:

```text
Generate
   ↓
Audition
   ↓
Separate
   ↓
Convert / Replace
   ↓
Edit
   ↓
Mix
   ↓
Export
```

Every completed operation should offer sensible next actions.

# 16. Stem Preview

Before adding separated stems to the timeline, provide a lightweight preview:

```text
Stem Preview

Vocals   [Mute] [Solo]
Drums    [Mute] [Solo]
Bass     [Mute] [Solo]
Other    [Mute] [Solo]

[ Add Selected to Timeline ]
```

Do not build a second full mixer. Audacity remains the mixing environment.

# 17. YuE2 — Primary Composition Engine

YuE2 is the primary song-composition and full-song-generation provider.

Support lyrics, style, full/melody/off planning modes, supplied ABC, plan-only, full render, seed, model revision, decoder revision, artifact preservation, progress and truncation reporting.

Preserve audio, ABC, plan, semantic tokens, acoustic latent artifacts where available, effective configuration, timings and model identities.

The YuE2 output is a creative master, not disposable scaffolding.

# 18. Symbolic Song Plan

Song plans are first-class Library/project assets.

Use a normalized project representation independent of raw YuE2 ABC.

Editing a plan creates a new revision. Never overwrite prior revisions silently.

# 19. YuE2 Artist LoRA

Treat Artist LoRA as an important planned feature, but keep it behind capability checks until testing proves it practical.

Artist LoRA role:

```text
YuE2 Base
+ Artist AR Adapter
+ required acoustic/NAR companion
→ artist-specific musical/performance tendencies
```

RVC remains separate:

```text
RoFormer vocal
→ RVC
→ voice identity/timbre
```

Do not conflate Artist LoRA with voice cloning.

# 20. Other YuE2 Adaptations

The adaptation architecture must support:

```text
None
Artist
Style
Instrumental
Future Specialized Adapter
```

And acoustic adaptation:

```text
Stock
RealAudio NAR
Artist Companion
```

Do not assume arbitrary adapter stacking is valid. Represent compatibility rules explicitly.

# 21. Instrumental YuE2 LoRA

Support testing of the Mothersuperior instrumental CoT/full LoRA as a research mode.

Treat Instrumental mode as a separate adapter configuration unless tests prove otherwise.

# 22. RoFormer / UVR

RoFormer is the production source-separation layer.

Use a provider-neutral model registry with Mel-Band RoFormer, BS-RoFormer, MDX, Demucs and future-compatible models.

Do not hard-code Demucs merely because Remiqora uses it.

# 23. Separation Result Model

A separation operation creates a stem-set group.

Each stem is a normal Library asset with provenance.

The original master always remains available.

# 24. RVC

RVC is the primary voice-conversion provider.

Every conversion becomes a new Library asset/version with complete settings/provenance.

# 25. Stable Audio 3

Stable Audio 3 is the primary instrument-generation/refinement provider.

Use project context where supported: BPM, key, metre, chords, section, selected range, original mix, original stem and prompt.

# 26. Transcription

Use a neutral transcription provider boundary.

Potential implementations:

- MuScriptor;
- SheetSage2;
- future Windows-compatible providers.

Allow transcription of full mixes or any compatible stem.

Transcription output becomes a Library asset.

# 27. Essentia

Do not use Essentia.

It is excluded from this Windows-first architecture.

# 28. Model Manager

Provide categories for Composition, Separation, Voice, Instrument Generation, Transcription and Adaptations/LoRAs.

Capabilities: install/import, locate existing, inspect licence, checksum, model revision, disk usage, compatibility, uninstall, repair, move and defaults.

Never silently download multi-gigabyte models.

# 29. Library Storage Strategy

Use two layers.

Application catalogue:
- SQLite may index projects, global assets, favourites, search metadata, recent assets, project membership and model registry.

Portable project manifest:
- every project retains its own AI metadata/provenance.

Suggested layout:

```text
My Song/
├── project.aup4
├── ai/
│   ├── manifest.json
│   ├── assets.json
│   ├── provenance/
│   ├── jobs/
│   ├── plans/
│   └── versions/
├── assets/
│   ├── generated/
│   ├── imported/
│   ├── separated/
│   ├── converted/
│   ├── instruments/
│   └── transcription/
└── cache/
```

# 30. Import Policy

Support clearly defined policies:

```text
Copy into project
Reference original file
Copy into global library
```

Default should favour portability and safety.

Never silently move/delete source user files.

# 31. Library Deduplication

Use checksums to detect duplicate media.

Do not automatically merge logical assets just because file content matches.

# 32. Delete Semantics

Provide explicit operations:

```text
Remove from Timeline
Remove from Project Library
Delete File from Disk
```

The destructive option requires confirmation.

Warn if an asset has dependants.

# 33. Search and Metadata

Search by asset name, tags, origin, model, provider, project, prompt text where appropriate, date, duration, adaptation/LoRA and approved/favourite state.

# 34. Audition

Every audio Library asset should be auditionable without timeline insertion.

Support play/pause, seek, waveform preview where feasible and loop.

# 35. “Open/Add to Editor” Flow

After any completed process, expose direct next actions.

Examples:

RVC:
```text
Conversion Complete
[ Audition ]
[ Add to Timeline ]
[ Add as Vocal Version ]
```

Stable Audio:
```text
Generation Complete
[ Audition ]
[ Add as Alternative ]
[ Add as New Track ]
```

Separation:
```text
Separation Complete
[ Preview Stems ]
[ Add All ]
[ Add Selected ]
```

# 36. AI Workspace

Recommended AI Studio panel:

```text
Create
Plan
Separate
Vocals
Instruments
Transcribe
Jobs
```

Keep concepts distinct:

```text
AI Studio = operations
Library = assets
Timeline = arrangement
```

# 37. Undo / Redo

AI inference itself is not an Audacity undo operation.

Result insertion is.

Undo removes timeline/version placement but must not delete the Library asset or rerun the AI model.

# 38. Crash Safety

Workers write to job-specific temporary directories.

Only after validation:
1. verify file;
2. checksum;
3. create Library asset;
4. write provenance;
5. atomically update project manifest;
6. then offer/import to timeline.

# 39. Windows-First Requirement

Primary platform:

```text
Windows 11 x64
```

Core operation must not require WSL.

Design for consumer NVIDIA GPUs around the RTX 5070 Ti Laptop class, not only 24–32 GB workstation cards.

# 40. Quality Policy

Quality takes priority over superficial speed.

Optimization order:

```text
1. correct runtime
2. efficient kernels
3. caching
4. component staging
5. memory transfers
6. graph/compile optimization
7. only then quality-affecting compromises
```

# 41. Graceful Degradation

Projects remain usable without installed AI models.

Existing generated assets always remain playable/editable.

# 42. Audacity Integration Strategy

Before implementing product features, inspect the exact pinned Audacity 4 source.

Prefer current project, projectscene, trackedit, actions, playback, import/export, effects, workspace and QML/Muse abstractions.

Avoid direct AU3 internals unless required.

# 43. Mandatory Initial Audit

Before substantial code changes, create:

```text
AUDACITY4_AI_MUSIC_STUDIO_INTEGRATION_AUDIT.md
```

Document the pinned Audacity commit, build process, module graph, track/clip APIs, load/save, actions, QML docking, import, undo, background-task patterns, settings/logging, project paths, safe custom-module locations, remaining AU3 dependencies and upstream-churn risk.

Recommend exact source locations for:

```text
aicore
aiproject
ailibrary
aijobs
aimodels
aistudio
songplan
stemtools
vocaltools
instrumenttools
transcription
```

# 44. First Vertical Slice — Library First

Do NOT begin with YuE2.

Build:

```text
Audacity 4
→ AI Library panel
→ Import local WAV
→ create Library asset
→ drag asset onto timeline
→ remove from timeline
→ asset remains in Library
→ save
→ close
→ reopen
→ asset + timeline relationship restored
```

# 45. Second Vertical Slice — Runtime + Generated Asset

Use a tiny test provider:

```text
AI Studio
→ submit job
→ local Runtime Host
→ generate a small WAV
→ create provenance
→ add result to Library
→ audition
→ drag onto timeline
```

# 46. Third Vertical Slice — YuE2

```text
Lyrics + Style
→ YuE2 Plan
→ Song Plan Library Asset
→ Full YuE2 Render
→ Generation Library Asset
→ Audition
→ Timeline
```

# 47. Fourth Vertical Slice — Separation

```text
YuE2 Master
→ RoFormer
→ Stem Set
→ Library
→ Stem Preview
→ Add Selected to Timeline
```

# 48. Fifth Vertical Slice — RVC

```text
Lead Vocal Library Asset
→ RVC
→ New Vocal Asset
→ Vocal Version Group
→ Audition / A-B / Add
```

# 49. Sixth Vertical Slice — Stable Audio 3

```text
Instrument Stem
+ project musical context
+ range
+ prompt
→ Stable Audio 3
→ Instrument Alternative
→ Library
→ Version Group
```

# 50. Seventh Vertical Slice — Transcription

```text
Any Compatible Audio Asset
→ transcription provider
→ MIDI / ABC / melody asset
→ Library
→ optional Song Plan integration
```

# 51. Remiqora Audit Task

Create:

```text
REMIQORA_REFERENCE_AUDIT.md
```

Study Remiqora specifically for shared asset/storage architecture, SQLite schema, model orchestrator, GPU locking, job cancellation, track library, stem workflow, “open in editor” transitions, transcription routing, file handling, process lifecycle and model health checks.

For each feature mark:

```text
ADOPT CONCEPT
ADAPT
REJECT
RESEARCH
```

Explain why.

# 52. Explicitly Excluded

Do not reintroduce:

- ACE-Step as a production model;
- Essentia;
- DiffSinger;
- VocalRender;
- Vevo2;
- Strudel;
- Pattern Lab;
- ComfyUI as an end-user runtime;
- Remiqora's browser DAW as the editor foundation.

ComfyUI and Yue2 Studio remain useful R&D/test environments only.

# 53. Definition of Success

The product succeeds when a user can:

1. create or open a project;
2. see every uploaded/generated asset in one Library;
3. generate a full song with YuE2;
4. preserve its symbolic plan;
5. audition the generation without inserting it;
6. drag it to the Audacity timeline;
7. separate it into stems;
8. preview stems;
9. add selected stems to the project;
10. convert the vocal using RVC;
11. create instrument alternatives using Stable Audio 3;
12. transcribe any suitable mix/stem;
13. compare alternative assets;
14. remove timeline clips without losing source assets;
15. close/reopen with all Library relationships intact;
16. search older generations and uploads;
17. move between projects while retaining a global creative archive;
18. work offline after model installation;
19. recover from failed AI jobs without project corruption;
20. export the final production using Audacity's normal editor/export pipeline.

The intended experience is:

> **Generate freely, keep everything, organise intelligently, and decide later what belongs in the final arrangement.**

That is the central product principle.
