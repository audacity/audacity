# Audacity 4 AI Music Studio Integration Audit

**Audit date:** 2026-09-20  
**Repository:** `D:\AI Music Studio v2`  
**Pinned source revision:** `4c177d436e48c1d20f231eada44035593cb26292` (`Audacity-4.0.0`)  
**Scope:** source-level architecture audit for the Library-first baseline. This is not a claim of live GUI, audio-device, or provider validation.

## Decision

Audacity 4 remains the editor foundation. AI code must live in isolated AU4 modules and use an external, authenticated local runtime host. The Library is project data with an optional application catalogue; it is not a timeline feature and providers may not include AU3 types.

The existing `aimusic` module is an R&D BPM/key-analysis proof of concept. It must not be extended as the Library, job, provenance, or provider architecture.

## Verified source seams

| Product need | Extension seam | Source location | Rule |
|---|---|---|---|
| Active project | `IGlobalContext` | `src/context/iglobalcontext.h` | Project-scoped AI services attach/detach here. |
| Project lifecycle/path/save | `IAudacityProject` | `src/project/iaudacityproject.h` | Use a project-adjacent `ai/` workspace; handle unsaved projects explicitly. |
| Import to editor | `IAudacityProject` and `IImporter` | `src/project/iaudacityproject.h`, `src/importexport/import/iimporter.h` | Validate a completed asset before invoking editor import. |
| Track/clip/selection operations | `ITrackeditProject` and interaction interfaces | `src/trackedit/itrackeditproject.h`, `src/trackedit/` | Keep behind a narrow editor adapter. |
| Undo/redo | `IProjectHistory` | `src/trackedit/iprojecthistory.h` | Record insertion only; never inference or Library persistence. |
| QML dock UI | `DockWindow`, `DockPage`, `DockPanel` | `src/appshell/qml/Audacity/AppShell/WindowContent.qml`, `src/appshell/qml/Audacity/AppShell/ProjectPage/ProjectPage.qml` | Put AI Studio in its own dock panel. |
| Actions | module/action registration pattern | `src/aimusic/`, `src/aistudio/` | Actions enqueue operations and never run providers synchronously. |
| Async and logging | Muse async primitives; `LOG*` | `muse/framework/`, existing module implementations | Marshal UI state asynchronously; redact prompts, lyrics, and paths in diagnostics. |

## Current module graph

```text
app / appshell / projectscene
  -> project, context, trackedit, importexport, playback, record, effects
  -> au3wrap (current AU3 implementation adapter)
  -> aicore, aiproject, aijobs, aistudio (existing AI foundation)
```

`src/CMakeLists.txt` currently registers `aimusic`, `aicore`, `aijobs`, `aiproject`, and `aistudio`. `aijobs/runtimehost` builds `ai_runtime_host`, while `aistudio` owns the UI-facing supervisor wiring. This is the correct basic boundary, but it does not yet provide the required first-class Library domain.

## Required module layout and dependency direction

```text
aistudio   -> ailibrary, aijobs, aiproject, aimodels, songplan
ailibrary  -> aicore, aiproject
aijobs     -> aicore
aiproject  -> aicore
providers/runtime host -> versioned IPC contract only
editor adapter -> project, context, trackedit, importexport only
```

Recommended source locations:

| Module | Location | Purpose |
|---|---|---|
| Shared contracts | `src/aicore/` | asset IDs, kinds, provenance, capability contracts, validation. |
| Project persistence | `src/aiproject/` | portable manifest, atomic writes, relative paths, recovery. |
| Library | `src/ailibrary/` | asset catalogue, search/filter model, asset groups, capability service, project/global indexing boundary. |
| Jobs/runtime client | `src/aijobs/` | persisted state machine, cancellation, retry, runtime protocol. |
| Model manager | `src/aimodels/` | discovery, licence, checksum, compatibility; no silent download. |
| AI dock/actions | `src/aistudio/` | operation controls, job diagnostics, Library dock integration. |
| Song plans | `src/songplan/` | normalized plans and immutable revisions. |
| Future provider adapters | `src/stemtools/`, `src/vocaltools/`, `src/instrumenttools/`, `src/transcription/` | provider-specific workflows after their vertical slices. |

## Persistence, undo, and crash contract

For a saved project, use `<project-dir>/ai/` with a portable manifest and project-relative asset paths. For an unsaved project, keep job output in a runtime temporary area until the user chooses a project location. Write manifests atomically: sibling temporary file, close/flush, replace, and retain the previous valid copy.

1. A runtime worker writes only to a job-specific temporary directory.
2. The host validates output and returns a result manifest/checksum.
3. `aiproject` creates a Library asset and provenance record, then atomically persists it.
4. Only an explicit user insertion invokes the editor adapter and creates an Audacity history entry.
5. Undo removes the timeline placement; it never deletes or regenerates the Library asset.

## AU3 and upstream risks

AU4 public interfaces still bridge an AU3-backed project implementation, so direct AU3 calls are a high churn risk. Timeline QML is also actively evolving. Isolate AU3 access in one tested editor adapter, retain Library and provider code outside `au3/`, and avoid modifying core timeline rendering for the initial Library slice.

The checkout reports only the `upstream` remote. Establish an owned fork remote before creating feature branches or syncing upstream changes.

## Next implementation gate: Library first

Before YuE2 work, introduce `ailibrary` and prove the following with a small local WAV:

```text
AI Library panel -> import/copy WAV -> ProjectAsset + manifest -> add to Timeline
-> remove from Timeline -> Library asset remains -> save/close/reopen -> both relationships restore
```

Acceptance requires an observed native session for drag/add, removal, save/reopen, and undo behavior. Source inspection and a successful build are not substitutes for that validation.
