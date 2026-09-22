# Audacity 4 Integration Audit

**Audit date:** 2026-09-12
**Repository:** `D:\AI Music Studio v2`
**Audited revision:** `4c177d436e48c1d20f231eada44035593cb26292`
**Pinned tags at HEAD:** `Audacity-4.0.0`, `baseline-audacity-4.0.0-verified`
**Upstream remote:** `https://github.com/audacity/audacity.git`

This is a source-level integration audit. It identifies supported AU4 extension seams for the master plan; it does not claim a live GUI or hardware test.

## Executive decision

Audacity 4 is a viable editor foundation for the proposed local-first AI Music Studio. The safest implementation is a set of isolated native modules that depend on stable AU4 interfaces, plus an out-of-process local runtime host. Heavy model environments must never be loaded into the editor process.

The existing `src/aimusic` module is a useful analysis proof of concept only. It launches a helper directly and reaches into AU3 track types for timing and label changes. It must not grow into the production runtime, job system, AI workspace, provenance, or provider boundary. Preserve it as R&D until a deliberate removal/archive decision is made.

**Gate 0 result:** build/install baseline is present and pinned.
**Gate 1 result:** completed on 2026-09-12. The `aicore`, `aijobs`, `aiproject`, and `aistudio` shells build and install; AI Studio opens a dock that explicitly reports an unconfigured runtime.
**Required next gate:** Gate 2, a local runtime host/client vertical slice with deterministic job behaviour.
**Do not start YuE2 integration** until the runtime, job, and project-workspace boundaries below are established.

## Current module graph

```text
app
 ├─ app shell / dock window / menus
 ├─ project
 │   └─ IAudacityProject: create, load, import, save, close, path
 ├─ context
 │   └─ IGlobalContext: active project and active track-edit project
 ├─ trackedit
 │   └─ tracks, clips, labels, selection, undo/history interfaces
 ├─ projectscene
 │   └─ QML timeline, track view models, toolbar and panel models
 ├─ importexport
 │   └─ IImporter and AU3-backed audio import
 ├─ playback / record / effects
 ├─ au3wrap
 │   └─ adapter layer over remaining AU3 project implementation
 └─ aimusic (current R&D proof of concept)
     └─ direct helper invocation and AU3 label/timing integration
```

New AI code should sit beside, rather than inside, `au3/`. The only justified AU3 adapter code belongs in a narrow editor adapter implementation.

## Confirmed integration surfaces

| Need | Supported seam | Source location | Recommendation |
|---|---|---|---|
| Active project | `IGlobalContext::currentProject()` | `src/context/iglobalcontext.h` | Use as the sole entry point for project-scoped AI services. Subscribe to its change notification to detach jobs/views on project switches. |
| Project path and save state | `IAudacityProject::path`, `needSave`, `save`, `aboutCloseBegin/End` | `src/project/iaudacityproject.h` | Create the AI workspace beside a saved project. For an unsaved project, hold artifacts in a per-job temporary directory and offer workspace creation before persistence. |
| Audio import | `IAudacityProject::import` / `importIntoTrack` | `src/project/iaudacityproject.h` | Use this public project-facing interface to insert validated job output. Do not construct AU3 wave tracks in provider code. |
| Import implementation | `IImporter` / `Au3Importer` | `src/importexport/import/iimporter.h`, `src/importexport/import/internal/au3/au3importer.h` | Reuse through the project interface first. Extend the importer only if a job requires an unsupported explicit placement/transaction. |
| Tracks, clips, labels | `ITrackeditProject`, `ITracksInteraction`, `IClipsInteraction`, `ILabelsInteraction` | `src/trackedit/` | Use for editor operations after an AI result is accepted. Keep provider code independent of these types. |
| Undo / redo | `IProjectHistory` | `src/trackedit/iprojecthistory.h` | Commit only the timeline insertion or editor change. Job execution and generated files are retained outside undo history. |
| Time signature | `ITrackeditProject::timeSignature` / `setTimeSignature` | `src/trackedit/itrackeditproject.h` | YuE2 plan data is authoritative. Apply timing only as a user-approved editor operation. |
| UI actions | `IActionsDispatcher` plus `IUiActionsRegister` | current example: `src/aimusic/aimusicmodule.cpp` | Register each AI command once; menus, shortcuts, context menus, and QML dispatch that same action. |
| QML models | module `registerUiTypes()` | `src/projectscene/projectscenemodule.cpp` | Register AI-specific QObjects in `aistudio`, not in `projectscene`, unless the feature modifies a generic editor surface. |
| Docked UI | `DockWindow`, `DockPage`, `DockPanel`, interactive URI registration | `src/appshell/qml/Audacity/AppShell/WindowContent.qml`, `src/projectscene/projectscenemodule.cpp` | Add an AI Studio dock page/panel through an isolated module and URI. Do not hard-code it into the main timeline. |
| Async UI delivery | `muse::async::Asyncable`, `Async::call`, channels and notifications | used throughout `src/appshell` and `src/projectscene` | Runtime I/O lives off the UI thread; return progress through an AI job model, then marshal UI state updates through framework async primitives. |
| Logs | framework logger (`LOGD`, `LOGI`, `LOGW`, `LOGE`) | initialized by `src/au3wrap/au3wrapmodule.cpp` | Emit structured, redacted job diagnostics. Never log audio, lyrics, session tokens, or raw prompts by default. |

## Safe module layout

Create the following isolated modules under `src/`, each with its own CMake target, module class, tests, public interfaces, and QML only where needed.

```text
src/
├─ aicore/        # shared types, schemas, provider contracts, result validation
├─ aiproject/     # workspace layout, manifest, provenance, atomic persistence
├─ aijobs/        # job state machine, runtime-client abstraction, cancellation/recovery
├─ aimodels/      # model registry, local path discovery, checksums, licences
├─ aistudio/      # dock panel, commands, UI models, job/diagnostics presentation
├─ songplan/      # normalized plan, revisioning, validation, basic plan UI
├─ stemtools/     # future RoFormer provider adapter and insertion workflow
├─ vocaltools/    # future RVC provider adapter and version workflow
└─ instrumenttools/ # future Stable Audio provider adapter and version workflow

runtime/
├─ host/          # loopback-only authenticated supervisor
├─ common/        # versioned IPC schemas and shared protocol tests
└─ providers/     # yue2, roformer, rvc, stable_audio environments/adapters
```

### Dependency rule

```text
aistudio → aijobs, aiproject, aimodels, songplan
aijobs   → aicore
aiproject → aicore
provider adapters → aicore protocol only
editor adapter → project / trackedit interfaces only
```

No provider module may include AU3 headers. No UI model may launch Python or a model worker. Only the local runtime host owns provider-process lifecycle.

## Editor-project adapter

Implement the master-plan `EditorProjectAdapter` in a new module, backed by `IGlobalContext`, `IAudacityProject`, and `ITrackeditProject`.

| Adapter operation | Proposed implementation |
|---|---|
| `projectPath()` | `currentProject()->path()`; reject/handle unsaved projects explicitly. |
| `selectedTrackRefs()` | selection controller plus `ITrackeditProject::track` metadata. |
| `selectedTimeRange()` | selection controller time range. |
| `importAudioAsset()` | stage and validate output, then call `IAudacityProject::import` or `importIntoTrack`; capture the resulting tracks. |
| `setTrackMute()` / `focusTrack()` | use track-edit/playback/navigation interfaces after their exact APIs are inspected for the first vertical slice. |

The adapter is the sole permitted bridge from AI orchestration to the timeline. The existing `aimusiccontroller.cpp` direct use of `AudacityProject`, `WaveTrack`, `LabelTrack`, and `au3::DomAccessor` is not the production pattern.

## Project and file lifecycle

`IAudacityProject` exposes create, load, import, close, save, path changes, and close notifications. It supports a project-adjacent AI workspace without changing the `.aup4` schema at the first milestone.

Recommended workspace policy:

```text
Saved project:  <project directory>/ai/
Unsaved project: <runtime temp>/jobs/<job id>/
First AI persistence request: offer Save/Create AI Workspace
```

Use atomic writes for `ai/manifest.json`: write a sibling temporary file, fsync/close, then replace. Keep the prior valid manifest as a backup. Store only project-relative asset paths in the manifest. On `aboutCloseBegin`, prevent new imports and detach UI subscriptions; running jobs may finish into their job directory but must not mutate a closing project.

## Runtime host and job boundary

The recommended local host is an independently supervised process with a random loopback port and a per-launch random session token. Its versioned IPC contract must cover:

- runtime health and provider discovery;
- model discovery and licence state;
- submit, progress, cancel, retry, and diagnostics;
- result manifests and checksums;
- worker death / interrupted-job reporting.

The native app should communicate with the host through an `IRuntimeClient` interface owned by `aijobs`. The interface must make timeout, cancellation, host restart, and protocol incompatibility explicit. A production implementation may use WebSocket/SSE as proposed in the master plan; a fake implementation supplies deterministic tests.

Job state is persisted as `queued`, `preparing`, `loading`, `running`, `decoding`, `importing`, `complete`, `failed`, or `cancelled`. Import is a final validated editor transaction, never an implicit side effect of worker output.

## QML and workspace recommendation

Audacity’s application shell is already a `DockWindow`; `WindowContent.qml` hosts dock pages and toolbars. Use a dedicated `AI Studio` dock panel on the right, exposed through a registered URI and action. Its initial tabs are Create, Plan, Separate, Vocals, Instruments, and Jobs, but Gate 1 should display only runtime status and a disabled/available vertical-slice command.

The song-plan editor should be a separate dock/panel or bottom panel. Keep it independent of `ProjectScene` timeline QML. Generic timeline overlays or track drawing may later use `projectscene` types, but only after the plan/job foundation is stable.

## Action registration

The current `aimusic` module demonstrates the correct mechanics: module context creates a controller, registers it with the action dispatcher, and registers UI actions through `IUiActionsRegister`.

Use this pattern for these action codes:

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

Action handlers validate project state and enqueue work only. They never perform inference, block for a worker, or directly mutate audio callbacks.

## Undo / redo contract

Use `IProjectHistory` for user-visible editor transactions only:

1. The runtime creates assets and a result manifest outside editor undo history.
2. The user chooses Insert, Replace, or Activate Version.
3. The adapter validates the result, imports it, and pushes one named history state.
4. Undo removes the inserted timeline state; the asset and provenance remain.
5. Redo restores the known asset without regenerating it.

Do not make model execution, job cancellation, or manifest writes part of Audacity undo/redo.

## Current proof-of-concept assessment

| Item | Status | Disposition |
|---|---|---|
| `src/aimusic` module registration | builds and registers actions | May remain as a quarantined R&D module. Do not use it as a production dependency. |
| Local helper process | proves external analysis process packaging | Replace with the runtime host/client abstraction. |
| BPM/key/chord/arrangement lanes | experimental imported-audio analysis | Not part of the master-plan baseline; stop feature expansion. Preserve only if explicitly retained as an R&D tool. |
| Direct AU3 track manipulation | works for the prototype | Prohibited for new AI workflow code except inside a tested adapter implementation. |
| QM/Chordino packaging | bundled third-party R&D dependency | Retain notices if retained; re-evaluate separately from the planned model/provider stack. |

## Remaining AU3 dependency and churn risk

| Area | Risk | Mitigation |
|---|---|---|
| Project and track implementation | High: current AU4 public interfaces still adapt AU3 internals. | Depend on `project`, `context`, and `trackedit` contracts; isolate any unavoidable AU3 access in one adapter. |
| Timeline QML / project scene | Medium-high: active AU4 QML is evolving. | Put AI Studio in its own dock module; avoid patching core label/track rendering. |
| Dock/workspace integration | Medium: workspace plumbing contains visible AU4 TODOs. | Add a small, independently registered dock page before attempting a custom overall workspace. |
| Import placement | Medium: public import exists, but exact transaction/track capture behavior needs a vertical-slice test. | Prove it with the test-provider WAV before provider work. |
| Save/reopen sidecar workspace | Medium: no first-class AI manifest support. | Keep manifest external, atomic, relative, and tolerant of missing providers. |
| Upstream tracking | High process risk: this checkout currently exposes only an `upstream` remote, not the planned `origin` fork remote. | Add/verify the fork `origin` before feature branches and keep upstream sync commits separate. |

## Required validation for Gate 1 and Gate 2

1. Build the isolated AI module shell with no Python/model dependency.
2. Show the AI Studio dock and action registration in a live Audacity session.
3. Use a fake runtime client to submit, progress, cancel, fail, and retry a deterministic job.
4. Create a saved project workspace and atomically persist a minimal manifest.
5. Close/reopen and confirm the manifest is found by a relative project path.
6. Replace the fake result with a test-provider WAV, import it through the editor adapter, and verify one undo/redo transaction without rerunning the provider.
7. Move the complete project folder and repeat the reopen/import-history check.
8. Kill the fake worker and verify the job becomes failed/interrupted with a usable diagnostic, while the editor remains responsive.

## Recommended next implementation slice

Gate 1 is now complete. The installed shell adds the `AI Studio` View-menu action and an initially hidden right-side panel. It has a versioned shared protocol constant, a fake unavailable runtime client, a project-workspace contract, and no provider/model dependency.

The next slice is **Gate 2 plus the test-provider vertical slice**: implement the authenticated local runtime host/client boundary, a persisted job state machine, and a deterministic WAV-plus-manifest provider. YuE2 begins only after that workflow survives save/reopen, cancellation, worker failure, undo/redo, and project relocation.

**Gate 2 complete, 2026-09-15:** `ai_runtime_host.exe` now provides a loopback-only JSON-line host with a required session token and protocol-version check. Its self-test proves unauthorized requests are rejected and the authenticated deterministic provider writes `output.wav` plus `result.json`. Opening AI Studio starts the host from the installed application directory with a fresh token, validates its authenticated health response, and presents the resulting runtime status in the dock. A user may explicitly enable the project-adjacent `ai/` workspace; the host then restarts in that workspace, writes the disposable job output under `ai/jobs/`, and atomically records job state in `ai/manifest.json`. The live test-provider vertical slice has passed: completion, cancellation, interrupted-job recovery, controlled worker failure, non-destructive new-track insertion, Audacity undo/redo, and project relocation. The test-provider gate is closed; YuE2 environment and licensing readiness is the next milestone.
