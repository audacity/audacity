# Remiqora Reference Audit

**Reference inspected:** [inikolax/remiqora](https://github.com/inikolax/remiqora) on 2026-09-20.  
**Purpose:** learn from its workflow/orchestration concepts without copying its browser DAW, model choices, or source architecture.

## Summary

Remiqora documents a shared SQLite-and-files library, a model lifecycle orchestrator that enforces heavyweight GPU mutual exclusion, job cancellation, source-to-stem-to-editor flow, and a FastAPI/Vue/Web Audio application. Its repository says it is actively developed and expects breaking changes. Those concepts are useful; the browser editor architecture is not compatible with this Audacity 4 fork.

## Decision matrix

| Reference feature | Decision | AI Music Studio treatment |
|---|---|---|
| Shared track library and files | ADOPT CONCEPT | `ailibrary` owns project assets; global indexing is optional and never required to understand a project. |
| SQLite catalogue | ADAPT | Index global search/favourites/project membership only; portable project manifests retain provenance and relationships. |
| Generated tracks as production material | ADOPT CONCEPT | Every output remains a Library asset even when no longer on the timeline. |
| One-click source -> stems -> editor | ADOPT CONCEPT | Use Audacity import/track interfaces after validated Library registration. |
| Model start/stop/health orchestration | ADAPT | External authenticated local runtime host; providers are independent from the Audacity process. |
| GPU mutual exclusion/lock | ADOPT CONCEPT | Default to one heavyweight job; evidence is required before allowing concurrency. |
| Progress, cancellation, and retry | ADOPT CONCEPT | Persisted `aijobs` state machine with diagnostics and restart recovery. |
| Separate-player stem preview | ADAPT | Lightweight audition/solo/mute preview; no second full mixer. |
| Open-in-editor transition | ADAPT | Add selected, already-registered Library assets to the Audacity timeline. |
| Transcription routing | ADAPT | Provider-neutral boundary; support MuScriptor/SheetSage2 only after capability and Windows validation. |
| File handling and model health checks | ADOPT CONCEPT | Checksums, model registry, licence display, explicit install/repair, and no silent downloads. |
| ACE-Step production engine/training | REJECT | Explicitly outside this baseline. |
| Demucs as fixed separation implementation | REJECT | Use a provider-neutral RoFormer/UVR registry; Demucs may be a future compatible provider, not an architectural default. |
| Vue/Pinia/Tailwind application | REJECT | Native Qt/QML modules only. |
| Web Audio mixer/timeline/effects | REJECT | Audacity owns editing, playback, effects, import/export, and undo. |
| Browser-side rendering/project persistence | REJECT | Use Audacity project lifecycle plus portable AI workspace metadata. |
| YuE2 symbolic plan reuse | ADAPT | Normalize into immutable project Song Plan revisions instead of storing raw provider data as the canonical plan. |
| LoRA support | RESEARCH | Capability-gated YuE2 Artist/Style/Instrumental adaptations with explicit compatibility; do not treat it as voice cloning. |
| Concurrent separation beside generation | RESEARCH | Only permit after VRAM, cancellation, and recovery measurements on target consumer hardware. |

## Architecture translation

```text
Remiqora: browser UI -> FastAPI/orchestrator -> model servers -> shared SQLite/files -> Web Audio DAW
AI Music Studio: Qt/QML dock -> aijobs/runtime IPC -> local provider workers -> portable manifest + optional catalogue -> Audacity editor
```

## Guardrails carried into implementation

- A Library asset, a timeline clip, a manifest record, and a disk file are separate concepts with separate delete operations.
- Runtime workers write only into job directories. The app validates, checksums, registers, and persists before it offers editor insertion.
- All contextual menus must use an asset capability service, so invalid operations are absent rather than merely disabled.
- The model manager records location, revision, checksum, licence, and compatibility; it does not acquire multi-gigabyte files without consent.
- The browser project is a reference only. No code, UI assets, or model-specific assumptions are to be copied into this GPL Audacity fork.

## Immediate outcome

The reference confirms the first implementation priority: a persistent, non-destructive Library and a provider-independent job/runtime boundary. It does not justify starting YuE2 or changing the existing editor/audio engine before the Library vertical slice passes native save/reopen validation.
