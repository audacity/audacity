# Cloud effects — feasibility notes

## Server side

### Audio extraction

- audio.com stores sample blocks (WavPack-compressed, addressed by hash, possibly spread across the cluster) plus the project description.
    - The description is in `ProjectSerializer` binary format (dict + doc), not plain XML.
    - The endpoint rebuilds a local `.aup3` from them (`au3-cloud-audiocom/sync/RemoteProjectSnapshot.cpp`).
- Extraction therefore needs Audacity itself: a **headless Audacity service** reusing the endpoint's rebuild code, then exporting the selection.

### Hosting

- The Audacity team owns the service end to end.
    - It's a stateless Docker service in its own k8s namespace, in `services/audacity-cloud-effect` of the audio.com backend repo.
    - It uses the standard projects API and checks the user's JWT.
    - We choose the stack and the contract; only deployment and monitoring are coordinated with audio.com.
- Open:
    - Version skew: the server build must read projects from every client version, including newer ones.
    - Running Audacity headless in Docker.

### Block gathering, storage, cleanup

- v1: rebuild the whole project, export, delete it. Nothing is kept per job.
- Cost grows with _project_ size, not selection size (e.g. 2 h × 4 tracks ≈ 2–3 GB of WavPack blocks, even for a 5 s selection).
- Open:
    - Ephemeral-disk limits × concurrent jobs, and latency on large projects.
    - Do block downloads stay inside the cluster or go through public presigned URLs? (ask Roman)
- Follow-ups:
    - **Cache** the rebuilt project per project, for the "tweak and re-run" case. The rebuild code already skips locally known blocks (`CalculateKnownBlocks`), so repeat runs become incremental.
    - **Needed blocks only.**
        - The server can't choose blocks alone: a block's id is in a WavPack tag inside the block (`WavPackCompressor.cpp`).
        - So the client sends the hashes of the blocks the selection touches, from its local `block_hashes` table.
        - This is additive: an optional request field, with a full fetch when it's absent.
        - Needs: the rebuild and export must tolerate missing blocks.
        - Risk: a block missed by mistake becomes silence without any error.

### Snapshot consistency

- Open: the selection must refer to a snapshot the server has. Does Apply = sync, then submit against snapshot X? What about unsynced changes, or slow or failed sync?

### Result placement

- Open: format describing where result clips go. For v1 (new tracks), how much does a changed project matter?

### Job lifecycle

- Open: Audacity closed mid-job, project opened on another machine, cancel/failure/timeout, and where job state lives.

### Auth / subscription / quota

- Open: which checks gate a job, and where they surface in the UI.

## Client side

### Server-delivered QML

- Loading is feasible: the view register accepts any URL (`builtineffectviewloader.cpp:75`).
- Open:
    - Security (QML runs JS).
    - Versioning against the app's QML API.
    - Caching and offline use.
    - Translations.

### Selection schema

- Open:
    - Range vs. clip selection.
    - Labels.
    - Multiple sample rates, stereo.
    - Gaps, trims, stretching.

### Plugin manager / menus

- Runtime reload of effects and menus already exists (extensions use it).
- The cloud badge is new UI work: there's no per-item menu icon today.
- Open: offline behaviour.

### Apply path and history

- Open: `Process` is expected to produce audio. What undo/history entry, if any, does a submitted job create?
