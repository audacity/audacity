# ComfyUI YuE2 BF16 Cover Provider Contract

**Profile ID:** `comfy-yue2-bf16-cover-v1`  
**Template:** `yue2_comfyui.json` at the repository root  
**Scope:** local, research-only, non-commercial YuE2 cover-generation evidence through an already-running ComfyUI service. This is not a provider contract to implement and ComfyUI is not an AI Music Studio runtime dependency.

## Verified template shape

The supplied template is ComfyUI's API prompt format, not its graphical UI workflow export. It declares the following path:

```text
LoadAudio (33) -> SheetSage2AudioToABC (19) -> PixaromaSwitch (35)
                                                -> YuE2GenerateMusic (22)
CheckpointLoaderSimple (15) -------------------> YuE2GenerateMusic / KSampler (8)
EmptyYuE2LatentAudio (5) ----------------------> KSampler
KSampler -> VAEDecodeAudio (9) -> SaveAudioAdvanced (10)
```

The template is configured for the observed BF16 pair:

- `yue2_3b_bf16.safetensors` in node `15`.
- `sheetsage2_bf16.safetensors` in node `18`.
- Melody extraction in node `19`.
- A full-music generation path in node `22`.
- `dpm_2`, `sgm_uniform`, 32 steps, CFG 1 in node `8`.
- MP3 output from node `10`.

The supplied workflow is evidence of a successful interactive cover and is the source template. It must never be overwritten by job submission.

## Provider endpoint contract

The provider only talks to a ComfyUI instance bound to loopback by default:

| Purpose | Method and endpoint |
| --- | --- |
| Health and host details | `GET /system_stats` |
| Required-node validation | `GET /object_info` |
| Queue a rendered prompt | `POST /prompt` |
| Observe a job | `GET /history/{prompt_id}` (and optionally `/ws`) |
| Cancel current work | `POST /interrupt` |

The default base URL is `http://127.0.0.1:8188`. A non-loopback URL must require an explicit user setting and be shown in the AI Studio UI before a job can run.

## Submission transformation

For each job, the adapter reads the template, deep-copies it, and makes only the following explicit substitutions. No node IDs, sampler settings, switch position, model names, or hidden defaults may be guessed or mutated.

| Job input | Template location | Rule |
| --- | --- | --- |
| Source audio | `33.inputs.audio` | Copy the selected Audacity audio asset into ComfyUI's configured input directory; replace the value with its generated basename. |
| Style text | `22.inputs.style` | Replace only when the user supplies style text. |
| Lyrics | `22.inputs.lyrics` | Replace only when the user supplies lyrics. |
| Generation seed | `22.inputs.seed` | Store the chosen seed; `0` is valid only if the user requests random behavior and the resolved Comfy seed is recoverable. |
| Render duration | `5.inputs.seconds` | Replace only after an explicit duration choice; keep the template duration otherwise. |
| Output location | `10.inputs.filename_prefix` | Replace with `ai/jobs/<job-id>/render` (relative to ComfyUI output) to prevent collisions. |

The input copy uses a job-specific name and SHA-256. The source audio is never modified.

## Preflight requirements

Before queuing, the adapter must prove that `/object_info` advertises every template `class_type`:

`EmptyYuE2LatentAudio`, `KSampler`, `VAEDecodeAudio`, `SaveAudioAdvanced`, `PreviewAny`, `CheckpointLoaderSimple`, `AudioEncoderLoader`, `SheetSage2AudioToABC`, `YuE2GenerateMusic`, `YuE2GenerateABC`, `LoadAudio`, and `PixaromaSwitch`.

It must also verify that the configured BF16 checkpoint and audio encoder names are available. A failure is reported as a provider preflight error, not as an Audacity project error.

### Observed preflight

On 2026-09-15, `GET /system_stats` and `GET /object_info` passed against the local host. The host reported ComfyUI 0.35.0, embedded Python 3.12.10, and PyTorch 2.11.0+cu130. All twelve workflow class types listed above were present. This check did not submit a prompt, write an input, or change the active ComfyUI graph.

## Provenance and project workspace

Each accepted job writes these project-adjacent artifacts under `ai/jobs/<job-id>/` before result insertion:

- immutable submitted API prompt JSON;
- provider profile ID, base URL, ComfyUI host details, and Comfy prompt ID;
- model and encoder names, template SHA-256, source-audio SHA-256, selected settings, timestamps, terminal state, and error text if any;
- a copied output audio file and its SHA-256;
- the imported Audacity track identity once result insertion succeeds.

The imported audio remains an ordinary Audacity track and must retain the existing undo/redo behavior. Cancellation sends `/interrupt`, retains the manifest, and never imports a partial result. A restart with an outstanding prompt ID must query `/history/{prompt_id}` before it offers a retry.

## Benchmark status and next implementation gate

The local endpoint and node-inventory preflight passed. A full workflow benchmark also passed on 2026-09-17: 264.215 seconds elapsed, 15,455 MiB peak GPU memory, 100% peak GPU utilisation, 65 C peak temperature, and a 2:36 MP3 output. The report is `benchmarks/yue2/reports/comfy-yue2-20260917-093432.json`.

The real-output insertion gate passed on 2026-09-17: the rendered MP3 imported, played, and survived undo/redo in a disposable Audacity project. This remains benchmark evidence only. Do not implement a ComfyUI adapter, expose ComfyUI controls in AI Studio, or require users to install ComfyUI. To repeat the benchmark, run the opt-in harness from the repository root:

```powershell
.\tools\comfy-yue2-benchmark.ps1 -Submit
```

It creates a timestamped report in `benchmarks/yue2/reports/`, recording host identity, template hash, prompt ID, elapsed duration, terminal history, and sampled GPU temperature, utilization, and memory. It does not alter the source template. The current harness deliberately does not implement the Audacity provider adapter or import audio; those remain separate, post-benchmark work.
