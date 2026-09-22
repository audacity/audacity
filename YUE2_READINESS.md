# YuE2 Readiness Record

**Checked:** 2026-09-15  
**Status:** the official full-precision runtime is blocked on this workstation. The separate ComfyUI BF16 cover workflow is retained only as R&D feasibility evidence; it is not an integration target or runtime dependency.

## Pinned official sources

| Component | Official source | Revision checked |
| --- | --- | --- |
| Inference code | `multimodal-art-projection/YuE` | GitHub `main`, checked 2026-09-15 |
| Main generator | `m-a-p/YuE2-3B` | `14fc6c6f146441b1dd6363fcb2e01e82a6914cb7` |
| Default decoder | `m-a-p/YuE2-Vae` | `9a94e1d0ea9f8087e98f77fa88df4a4068104d2a` |
| Optional cover/transcription stack | `m-a-p/SheetSage2` + `m-a-p/MERT-v2-FullSong` | separate environment; not required for new-song generation |

The official YuE2 README describes a staged API: `plan()` then semantic generation, synthesis, and decoding. Its artifact output retains the score, semantic tokens, acoustic latents, settings, and model identities. This aligns with the project-adjacent AI workspace and non-destructive version model.

The current official model package includes `yue2_infer-0.1.5-py3-none-any.whl` and exposes `YuE2Pipeline.from_pretrained("m-a-p/YuE2-3B", device="cuda")`. That is the selected native integration seam: the AI Music Studio worker owns the Python process, calls the pipeline directly, persists `save_artifacts()` output, and never shells out to or embeds ComfyUI.

## Licence gate

- YuE2 first-party code, documentation, and skill: Apache-2.0.
- YuE2-3B and YuE2-Vae model weights: CC BY-NC 4.0.
- Third-party notices remain separate and must ship with any provider package.

The runtime must expose the weight licence and require an explicit non-commercial acknowledgement before download or use. Commercial deployment is out of scope unless separate written permission is obtained from the rights holders.

## Hardware and operating-system check

The current official quick-start requires Linux, Python 3.10 or newer, an NVIDIA GPU with BF16 support, and **24 GB VRAM**. The official route produces 48 kHz stereo audio without quantization.

Observed workstation:

| Item | Observed | Result |
| --- | --- | --- |
| GPU | NVIDIA GeForce RTX 4060 Ti | BF16-capable architecture, but capacity is insufficient |
| VRAM | 16,380 MiB | fails the official 24 GB requirement |
| Driver | 616.92 | driver itself is not the current gate |
| Operating system | Windows 11 | not the official quick-start platform |

The direct runtime is therefore an isolated native-Windows feasibility experiment, not a claimed supported configuration. It must use a dedicated environment and its own model cache; it must not borrow ComfyUI's Python installation, custom nodes, or checkpoint layout.

## Native low-VRAM candidate: audio.cpp GGUF

`audio-cpp/Yue2-3B-GGUF` supplies native CUDA GGUF weights for the independent `audio.cpp` runtime. It is not a ComfyUI package or dependency. Its YuE2 implementation is currently published as a development branch for community testing, so it is a provider candidate rather than a release-default claim.

| Tier | Native files | Download size | Published 5090 peak VRAM | Intended eligibility |
| --- | --- | ---: | ---: | --- |
| Full reference | Official BF16 pipeline + default VAE | 7.29 GB main model plus decoder assets | official 24 GB requirement | 24 GB or more |
| Quality-balanced candidate | Q8_0 GGUF + F16 VAE | validate before release | 8.87 GiB | at least 12 GB after local validation |
| Low-VRAM candidate | Q4_0 GGUF + F16 VAE | 2.7 GB + 265.2 MB | 7.76 GiB | 12 GB target, pending local validation |

The published Q4/Q8 measurements are on an RTX 5090, not a guarantee for 12 GB cards or other driver/kernel combinations. The application must preflight the selected backend, report available VRAM, run one model at a time, and decline unsupported configurations cleanly. It must not silently fall back to ComfyUI or switch precision without recording the effective model and settings.

**Decision direction:** retain the official direct pipeline as the quality reference; investigate `audio.cpp` Q4_0 + F16 VAE as the native low-VRAM provider; promote it only after a representative 12 GB benchmark covers generation, cancellation, provenance, result insertion, and listening quality.

## ComfyUI provider evidence

`Comfy-Org/YuE2` repackages YuE2 assets for ComfyUI and carries the same CC BY-NC 4.0 weight licence. Its current checked revision is `8e6fcf0f23252ed188b634bd50d44f4b01fba890`.

On 2026-09-15, the project owner successfully completed a cover with the ComfyUI workflow using `yue2_3b_bf16.safetensors` on the observed RTX 4060 Ti (16 GB). This establishes local feasibility for that exact user-operated workflow; it does not establish unattended API operation, reproducibility, memory headroom, feature parity, or production readiness.

Local preflight on 2026-09-15 confirmed a healthy loopback ComfyUI 0.35.0 host at `http://127.0.0.1:8188`, using embedded Python 3.12.10 and PyTorch 2.11.0+cu130. Its node registry contains every class required by `yue2_comfyui.json`, including `YuE2GenerateMusic`, `YuE2GenerateABC`, `SheetSage2AudioToABC`, `PixaromaSwitch`, and `SaveAudioAdvanced`. This clears only the service-compatibility gate; it does not submit or alter a generation job.

## Benchmark evidence

On 2026-09-17, the unchanged API template completed through the local ComfyUI endpoint. The immutable machine-readable report is `benchmarks/yue2/reports/comfy-yue2-20260917-093432.json`.

| Measure | Observed result |
| --- | ---: |
| Terminal status | `success` |
| Wall-clock generation time | 264.215 s |
| Peak GPU memory | 15,455 MiB of 16,380 MiB |
| Peak GPU utilisation | 100% |
| Peak GPU temperature | 65 C |
| Output | `audio/YuE2_00002.mp3` |
| Output size | 4,805,299 bytes |
| Output duration | 2:36 |

The run leaves roughly 925 MiB below the observed VRAM capacity, so it proves the tested configuration can complete but does not yet establish safe headroom for longer jobs, other ComfyUI nodes, or concurrent GPU use. The current workflow reports one wall-clock figure rather than separate plan, semantic, acoustic, and decode timings; preserve that limitation in any comparison.

The 2026-09-17 Audacity disposable-project check also passed: the rendered MP3 imported, played, and retained ordinary undo/redo behavior. This closes the real-output insertion check for the R&D workflow; it does not make ComfyUI an end-user dependency or replace the planned native provider path.

The read-only Hugging Face dry run reports the minimum ComfyUI pair as:

| File | Download size |
| --- | ---: |
| `checkpoints/yue2_3b_int8_convrot.safetensors` | 4.0 GB |
| `audio_encoders/sheetsage2_bf16.safetensors` | 1.4 GB |
| **Total** | **5.4 GB** |

This satisfies the under-8-GB model-package target for the int8 alternative. It does **not** establish memory use, generation quality, exact feature parity, or a commercial right. The Comfy provider must be isolated behind its own adapter and must pass the same cancellation, provenance, insertion, and regression checks as the test provider.

## Decision

Do not treat a ComfyUI workflow or an unsupported Windows port as the official full-precision YuE2 provider. ComfyUI is not an integration target for this project. Its pinned workflow and benchmark evidence are retained solely to inform native-runtime feasibility.

The next decision required from the project owner is one of:

1. Run the official YuE2 provider on a Linux machine with at least 24 GB VRAM.
2. Keep the current workstation as the Audacity client and use a separately managed local/remote provider host on qualifying hardware.
3. Approve the `Comfy-Org/YuE2` int8 package as a separate research-only Windows provider and validate it on this workstation; it must not replace the official provider by default.

## Sources

- https://github.com/multimodal-art-projection/YuE
- https://huggingface.co/m-a-p/YuE2-3B
- https://huggingface.co/m-a-p/YuE2-Vae
- https://huggingface.co/m-a-p/SheetSage2
- https://huggingface.co/m-a-p/MERT-v2-FullSong
- https://huggingface.co/Comfy-Org/YuE2
- https://huggingface.co/audio-cpp/Yue2-3B-GGUF
