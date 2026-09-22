# Native YuE2 Q4 12 GB Acceptance Test

**Purpose:** decide whether the native `audio.cpp` Q4 YuE2 path is viable for the 12 GB GPU tier.  
**Excluded:** ComfyUI, its Python runtime, custom nodes, and its checkpoints.

## Test target

| Item | Pin |
| --- | --- |
| Runtime | `0xShug0/audio.cpp`, `dev` branch, CUDA build |
| Model package | `audio-cpp/Yue2-3B-GGUF` at `eb116220931de5f373d024d48800338178c7de51` |
| Main model | `yue2-3b-q4_0.gguf` |
| Decoder | `yue2-vae-f16.gguf` |
| Required sidecars | `sidecars/yue2-model-config.json`, `sidecars/yue2-generation-config.json`, `sidecars/yue2-qwen.tiktoken`, `sidecars/yue2-vae-config.json` |

The Q4 main model is 2.7 GB and the F16 VAE is 265.2 MB. The upstream package reports 7.76 GiB peak VRAM for its Q4 + F16 longform measurement on an RTX 5090. That is a starting hypothesis, not a 12 GB compatibility claim.

## Prerequisites

- Windows 11 x64, NVIDIA GPU with 12 GB VRAM, and current NVIDIA driver.
- No other GPU-heavy applications running during the test.
- Visual Studio Build Tools 2022 or newer with Desktop C++, MSVC x64, Windows SDK, CMake, Ninja, and MSVC OpenMP.
- NVIDIA CUDA Toolkit compatible with the selected `audio.cpp` build.
- Git and the Hugging Face `hf` CLI.

## Setup

Run these commands from a short, space-free working path such as `D:\native-yue2-test`:

```powershell
git clone --branch dev --recurse-submodules https://github.com/0xShug0/audio.cpp audio.cpp
Set-Location .\audio.cpp
.\scripts\build_windows.ps1 -Preset windows-cuda-release -Target audiocpp_cli
New-Item -ItemType Directory -Force .\models\yue2-q4 | Out-Null
hf download audio-cpp/Yue2-3B-GGUF --revision eb116220931de5f373d024d48800338178c7de51 --include yue2-3b-q4_0.gguf --include yue2-vae-f16.gguf --include sidecars/* --local-dir .\models\yue2-q4
```

Do not use a released binary unless its release notes explicitly list YuE2 support. YuE2 is currently in the `dev` branch.

## One benchmark run

Use an original test prompt. Do not use a commercial song or private recording for this hardware gate.

```powershell
$cli = '.\build\windows-cuda-release\bin\audiocpp_cli.exe'
$model = (Resolve-Path .\models\yue2-q4).Path
$out = Join-Path (Get-Location) 'yue2-q4-12gb-test.wav'

nvidia-smi --query-gpu=name,memory.total,memory.used,temperature.gpu --format=csv,noheader
& $cli --task gen --family yue2 --model $model --backend cuda --threads 8 --text "[Verse]`nMorning light arrives, a new melody begins.`n[Chorus]`nCarry the rhythm home, bright and clear." --request-option "style=English indie pop, warm lead vocal, acoustic guitar, bass, light drums" --request-option cot=full --request-option seed=20260917 --request-option num_inference_steps=8 --session-option yue2.model_gguf=yue2-3b-q4_0.gguf --session-option yue2.vae_gguf=yue2-vae-f16.gguf --out $out --log
nvidia-smi --query-gpu=name,memory.total,memory.used,temperature.gpu --format=csv,noheader
```

If the current `dev` CLI reports an option-name change, stop and retain the exact command and error; do not improvise a fallback runtime.

## Pass criteria

1. CUDA backend is selected and the process exits successfully.
2. A playable WAV is produced.
3. No out-of-memory event, driver reset, system lockup, or corrupted output occurs.
4. Capture peak VRAM, temperature, wall time, generated duration, and output file size.
5. Listen for obvious dropouts, silence, severe distortion, or an incomplete song.
6. Import the WAV into a disposable Audacity project; confirm playback and undo/redo.

Record the GPU model, driver, `audio.cpp` commit, model-package revision, exact command, and all measurements. A pass on one 12 GB GPU establishes only that configuration; it does not automatically promote Q4 to the general release default.
