[CmdletBinding()]
param(
    [string]$BaseUrl = 'http://127.0.0.1:8188',
    [string]$WorkflowPath,
    [string]$ReportDirectory,
    [int]$PollSeconds = 2,
    [switch]$Submit
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# Windows PowerShell 5.1 evaluates parameter defaults before $PSScriptRoot is
# populated, so resolve these script-relative defaults after the param block.
if ([string]::IsNullOrWhiteSpace($WorkflowPath)) {
    $WorkflowPath = Join-Path $PSScriptRoot '..\yue2_comfyui.json'
}
if ([string]::IsNullOrWhiteSpace($ReportDirectory)) {
    $ReportDirectory = Join-Path $PSScriptRoot '..\benchmarks\yue2\reports'
}

# Windows PowerShell 5.1 cannot use ConvertFrom-Json -AsHashtable. ComfyUI's
# node registry contains keys which differ only by case, so PSCustomObject is
# not suitable either. JavaScriptSerializer returns a case-sensitive dictionary
# on both Windows PowerShell 5.1 and PowerShell 7.
Add-Type -AssemblyName System.Web.Extensions
$jsonSerializer = New-Object System.Web.Script.Serialization.JavaScriptSerializer
$jsonSerializer.MaxJsonLength = [int]::MaxValue
$jsonSerializer.RecursionLimit = 100

$requiredClasses = @(
    'EmptyYuE2LatentAudio', 'KSampler', 'VAEDecodeAudio', 'SaveAudioAdvanced',
    'PreviewAny', 'CheckpointLoaderSimple', 'AudioEncoderLoader',
    'SheetSage2AudioToABC', 'YuE2GenerateMusic', 'YuE2GenerateABC',
    'LoadAudio', 'PixaromaSwitch'
)

function Get-JsonResponse {
    param([Parameter(Mandatory = $true)][string]$Uri)

    if ($PSVersionTable.PSVersion.Major -lt 6) {
        $response = Invoke-WebRequest -Uri $Uri -TimeoutSec 30 -UseBasicParsing
    } else {
        $response = Invoke-WebRequest -Uri $Uri -TimeoutSec 30
    }
    return $jsonSerializer.DeserializeObject($response.Content)
}

function Get-GpuSample {
    $lines = & nvidia-smi --query-gpu=name,temperature.gpu,memory.used,memory.total,utilization.gpu --format=csv,noheader,nounits 2>$null
    if ($LASTEXITCODE -ne 0 -or -not $lines) {
        return @()
    }

    return @($lines | ForEach-Object {
        $fields = $_ -split ',\s*'
        [ordered]@{
            name = $fields[0]
            temperatureC = [double]$fields[1]
            memoryUsedMiB = [double]$fields[2]
            memoryTotalMiB = [double]$fields[3]
            utilizationPercent = [double]$fields[4]
        }
    })
}

function Get-HttpFailureDetails {
    param([Parameter(Mandatory = $true)]$ErrorRecord)

    $response = $ErrorRecord.Exception.Response
    if ($null -eq $response) {
        return $ErrorRecord.Exception.Message
    }

    try {
        $reader = New-Object System.IO.StreamReader($response.GetResponseStream())
        $body = $reader.ReadToEnd()
        $reader.Dispose()
        if (-not [string]::IsNullOrWhiteSpace($body)) {
            return $body
        }
    } catch {
        # Preserve the original request error if its response stream is unreadable.
    }

    return "HTTP $([int]$response.StatusCode) $($response.StatusDescription)"
}

function Write-BenchmarkReport {
    param([Parameter(Mandatory = $true)]$Report)

    New-Item -ItemType Directory -Force -Path $ReportDirectory | Out-Null
    $stamp = Get-Date -Format 'yyyyMMdd-HHmmss'
    $path = Join-Path $ReportDirectory "comfy-yue2-$stamp.json"
    $Report | ConvertTo-Json -Depth 16 | Set-Content -NoNewline -Encoding utf8 -Path $path
    Write-Output "Report: $path"
}

if ($PollSeconds -lt 1) {
    throw 'PollSeconds must be at least 1.'
}

$workflowFullPath = (Resolve-Path -LiteralPath $WorkflowPath).Path
$workflowText = Get-Content -Raw -LiteralPath $workflowFullPath
$prompt = $jsonSerializer.DeserializeObject($workflowText)

$base = $BaseUrl.TrimEnd('/')
$startedAt = Get-Date
$systemStats = Get-JsonResponse -Uri "$base/system_stats"
$objectInfo = Get-JsonResponse -Uri "$base/object_info"
$missingClasses = @($requiredClasses | Where-Object { -not $objectInfo.ContainsKey($_) })

$report = [ordered]@{
    profileId = 'comfy-yue2-bf16-cover-v1'
    runMode = $(if ($Submit) { 'submitted' } else { 'preflight-only' })
    startedAtUtc = $startedAt.ToUniversalTime().ToString('o')
    baseUrl = $base
    workflowPath = $workflowFullPath
    workflowSha256 = (Get-FileHash -Algorithm SHA256 -LiteralPath $workflowFullPath).Hash.ToLowerInvariant()
    systemStats = $systemStats
    requiredClasses = $requiredClasses
    missingClasses = $missingClasses
    gpuSamples = @(Get-GpuSample)
    promptId = $null
    terminalStatus = $null
    elapsedSeconds = $null
    history = $null
}

if ($missingClasses.Count -gt 0) {
    $report.terminalStatus = 'preflight-failed'
    Write-BenchmarkReport -Report $report
    throw "ComfyUI is missing required workflow classes: $($missingClasses -join ', ')"
}

if (-not $Submit) {
    $report.terminalStatus = 'preflight-passed'
    $report.elapsedSeconds = [math]::Round(((Get-Date) - $startedAt).TotalSeconds, 3)
    Write-BenchmarkReport -Report $report
    Write-Output 'Preflight passed. Re-run with -Submit to queue the unchanged workflow.'
    return
}

$requestBody = @{ prompt = $prompt; client_id = [guid]::NewGuid().ToString() } | ConvertTo-Json -Depth 100
try {
    # Windows PowerShell 5.1 otherwise encodes a string request body with the
    # current ANSI code page. The template contains Unicode punctuation, while
    # ComfyUI's aiohttp endpoint correctly requires UTF-8 JSON.
    $requestBytes = [System.Text.Encoding]::UTF8.GetBytes($requestBody)
    $queued = Invoke-RestMethod -Method Post -Uri "$base/prompt" -ContentType 'application/json; charset=utf-8' -Body $requestBytes -TimeoutSec 30
} catch {
    $report.terminalStatus = 'submission-failed'
    $report.submissionError = Get-HttpFailureDetails -ErrorRecord $_
    $report.elapsedSeconds = [math]::Round(((Get-Date) - $startedAt).TotalSeconds, 3)
    Write-BenchmarkReport -Report $report
    throw "ComfyUI rejected the prompt: $($report.submissionError)"
}
if (-not $queued.prompt_id) {
    throw 'ComfyUI accepted the request without returning prompt_id.'
}

$report.promptId = [string]$queued.prompt_id
Write-Output "Queued ComfyUI prompt: $($report.promptId)"

while ($true) {
    Start-Sleep -Seconds $PollSeconds
    $report.gpuSamples += @(Get-GpuSample)
    $history = Get-JsonResponse -Uri "$base/history/$($report.promptId)"
    if (-not $history.ContainsKey($report.promptId)) {
        continue
    }

    $entry = $history[$report.promptId]
    $status = [string]$entry.status.status_str
    if ($status -in @('success', 'error')) {
        $report.terminalStatus = $status
        $report.history = $entry
        break
    }
}

$report.elapsedSeconds = [math]::Round(((Get-Date) - $startedAt).TotalSeconds, 3)
Write-BenchmarkReport -Report $report
if ($report.terminalStatus -ne 'success') {
    throw "ComfyUI job ended with status '$($report.terminalStatus)'."
}

Write-Output "Benchmark complete in $($report.elapsedSeconds) seconds."
