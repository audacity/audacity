# This script uses signing service to sign windows binaries.

# The file to be signed should be uploaded to the /unsigned/ folder.
# The signing service runs automatically on a schedule
# The result - the signed file - is placed by the service in the same s3 bucket
#     in the signed folder with the same name as the source file.
# The file must be removed from the signed folder after downloading.

[CmdletBinding()]
param (
    # it can be a single file or a directory, but not both
    [string]$File,
    [string]$Directory
)

# Configure the error behavior
$ErrorActionPreference = "Stop"
$PSDefaultParameterValues['*:ErrorAction']='Stop'

$env:AWS_DEFAULT_REGION="us-east-1"
$env:AWS_ACCESS_KEY_ID = $env:WINDOWS_CODE_SIGNING_ACCESS_KEY_ID
$env:AWS_SECRET_ACCESS_KEY = $env:WINDOWS_CODE_SIGNING_SECRET_ACCESS_KEY

$s3Bucket = "muse-sign"
$s3UnsignedDir = "unsigned"
$s3SignedDir = "signed"

function Upload-FilesForSigning {
    param (
        [string[]]$FilesToUpload
    )

    Write-Host "Uploading files for signing..."
    foreach ($file in $FilesToUpload) {
        $fileName = [System.IO.Path]::GetFileName($file)
        $s3UnsignedUrl = "s3://$s3Bucket/$s3UnsignedDir/$fileName"
        Write-Host "Uploading $file to $s3UnsignedUrl"
        aws s3 cp $file $s3UnsignedUrl | Out-Null
    }
    Write-Host "All unsigned files uploaded:"
    $FilesToUpload | ForEach-Object { Write-Host " - $_" }
}

function Download-SignedFiles {
    param (
        [string[]]$FilesToDownload
    )

    Write-Host "Waiting for signed files from S3..."
    Write-Host "Expecting signed files for:"
    $FilesToDownload | ForEach-Object { Write-Host " - $_" }

    $signedFilesDownloaded = @()
    $maxAttempts = 10
    $sleepSeconds = 60

    for ($attempt = 1; $attempt -le $maxAttempts; $attempt++) {
        Write-Host "`nChecking for signed files... attempt $attempt"
        $allSigned = $true

        foreach ($file in $FilesToDownload) {
            $fileName = [System.IO.Path]::GetFileName($file)
            $s3SignedUrl = "s3://$s3Bucket/$s3SignedDir/$fileName"
            $fileSignedPath = "${file}_signed"

            if ($signedFilesDownloaded -contains $fileName) {
                Write-Host "Already downloaded: $fileName"
                continue
            }

            Write-Host "Trying to download signed file: $fileName"
            aws s3 cp $s3SignedUrl $fileSignedPath
            if ($LASTEXITCODE -eq 0) {
                Write-Host "Successfully downloaded signed file: $fileName"
                $signedFilesDownloaded += $fileName
                Write-Host "Replacing original with signed file: $file"

                Remove-Item -Path ${file} -Force
                Rename-Item -Path $fileSignedPath -NewName $file -Force

                Write-Host "Removing signed file from S3: $s3SignedUrl"
                aws s3 rm $s3SignedUrl | Out-Null
            } else {
                Write-Host "Signed file not available yet: $fileName"
                $allSigned = $false
            }
        }

        $pendingFiles = $FilesToDownload.Where({ $signedFilesDownloaded -notcontains [System.IO.Path]::GetFileName($_) })
        if ($pendingFiles.Count -gt 0) {
            Write-Host "Still waiting for:"
            $pendingFiles | ForEach-Object { Write-Host " - $_" }
        }

        Write-Host "Signing done: $allSigned, Files processed: $($signedFilesDownloaded.Count) / $($FilesToDownload.Count)"

        if ($allSigned -and ($FilesToDownload.Count -eq $signedFilesDownloaded.Count)) {
            Write-Host "`nAll signed files downloaded and processed successfully."
            break
        } elseif ($attempt -eq $maxAttempts) {
            throw "Signing failed for some files after multiple attempts. Missing signed files: $($pendingFiles)"
        }

        Write-Host "Sleeping for $sleepSeconds seconds before next check..."
        Start-Sleep -Seconds $sleepSeconds
    }
}

# Only allow File or Directory
if ($File -eq "" -and $Directory -eq "") {
    Write-Host "-File or -Directory should be provided"
    exit 1
} elseif ($File -ne "" -and $Directory -ne "") {
    Write-Host "Only one of -File or -Directory should be provided"
    exit 1
}

# Get list of files to sign
$filesToProcess = @()
if ($File -ne "") {
    Write-Host "Processing single file: $File"
    $filesToProcess += $File
} else {
    Write-Host "Searching for unsigned files in directory: $Directory"
    # dlls are skipped for now because the signing process is too slow
    $filesToProcess = Get-ChildItem `
        -Path "$Directory" `
        -Include *.exe,*.msi `
        -Recurse `
        -File | Where-Object {
            (Get-AuthenticodeSignature $_.FullName).Status -ne [System.Management.Automation.SignatureStatus]::Valid
        } | Select-Object -ExpandProperty FullName

    Write-Host "Unsigned files found:"
    $filesToProcess | ForEach-Object { Write-Host " - $_" }
}

if ($filesToProcess.Count -eq 0) {
    Write-Host "No unsigned files found to process."
    exit 0
}

# Step 1: Upload all unsigned files
Upload-FilesForSigning -FilesToUpload $filesToProcess

# Step 2: Download all signed files
Download-SignedFiles -FilesToDownload $filesToProcess

Write-Host "`nSigning process completed successfully."
