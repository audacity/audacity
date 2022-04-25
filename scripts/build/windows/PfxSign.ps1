# This script uses Set-AuthenticodeSignature to sign a file
# or exe/dll/msi files in a directory using a pfx file.
# If PFX file is not present, script expects a base64 encode certificate
# in WINDOWS_CERTIFICATE environment variable.
# Password can be passed using WINDOWS_CERTIFICATE_PASSWORD environment variable.
# The script only signs previously unsigned files

[CmdletBinding()]
param (
    # A path to the certificate file. If missing, env:WINDOWS_CERTIFICATE will be used.
    [string]$CertFile, 
    # A password for the certificate. If missing, env:WINDOWS_CERTIFICATE_PASSWORD will be used.
    [string]$Password,
    # A file to sign. 
    [string]$File,
    # A directory to sign.
    [string]$Directory
)

# Configure the error behavior
$ErrorActionPreference = "Stop"
$PSDefaultParameterValues['*:ErrorAction']='Stop'

# Sign a file, if it was previously unsigned
# We sign using SHA256, as we only support Windows 7+.
function Set-FileSignature {
    param (
        [String]$InputFile,
        $pfxCert
    )
    if((Get-AuthenticodeSignature $InputFile).Status -ne [System.Management.Automation.SignatureStatus]::Valid) {
        Write-Host "Singning file $InputFile"

        Set-AuthenticodeSignature `
            -FilePath $InputFile `
            -Certificate $pfxCert `
            -IncludeChain All `
            -TimestampServer 'http://timestamp.digicert.com' `
            -HashAlgorithm 'sha256' `
            -Force
    } else {
        Write-Host "Skipping file $InputFile as it is already signed"
    }
}

# Sanity checks: only allow File or Directory
if ($File -eq "" -and $Directory -eq "") {
    Write-Host "-File or -Directory should be provided"
    exit 1
} elseif ($File -ne "" -and $Directory -ne "") {
    Write-Host "Only one of -File or -Directory should be provided"
    exit 1
}

$cleanupPfx = $false

# Check, if we need to retrieve PFX from environment
if($CertFile -eq "") {
    Write-Host "Trying to read certificate file from env:WINDOWS_CERTIFICATE"

    if(Test-Path "env:WINDOWS_CERTIFICATE") {
        $CertFile = "cert.pfx"

        [IO.File]::WriteAllBytes($CertFile, [System.Convert]::FromBase64String($env:WINDOWS_CERTIFICATE))

        $cleanupPfx = $true
    } else {
        Write-Host "No certificate is provided"
        exit 1
    }
}

# Check, if we need to retrieve password from environment
if($Password -eq "") {
    if (Test-Path "env:WINDOWS_CERTIFICATE_PASSWORD") {
        $Password = $env:WINDOWS_CERTIFICATE_PASSWORD
    }
}

$pfx = $null

# Retrieve the actual certificate
if($Password -ne "") {  
    $securePassword = ConvertTo-SecureString -String $Password -AsPlainText -Force
    $pfx = Get-PfxData -FilePath "$CertFile" -Password $securePassword
} else {
    $pfx = Get-PfxData -FilePath "$CertFile"
}

$pfxCert =  $pfx.EndEntityCertificates[0]

# Perform the code signing.
if ($File -ne "") {
    Set-FileSignature -InputFile $File -pfxCert $pfxCert
} else {
    Get-ChildItem `
        -Path "$Directory" `
        -Include *.dll,*.exe,*.msi `
        -Recurse `
        -File | ForEach-Object {
            Set-FileSignature -InputFile $_.FullName -pfxCert $pfxCert
        }
}

# Remove PFX file if it was created from environment.
if($cleanupPfx) {
    Remove-Item "${CertFile}"
}
