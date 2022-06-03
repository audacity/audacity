$audacityExecutable = $args[0]
$timeoutInSeconds = [int]$(if($args[1] -eq '--timeout') { $args[2] } else { $args[4] })
$journalFile = if($args[1] -eq '--timeout') { $args[4] } else { $args[2] }

Write-Host "Audacity Executable: $audacityExecutable"
Write-Host "Journal File: $journalFile"

if (-not(Test-Path -Path $journalFile -PathType Leaf)) {
    Write-Host "Journal file does not exist"
    exit 1
}

$process = [Diagnostics.Process]::Start("$audacityExecutable", "--journal $journalFile")

$completedInTime = $process.WaitForExit($timeoutInSeconds * 1000)

if (-not $completedInTime) {
    Write-Host "Timed out waiting for Audacity to finish"
    exit 1
}

Write-Host "Audacity finished in $(($process.ExitTime - $process.StartTime).TotalSeconds) seconds"
Write-Host "Exit code: $($process.ExitCode)"

if ($process.ExitCode -ne 0) {
   $logDir = "$env:APPDATA\audacity"
   Write-Host "Audacity exited with an error, gathering data from $logDir"

   if (Test-Path -Path "$logDir\journallog.txt" ) {
      Get-Content -Path "$logDir\journallog.txt" | Out-Default
   } else {
      Write-Host "No journallog.cfg file found"
   }

   if (Test-Path -Path "$logDir\lastlog.txt" ) {
      Get-Content -Path "$logDir\lastlog.txt" | Out-Default
   } else {
      Write-Host "No lastlog.txt file found"
   }

   if (Test-Path -Path "$logDir\audacity.cfg" ) {
      Get-Content -Path "$logDir\audacity.cfg" | Out-Default
   } else {
      Write-Host "No audacity.cfg file found"
   }

   exit 1
}
