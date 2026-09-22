<#

.SYNOPSIS
A simple manager for third-party source code dependencies.
Run "repoint help" for more documentation.

#>

Set-StrictMode -Version 2.0
$ErrorActionPreference = "Stop"
$env:HGPLAIN = "true"

$sml = $env:REPOINT_SML

$mydir = Split-Path $MyInvocation.MyCommand.Path -Parent
$program = "$mydir/repoint.sml"

# We need either Poly/ML or SML/NJ. No great preference as to which.

# Typical locations
$env:PATH = "$env:PATH;C:\Program Files (x86)\SMLNJ\bin;C:\Program Files\Poly ML;C:\Program Files (x86)\Poly ML"

if (!$sml) {
    if (Get-Command "sml" -ErrorAction SilentlyContinue) {
       $sml = "smlnj"
    } elseif (Get-Command "polyml" -ErrorAction SilentlyContinue) {
       $sml = "poly"
    } else {
       echo @"

ERROR: No supported SML compiler or interpreter found       

  The Repoint external source code manager needs a Standard ML (SML)
  compiler or interpreter to run.

  Please ensure you have one of the following SML implementations
  installed and present in your PATH, and try again.

    1. Standard ML of New Jersey
       - executable name: sml

    2. Poly/ML
       - executable name: polyml

"@
       exit 1
    }
}

if ($args -match "'""") {
    $arglist = '["usage"]'
} else {
    $arglist = '["' + ($args -join '","') + '"]'
}

if ($sml -eq "poly") {

    $program = $program -replace "\\","\\\\"
    echo "use ""$program""; repoint $arglist" | polyml -q --error-exit | Out-Host

    if (-not $?) {
        exit $LastExitCode
    }

} elseif ($sml -eq "smlnj") {

    $lines = @(Get-Content $program)
    $lines = $lines -notmatch "val _ = main ()"

    $intro = @"
val smlrun__cp = 
    let val x = !Control.Print.out in
        Control.Print.out := { say = fn _ => (), flush = fn () => () };
        x
    end;
val smlrun__prev = ref "";
Control.Print.out := { 
    say = fn s => 
        (if String.isSubstring "Error" s orelse String.isSubstring "Fail" s
         then (Control.Print.out := smlrun__cp;
               (#say smlrun__cp) (!smlrun__prev);
               (#say smlrun__cp) s)
         else (smlrun__prev := s; ())),
    flush = fn s => ()
};
"@ -split "[\r\n]+"

    $outro = @"
val _ = repoint $arglist;
val _ = OS.Process.exit (OS.Process.success);
"@ -split "[\r\n]+"

    $script = @()
    $script += $intro
    $script += $lines
    $script += $outro

    $tmpfile = ([System.IO.Path]::GetTempFileName()) -replace "[.]tmp",".sml"

    $script | Out-File -Encoding "ASCII" $tmpfile

    $env:CM_VERBOSE="false"

    sml $tmpfile

    if (-not $?) {
        del $tmpfile
        exit $LastExitCode
    }

    del $tmpfile

} else {

    "Unknown SML implementation name: $sml"
    exit 2
}
