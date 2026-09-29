@echo off

ECHO "Setup VS Environment"
SET VSWHERE="C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe"
FOR /f "usebackq tokens=*" %%i in (`%VSWHERE% -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do (
  SET VS_INSTALL_DIR=%%i
)
ECHO "VS_INSTALL_DIR: %VS_INSTALL_DIR%"
IF "%PROCESSOR_ARCHITECTURE%"=="ARM64" (
  REM Pin the VS 2022 toolset: the windows-11-arm runner image now ships VS 2026,
  REM whose MSVC rejects <experimental/coroutine> as included by C++/WinRT in C++17.
  CALL "%VS_INSTALL_DIR%\VC\Auxiliary\Build\vcvarsarm64.bat" -vcvars_ver=14.44
  IF ERRORLEVEL 1 EXIT /B 1
) ELSE (
  CALL "%VS_INSTALL_DIR%\VC\Auxiliary\Build\vcvars64.bat"
)

SET "QT_DIR=C:\build_tools\6.2.4"
SET "PATH=%QT_DIR%\msvc2019_64\bin;%PATH%"

SET HERE=%~dp0
cmake %* -P %HERE%/ci_build.cmake
