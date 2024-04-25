@echo off

ECHO "Setup VS Environment"
SET VSWHERE="C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe"
FOR /f "usebackq tokens=*" %%i in (`%VSWHERE% -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do (
  SET VS_INSTALL_DIR=%%i
)
ECHO "VS_INSTALL_DIR: %VS_INSTALL_DIR%"
CALL "%VS_INSTALL_DIR%\VC\Auxiliary\Build\vcvars64.bat"

SET "QT_DIR=C:\build_tools\6.2.4"
SET "PATH=%QT_DIR%\msvc2019_64\bin;%PATH%"

SET HERE=%~dp0
cmake %* -P %HERE%/ci_build.cmake
