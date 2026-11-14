@echo off

IF NOT EXIST build mkdir build

cd build

REM Detect the highest available Visual Studio version
for /f "tokens=*" %%i in ('"C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe" -latest -products * -requires Microsoft.Component.MSBuild -property installationVersion') do set VS_VERSION=%%i

REM The `VS_VERSION` will be something like 17.11.35312.102 ; keep only the major version
set VS_VERSION=%VS_VERSION:~0,2%

if %VS_VERSION%==16 (
    set GENERATOR="Visual Studio 16 2019"
) else if %VS_VERSION%==17 (
    set GENERATOR="Visual Studio 17 2022"
) else (
    echo No supported Visual Studio version found.
    exit /b 1
)

echo Using %GENERATOR%

cmake -DCMAKE_INSTALL_PREFIX=install -DDEVELOPER=TRUE -G%GENERATOR% ..

cmake --build . --config Debug --target install

IF %ERRORLEVEL% EQU 0 (
    echo Build successfull. audacity.sln was generated in .\build
) ELSE (
    echo Build failed.
)

pause
