; Basic setup script for the Inno Setup installer builder.  For more
; information on the free installer builder, see www.jrsoftware.org.
;
; This script was contributed by Tim Peters.
; It was designed for Inno Setup 2.0.19 but works with later versions as well.

[Setup]
AppName=Expat
AppId=expat
AppVersion=2.2.1
AppVerName=Expat 2.2.1
AppCopyright=Copyright � 1998-2017 Thai Open Source Software Center, Clark Cooper, and the Expat maintainers
AppPublisher=The Expat Developers
AppPublisherURL=http://www.libexpat.org/
AppSupportURL=http://www.libexpat.org/
AppUpdatesURL=http://www.libexpat.org/
UninstallDisplayName=Expat XML Parser 2.2.1
VersionInfoVersion=2.2.1

DefaultDirName={pf}\Expat 2.2.1
UninstallFilesDir={app}\Uninstall

Compression=lzma
SolidCompression=yes
SourceDir=..
OutputDir=win32
DisableStartupPrompt=yes
AllowNoIcons=yes
DisableProgramGroupPage=yes
DisableReadyPage=yes

[Files]
Flags: ignoreversion; Source: win32\bin\Release\xmlwf.exe;  DestDir: "{app}\Bin"
Flags: ignoreversion; Source: win32\MANIFEST.txt;           DestDir: "{app}"
Flags: ignoreversion; Source: AUTHORS;                      DestDir: "{app}"; DestName: AUTHORS.txt
Flags: ignoreversion; Source: Changes;                      DestDir: "{app}"; DestName: Changes.txt
Flags: ignoreversion; Source: COPYING;                      DestDir: "{app}"; DestName: COPYING.txt
Flags: ignoreversion; Source: README;                       DestDir: "{app}"; DestName: README.txt
Flags: ignoreversion; Source: doc\*.html;                   DestDir: "{app}\Doc"
Flags: ignoreversion; Source: doc\*.css;                    DestDir: "{app}\Doc"
Flags: ignoreversion; Source: doc\*.png;                    DestDir: "{app}\Doc"
Flags: ignoreversion; Source: win32\bin\Release\*.dll;      DestDir: "{app}\Bin"
Flags: ignoreversion; Source: win32\bin\Release\*.lib;      DestDir: "{app}\Bin"
Flags: ignoreversion; Source: expat.sln;                    DestDir: "{app}\Source"
Flags: ignoreversion; Source: win32\README.txt;             DestDir: "{app}\Source"
Flags: ignoreversion; Source: lib\*.c;                      DestDir: "{app}\Source\lib"
Flags: ignoreversion; Source: lib\*.h;                      DestDir: "{app}\Source\lib"
Flags: ignoreversion; Source: lib\*.def;                    DestDir: "{app}\Source\lib"
Flags: ignoreversion; Source: lib\*.vcxproj;                DestDir: "{app}\Source\lib"
Flags: ignoreversion; Source: lib\*.vcxproj.filters;        DestDir: "{app}\Source\lib"
Flags: ignoreversion; Source: examples\*.c;                 DestDir: "{app}\Source\examples"
Flags: ignoreversion; Source: examples\*.vcxproj;           DestDir: "{app}\Source\examples"
Flags: ignoreversion; Source: examples\*.vcxproj.filters;   DestDir: "{app}\Source\examples"
Flags: ignoreversion; Source: tests\*.c;                    DestDir: "{app}\Source\tests"
Flags: ignoreversion; Source: tests\*.cpp;                  DestDir: "{app}\Source\tests"
Flags: ignoreversion; Source: tests\*.h;                    DestDir: "{app}\Source\tests"
Flags: ignoreversion; Source: tests\*.sln;                  DestDir: "{app}\Source\tests"
Flags: ignoreversion; Source: tests\*.vcxproj;              DestDir: "{app}\Source\tests"
Flags: ignoreversion; Source: tests\*.vcxproj.filters;      DestDir: "{app}\Source\tests"
Flags: ignoreversion; Source: tests\README.txt;             DestDir: "{app}\Source\tests"
Flags: ignoreversion; Source: tests\benchmark\*.c;          DestDir: "{app}\Source\tests\benchmark"
Flags: ignoreversion; Source: tests\benchmark\*.sln;        DestDir: "{app}\Source\tests\benchmark"
Flags: ignoreversion; Source: tests\benchmark\*.vcxproj;    DestDir: "{app}\Source\tests\benchmark"
Flags: ignoreversion; Source: tests\benchmark\README.txt;   DestDir: "{app}\Source\tests\benchmark"
Flags: ignoreversion; Source: xmlwf\*.c*;                   DestDir: "{app}\Source\xmlwf"
Flags: ignoreversion; Source: xmlwf\*.h;                    DestDir: "{app}\Source\xmlwf"
Flags: ignoreversion; Source: xmlwf\*.vcxproj;              DestDir: "{app}\Source\xmlwf"
Flags: ignoreversion; Source: xmlwf\*.vcxproj.filters;      DestDir: "{app}\Source\xmlwf"

[Messages]
WelcomeLabel1=Welcome to the Expat XML Parser Setup Wizard
WelcomeLabel2=This will install [name/ver] on your computer.%n%nExpat is an XML parser with a C-language API, and is primarily made available to allow developers to build applications which use XML using a portable API and fast implementation.%n%nIt is strongly recommended that you close all other applications you have running before continuing. This will help prevent any conflicts during the installation process.
