;   Audacity: A Digital Audio Editor
;   Audacity(R) is copyright (c) 1999-2015 Audacity Team.
;   License: GPL v2.  See License.txt.
;
;   audacity.iss
;   Vaughan Johnson, Leland Lucius, Martyn Shaw, Richard Ash, & others
;

; This requires that the ISS Preprocessor be installed
#define AppExe "..\win\release\audacity.exe" 
#define AppMajor ""
#define AppMinor ""
#define AppRev ""
#define AppBuild ""
#define FullVersion ParseVersion(AppExe, AppMajor, AppMinor, AppRev, AppBuild)
#define AppVersion Str(AppMajor) + "." + Str(AppMinor) + "." + Str(AppRev)
#define AppName GetStringFileInfo(AppExe, PRODUCT_NAME)

[UninstallRun]
; Uninstall prior installations.
Filename: "{app}\unins*.*"; 

[Setup]
; compiler-related directives
OutputBaseFilename=audacity-win-{#AppVersion}

WizardImageFile=audacity_InnoWizardImage.bmp
WizardSmallImageFile=audacity_InnoWizardSmallImage.bmp

SolidCompression=yes

; installer-related directives
AppName={#AppName}
AppVerName=Audacity {#AppVersion}
; Specify AppVersion as well, so it appears in the Add/Remove Programs entry. 
AppVersion={#AppVersion}
AppPublisher="Audacity Team"
AppPublisherURL=http://web.audacityteam.org
AppSupportURL=http://web.audacityteam.org
AppUpdatesURL=http://web.audacityteam.org
ChangesAssociations=yes

DefaultDirName={pf}\Audacity

VersionInfoProductName={#AppName}
VersionInfoProductTextVersion={#GetFileProductVersion(AppExe)}
VersionInfoDescription={#AppName + " " + AppVersion + " Setup"}
VersionInfoVersion={#GetFileVersion(AppExe)}
VersionInfoCopyright={#GetFileCopyright(AppExe)}

; Always warn if dir exists, because we'll overwrite previous Audacity.
DirExistsWarning=yes
DisableProgramGroupPage=yes
UninstallDisplayIcon="{app}\audacity.exe"

; No longer force them to accept the license, just display it.   LicenseFile=..\LICENSE.txt
InfoBeforeFile=audacity_InnoWizard_InfoBefore.rtf
InfoAfterFile=..\README.txt

; We no longer produce new ANSI builds. 
; As we use Inno Setup (u), the Unicode version, to build this script, 
; the MinVersion will automatically be set to what we need. 
; We no longer explicitly set it.
;   MinVersion=4.0,5.0

; cosmetic-related directives
SetupIconFile=audacity.ico

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "pt_BR"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "ca"; MessagesFile: "compiler:Languages\Catalan.isl"
Name: "co"; MessagesFile: "compiler:Languages\Corsican.isl"
Name: "cs"; MessagesFile: "compiler:Languages\Czech.isl"
Name: "da"; MessagesFile: "compiler:Languages\Danish.isl"
Name: "nl"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "fi"; MessagesFile: "compiler:Languages\Finnish.isl"
Name: "fr"; MessagesFile: "compiler:Languages\French.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"
Name: "el"; MessagesFile: "compiler:Languages\Greek.isl"
Name: "he"; MessagesFile: "compiler:Languages\Hebrew.isl"
Name: "hu"; MessagesFile: "compiler:Languages\Hungarian.isl"
Name: "it"; MessagesFile: "compiler:Languages\Italian.isl"
Name: "ja"; MessagesFile: "compiler:Languages\Japanese.isl"
Name: "ne"; MessagesFile: "compiler:Languages\Nepali.islu"
Name: "nb"; MessagesFile: "compiler:Languages\Norwegian.isl"
Name: "pl"; MessagesFile: "compiler:Languages\Polish.isl"
Name: "pt"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"
Name: "sr_RS"; MessagesFile: "compiler:Languages\SerbianCyrillic.isl"
; "0" will be translated to "@" when read by Audacity.
Name: "sr_RS0latin"; MessagesFile: "compiler:Languages\SerbianLatin.isl"
Name: "sl"; MessagesFile: "compiler:Languages\Slovenian.isl"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "tr"; MessagesFile: "compiler:Languages\Turkish.isl"
Name: "uk"; MessagesFile: "compiler:Languages\Ukrainian.isl"

; Additional Inno Setup translations can be downloaded from:
;
; http://www.jrsoftware.org/files/istrans/
;
; If you find one that will work, add it to the win/InnoSetupLanguages directory.
; The filename must be the locale name and the ".isl" extension.  For example, "af.isl"
; would have the "Afrikaans" translation.
;
; Add any additional languages from the win/InnoSetupLanguages directory
;
; Based on the examples from the ISS Preprocessor manual
;
#define FindHandle
#define FindResult

#sub AddLanguage
  #define FileName FindGetFileName(FindHandle)
  #define LangCode Local[0] = Copy(FileName, 1, Pos(".", FileName) - 1)
  Name: {#LangCode}; MessagesFile: "InnoSetupLanguages\{#FileName}"
#endsub

#for {FindHandle = FindResult = FindFirst("InnoSetupLanguages\*.isl", 0); FindResult; FindResult = FindNext(FindHandle)} AddLanguage
#if FindHandle
  #expr FindClose(FindHandle)
#endif

[INI]
Filename: "{app}\FirstTime.ini"; Section: "FromInno"; Key: "ResetPrefs"; String: "1"; Tasks: resetPrefs;
Filename: "{app}\FirstTime.ini"; Section: "FromInno"; Key: "Language"; String: "{language}"

[Tasks]
Name: desktopicon; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: resetPrefs; Description:  "{cm:ResetPrefs}"; Flags: unchecked
; No longer allow user to choose whether to associate AUP file type with Audacity.
; Name: associate_aup; Description: "&Associate Audacity project files"; GroupDescription: "Other tasks:"; Flags: checkedonce

[Files]
; Don't display in separate window, rather as InfoAfterFile.   Source: "..\README.txt"; DestDir: "{app}"; Flags: ignoreversion isreadme
Source: "..\README.txt"; DestDir: "{app}"; Flags: ignoreversion

Source: "..\LICENSE.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#AppExe}"; DestDir: "{app}"; Flags: ignoreversion

; Manual, which should be got from the manual wiki using ..\scripts\mw2html_audacity\wiki2htm.bat
Source: "..\help\manual\*"; DestDir: "{app}\help\manual\"; Flags: ignoreversion recursesubdirs

Source: "..\presets\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs

; wxWidgets DLLs. Be specific (not *.dll) so we don't accidentally distribute avformat.dll, for example.
; Don't use the WXWIN environment variable, because...
; 1) Can't get the documented {%WXWIN|default dir} parsing to work.
; 2) Need the DLL's in the release dir for testing, anyway.
Source: "..\win\release\wxbase28u_net_vc_custom.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\win\release\wxbase28u_xml_vc_custom.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\win\release\wxbase28u_vc_custom.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\win\release\wxmsw28u_adv_vc_custom.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\win\release\wxmsw28u_core_vc_custom.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\win\release\wxmsw28u_html_vc_custom.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\win\release\wxmsw28u_qa_vc_custom.dll"; DestDir: "{app}"; Flags: ignoreversion

; MSVC runtime DLLs. Some users can't put these in the system dir, so just put them in the EXE dir.
; It's legal, per http://www.fsf.org/licensing/licenses/gpl-faq.html#WindowsRuntimeAndGPL .
; This is not an ideal solution, but should need the least tech support.
; We'll know we have the right version, don't step on anybody else's older version, and
; it's easy to make the zip (and they match better).
; Copy the two required DLL's from 
; "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\redist\x86\Microsoft.VC120.CRT\"
; or "C:\Program Files\Microsoft Visual Studio 12.0\VC\redist\x86\Microsoft.VC120.CRT\"
; according to your system 
Source: "..\win\release\msvcp120.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\win\release\msvcr120.dll"; DestDir: "{app}"; Flags: ignoreversion

Source: "..\win\release\languages\*"; DestDir: "{app}\Languages\"; Flags: ignoreversion recursesubdirs
; We don't currently ship any modules, so the next line is commented out
; Source: "..\win\release\modules\*"; DestDir: "{app}\Modules\"; Flags: ignoreversion recursesubdirs skipifsourcedoesntexist
Source: "..\win\release\nyquist\*"; DestDir: "{app}\Nyquist\"; Flags: ignoreversion recursesubdirs
Source: "..\win\release\plug-ins\*"; DestDir: "{app}\Plug-Ins\"; Flags: ignoreversion

[Icons]
Name: "{commonprograms}\Audacity"; Filename: "{app}\audacity.exe"
Name: "{commondesktop}\Audacity"; Filename: "{app}\audacity.exe"; Tasks: desktopicon

[InstallDelete]
; Get rid of previous 'first time' file, in case somebody want to reinstall without the reset option after they installed with it
Type: files; Name: "{app}\FirstTime.txt" 

; Get rid of Audacity 1.0.0 stuff that's no longer used.
Type: files; Name: "{app}\audacity-help.htb"
Type: files; Name: "{app}\audacity-1.2-help.htb"

; Get rid of previous versions of MSVC runtimes.
Type: files; Name: "{app}\Microsoft.VC80.CRT.manifest"
Type: files; Name: "{app}\Microsoft.VC90.CRT.manifest"
Type: files; Name: "{app}\msvcp80.dll"
Type: files; Name: "{app}\msvcr80.dll"
Type: files; Name: "{app}\msvcp90.dll"
Type: files; Name: "{app}\msvcr90.dll"

; Get rid of previous help folder.
Type: filesandordirs; Name: "{app}\help"

; Don't want to do this because user may have stored their own.
;   Type: filesandordirs; Name: "{app}\vst"

; We've switched from a folder in the start menu to just the Audacity.exe at the top level.
; Get rid of 1.0.0 folder and its icons.
Type: files; Name: "{commonprograms}\Audacity\audacity.exe"
Type: files; Name: "{commonprograms}\Audacity\unins000.exe"
Type: dirifempty; Name: "{commonprograms}\Audacity"

;Get rid of previous uninstall item
Type: files; Name: "{app}\unins*.*"

; Get rid of no longer used test.lsp.
Type: files; Name: "{app}\Nyquist\test.lsp"

; Get rid of specific LADSPA plug-ins that we now ship with different names.
Type: files; Name: "{app}\Plug-Ins\GVerb.dll"
Type: files; Name: "{app}\Plug-Ins\Hard Limiter.dll"
Type: files; Name: "{app}\Plug-Ins\sc4.dll"

;Get rid of any modules that we have ever installed
Type: files; Name: "{app}\Modules\mod-script-pipe.dll"
Type: files; Name: "{app}\Modules\mod-script-pipe.exp"
Type: files; Name: "{app}\Modules\mod-script-pipe.lib"

;get rid of the Modules dir, if it is empty
Type: dirifempty; Name: "{app}\Modules"

; Get rid of gverb that we no longer ship
Type: files; Name: "{app}\Plug-Ins\gverb_1216.dll"

; Get rid of old crossfade* plugins that we no longer ship
Type: files; Name: "{app}\Plug-Ins\crossfadein.ny"
Type: files; Name: "{app}\Plug-Ins\crossfadeout.ny"
                                            
[Registry]
; No longer allow user to choose whether to associate AUP file type with Audacity.
; Leaving this one commented out example of the old way.
; Root: HKCR; Subkey: ".AUP"; ValueType: string; ValueData: "Audacity.Project"; Flags: createvalueifdoesntexist uninsdeletekey; Tasks: associate_aup
Root: HKCR; Subkey: ".AUP"; ValueType: string; ValueData: "Audacity.Project"; Flags: createvalueifdoesntexist uninsdeletekey;
Root: HKCR; Subkey: "Audacity.Project\OpenWithList\audacity.exe"; Flags: createvalueifdoesntexist uninsdeletekey;
Root: HKCR; Subkey: "Audacity.Project"; ValueType: string; ValueData: "Audacity Project File"; Flags: createvalueifdoesntexist uninsdeletekey;
Root: HKCR; Subkey: "Audacity.Project\shell"; ValueType: string; ValueData: ""; Flags: createvalueifdoesntexist uninsdeletekey;
Root: HKCR; Subkey: "Audacity.Project\shell\open"; Flags: createvalueifdoesntexist uninsdeletekey;
Root: HKCR; Subkey: "Audacity.Project\shell\open\command"; ValueType: string; ValueData: """{app}\audacity.exe"" ""%1"""; Flags: createvalueifdoesntexist uninsdeletekey;

;The following would allow a following 'help' installer to know where to put the 'help' files.
;Root: HKCR; Subkey: "Audacity.Project\Path";  ValueType: string; ValueData: {app}; Flags: createvalueifdoesntexist uninsdeletekey;

[Run]
Filename: "{app}\audacity.exe"; Description: "Launch Audacity"; Flags: nowait postinstall skipifsilent

[CustomMessages]
en.ResetPrefs=Reset Preferences
es.ResetPrefs=¿Desea restablecer las preferencias?
fr.ResetPrefs=Réinitialiser les  Préférences
