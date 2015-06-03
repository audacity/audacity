; *** Inno Setup version 5.5.3+ Catalan (Valencian) messages ***
;
; Note: This Valencian language file is based on the Catalan one.
;
; Translated by Pau Sellés i Garcia (pau.selles@softvalencia.org) Softvalencià Translators Team
;

[LangOptions]

LanguageName=Catal<00E0> (valenci<00E0>)
LanguageID=$0803
LanguageCodePage=1252

[Messages]

; *** Application titles
SetupAppTitle=Instal·lació
SetupWindowTitle=Instal·lació - %1
UninstallAppTitle=Desinstal·lació
UninstallAppFullTitle=Desinstal·lació - %1

; *** Misc. common
InformationTitle=Informació
ConfirmTitle=Confirmació
ErrorTitle=Error

; *** SetupLdr messages
SetupLdrStartupMessage=Este programa instal·larà l'aplicació %1. Voleu continuar?
LdrCannotCreateTemp=No s'ha pogut crear un fitxer temporal i per això la instal·lació es cancel·larà
LdrCannotExecTemp=No s'ha pogut executar un fitxer a la carpeta temporal i per això la instal·lació es cancel·larà

; *** Startup error messages
LastErrorMessage=%1.%n%nError %2: %3
SetupFileMissing=El fitxer %1 no es troba a la carpeta d'instal·lació. Resoleu el problema o obteniu una còpia nova de l'aplicació.
SetupFileCorrupt=Els fitxers d'instal·lació estan malmesos. Heu d'obtindre una còpia nova de l'aplicació.
SetupFileCorruptOrWrongVer=Els fitxers d'instal·lació estan malmesos, o són incompatibles amb esta versió de l'instal·lador. Resoleu el problema manualment o obteniu una còpia nova de l'instal·lador.
InvalidParameter=S'ha passat un paràmetre no vàlid a la línia d'ordes:%n%n%1
SetupAlreadyRunning=La instal·lació ja és en curs.
WindowsVersionNotSupported=Esta aplicació no és compatible amb la versió del Windows instal·lada a l'ordinador.
WindowsServicePackRequired=Esta aplicació necessita el %1 Service Pack %2 o posterior.
NotOnThisPlatform=Esta aplicació no pot funcionar en %1.
OnlyOnThisPlatform=Esta aplicació només funcionarà en %1.
OnlyOnTheseArchitectures=Esta aplicació només es pot instal·lar en versions de Windows dissenyades per a les següents arquitectures de processador:%n%n%1
MissingWOW64APIs=Esta versió de Windows no conté la funcionalitat necessària per a realitzar una instal·lació de 64 bits. Per tal de corregir este problema instal·leu el Service Pack %1.
WinVersionTooLowError=Esta aplicació requereix una versió %2 o posterior de %1.
WinVersionTooHighError=Esta aplicació no es pot instal·lar en %1 versió %2 o posterior.
AdminPrivilegesRequired=Per poder instal·lar esta aplicació cal tindre privilegis d'administrador.
PowerUserPrivilegesRequired=Per poder instal·lar esta aplicació cal ser usuari administrador o bé membre del grup d'usuaris Power Users.
SetupAppRunningError=L'instal·lador ha detectat que l'aplicació %1 s'està executant actualment.%n%nTanqueu l'aplicació i feu clic a «Avant» per continuar o «Cancel·la» per eixir.
UninstallAppRunningError=L'instal·lador ha detectat que l'aplicació %1 s'està executant actualment.%n%nTanqueu l'aplicació i feu clic a «Avant» per continuar o «Cancel·la» per eixir.

; *** Misc. errors
ErrorCreatingDir=El programa d'instal·lació no ha pogut crear la carpeta «%1»
ErrorTooManyFilesInDir=No s'ha pogut crear un fitxer a la carpeta «%1» perquè conté massa fitxers

; *** Setup common messages
ExitSetupTitle=Eixida de la instal·lació
ExitSetupMessage=La instal·lació encara no ha finalizat. Si eixiu ara, la instal·lació es cancel·larà.%n%nTot i així, podeu tornar a executar este instal·lador més tard per completar la instal·lació.%n%nVoleu eixir de la instal·lació?
AboutSetupMenuItem=&Quant a la instal·lació...
AboutSetupTitle=Quant a la instal·lació
AboutSetupMessage=%1 versió %2%n%3%n%nPàgina web de %1:%n%4
AboutSetupNote=
TranslatorNote=Catalan translation by Pau Sellés (pau.selles@softvalencia.org)

; *** Buttons
ButtonBack=< &Arrere
ButtonNext=&Avant >
ButtonInstall=&Instal·la
ButtonOK=D'acord
ButtonCancel=Cancel·la
ButtonYes=&Sí
ButtonYesToAll=Sí a &tot
ButtonNo=&No
ButtonNoToAll=N&o a tot
ButtonFinish=&Finalitza
ButtonBrowse=&Navega...
ButtonWizardBrowse=&Navega...
ButtonNewFolder=Crea una carpeta &nova

; *** "Select Language" dialog messages
SelectLanguageTitle=Selecció de la llengua
SelectLanguageLabel=Seleccioneu la llengua que preferiu durant la instal·lació:

; *** Common wizard text
ClickNext=Feu clic «Avant» per continuar o «Cancel·la» per abandonar la instal·lació.
BeveledLabel=
BrowseDialogTitle=Selecció de carpeta
BrowseDialogLabel=Seleccioneu la carpeta de destinació i feu clic a «D'acord».
NewFolderName=Carpeta nova

; *** "Welcome" wizard page
WelcomeLabel1=Vos donem la benvinguda a l'auxiliar d'instal·lació de l'aplicació [name]
WelcomeLabel2=Este programa instal·larà l'aplicació [name/ver] a l'ordinador.%n%nÉs molt recomanable que abans de continuar tanqueu tots els altres programes oberts, per tal d'evitar conflictes durant el procés d'instal·lació.

; *** "Password" wizard page
WizardPassword=Contrasenya
PasswordLabel1=Esta instal·lació està protegida amb una contrasenya.
PasswordLabel3=Introduïu la contrasenya i feu clic a «Avant» per continuar. Esta contrasenya distingeix entre majúscules i minúscules.
PasswordEditLabel=&Contrasenya:
IncorrectPassword=La contrasenya introduïda no és correcta. Torneu-ho a intentar.

; *** "License Agreement" wizard page
WizardLicense=Acceptació de la llicencia d'ús
LicenseLabel=Cal que llegiu i accepteu la llicència d'ús abans de continuar.
LicenseLabel3=La llicència d'ús especifica amb quins drets i deures està subjecte l'aplicació que voleu instal·lar. Cal que n'accepteu els termes abans de continuar la instal·lació.
LicenseAccepted=&Accepte l'acord
LicenseNotAccepted=&No accepte l'acord

; *** "Information" wizard pages
WizardInfoBefore=Informació
InfoBeforeLabel=Llegiu la informació següent abans de continuar.
InfoBeforeClickLabel=Quan estigueu preparat per continuar, feu clic a «Avant».
WizardInfoAfter=Informació
InfoAfterLabel=Llegiu la informació següent abans de continuar.
InfoAfterClickLabel=Quan estigueu preparat per continuar, feu clic a «Avant».

; *** "User Information" wizard page
WizardUserInfo=Informació de l'usuari
UserInfoDesc=Introduïu la vostra informació.
UserInfoName=&Nom de l'usuari:
UserInfoOrg=&Organització:
UserInfoSerial=&Número de sèrie:
UserInfoNameRequired=Cal introduir un nom.

; *** "Select Destination Location" wizard page
WizardSelectDir=Seleccioneu una carpeta de destinació
SelectDirDesc=On voleu instal·lar l'aplicació [name]?
SelectDirLabel3=El programa d'instal·lació instal·larà l'aplicació [name] a la carpeta següent.
SelectDirBrowseLabel=Per continuar, feu clic a «Avant». Si desitgeu seleccionar una altra carpeta, feu clic a «Navega».
DiskSpaceMBLabel=Este programa necessita un mínim de [mb] MB d'espai lliure al disc.
CannotInstallToNetworkDrive=La instal·lació no es pot fer a un disc de xarxa.
CannotInstallToUNCPath=La instal·lació no es pot fer a un camí UNC.
InvalidPath=Cal donar un camí complet amb lletra d'unitat, per exemple:%n%nC:\Aplicació%n%no bé un camí UNC en la forma:%n%n\\servidor\compartit
InvalidDrive=El disc o camí de xarxa seleccionat no existeix, trieu-ne un altre.
DiskSpaceWarningTitle=No hi ha prou espai al disc
DiskSpaceWarning=El programa d'instal·lació necessita com a mínim %1 KB d'espai lliure, però el disc seleccionat només té %2 KB disponibles.%n%nTot i amb això, desitgeu continuar?
DirNameTooLong=El nom de la carpeta o del camí és massa llarg.
InvalidDirName=El nom de la carpeta no és vàlid.
BadDirName32=Un nom de carpeta no pot contindre cap dels caràcters següents:%n%n%1
DirExistsTitle=La carpeta existeix
DirExists=La carpeta:%n%n%1%n%nja existeix. Voleu continuar i instal·lar l'aplicació en esta carpeta?
DirDoesntExistTitle=La carpeta no existeix
DirDoesntExist=La carpeta:%n%n%1%n%nno existeix. Voleu crear-la?

; *** "Select Program Group" wizard page
WizardSelectComponents=Selecció de components
SelectComponentsDesc=Quins components voleu instal·lar?
SelectComponentsLabel2=Seleccioneu els components que voleu instal·lar; elimineu els components que no voleu instal·lar. Feu clic a «Avant» per continuar.
FullInstallation=Instal·lació completa
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instal·lació compacta
CustomInstallation=Instal·lació personalitzada
NoUninstallWarningTitle=Els components existeixen
NoUninstallWarning=L'auxiliar d'instal·lació ha detectat que els components següents ja es troben a l'ordinador:%n%n%1%n%nSi no estan seleccionats no es desinstal·laran.%n%nVoleu continuar igualment?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Esta selecció requereix un mínim de [mb] MB d'espai al disc.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Trieu tasques addicionals
SelectTasksDesc=Quines tasques addicionals cal executar?
SelectTasksLabel2=Trieu les tasques addicionals que voleu que siguen executades mentre s'instal·la l'apliació [name], i després feu clic a «Avant».

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Trieu la carpeta del menú d'inici
SelectStartMenuFolderDesc=On cal situar els enllaços del programa?
SelectStartMenuFolderLabel3=El programa d'instal·lació crearà l'accés directe al programa a la carpeta següent del menú d'inici.
SelectStartMenuFolderBrowseLabel=Per continuar, feu clic a «Avant». Si desitgeu triar una altra carpeta, feu clic a «Navega...».
MustEnterGroupName=Cal introduir un nom de carpeta.
GroupNameTooLong=El nom de la carpeta o del camí és massa llarg.
InvalidGroupName=El nom de la carpeta no és vàlid.
BadGroupName=El nom del grup no pot contindre cap dels caràcters següents:%n%n%1
NoProgramGroupCheck2=&No crees una carpeta al menú d'inici

; *** "Ready to Install" wizard page
WizardReady=Preparat per a instal·lar
ReadyLabel1=El programa d'instal·lació està preparat per a iniciar la instal·lació de l'aplicació [name] a l'ordinador.
ReadyLabel2a=Feu clic a «Instal·la» per continuar amb la instal·lació, o «Arrere» si voleu revisar o modificar les opcions d'instal·lació.
ReadyLabel2b=Feu clic a «Instal·la» per continuar amb la instal·lació.
ReadyMemoUserInfo=Informació de l'usuari:
ReadyMemoDir=Carpeta de destinació:
ReadyMemoType=Tipus d'instal·lació:
ReadyMemoComponents=Components seleccionats:
ReadyMemoGroup=Carpeta del Menú Inici:
ReadyMemoTasks=Tasques addicionals:

; *** "Preparing to Install" wizard page
WizardPreparing=S'està preparant la instal·lació
PreparingDesc=S'està preparant la instal·lació de l'aplicació [name] a l'ordinador.
PreviousInstallNotCompleted=La instal·lació o desinstal·lació anterior no s'ha dut a terme. Caldrà que reinicieu l'ordinador per a finalitzar esta instal·lació.%n%nDesprés de reiniciar l'ordinador, executeu este programa de nou per completar la instal·lació de l'aplicació [name].
CannotContinue=La instal·lació no pot continuar. Feu clic a «Cancel·la» per a eixir.
ApplicationsFound=Les següents aplicacions estan fent servir fitxers que necessiten ser actualitzats per la instal·lació. Es recomana que permeteu a la instal·lació tancar automàticament estes aplicacions.
ApplicationsFound2=Les següents aplicacions estan fent servir fitxers que necessiten ser actualitzats per la instal·lació. Es recomana que permeteu a la instal·lació tancar automàticament estes aplicacions. Després de completar la instal·lació s'intentarà reiniciar les aplicacions.
CloseApplications=&Tanca automàticament les aplicacions
DontCloseApplications=&No tanques les aplicacions
ErrorCloseApplications=El programa d'instal·lació no ha pogut tancar automàticament totes les aplicacions. Es recomana que abans de continuar tanqueu totes les aplicacions que estan usant fitxers que han de ser actualitzats pel programa d'instal·lació.

; *** "Installing" wizard page
WizardInstalling=S'està instal·lant
InstallingLabel=Espereu mentre s'instal·la l'aplicació [name] a l'ordinador.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=S'està finalitzant la instal·lació de l'aplicació [name]
FinishedLabelNoIcons=La insal·lació de l'aplicació [name] a l'ordinador.
FinishedLabel=La instal·lació de l'aplicació [name] a l'ordinador ha finalitzat correctament. Feu clic a qualsevol de les icones creades per a iniciar l'aplicació.
ClickFinish=Feu clic a «Finalitza» per a eixir de la instal·lació.
FinishedRestartLabel=Per completar la instal·lació de [name] cal reiniciar l'ordinador. Voleu fer-ho ara?
FinishedRestartMessage=Per completar la instal·lació de l'aplicació [name] cal reiniciar l'ordinador. Voleu fer-ho ara?
ShowReadmeCheck=Sí, vull llegir el fitxer LLEGIU-ME.TXT
YesRadio=&Sí, reinicia l'ordinador ara
NoRadio=&No, reiniciaré l'ordinador més tard
; used for example as 'Run MyProg.exe'
RunEntryExec=Executa %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Mostra %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=L'instal·lador necessita el disc següent
SelectDiskLabel2=Introduïu el disc %1 i feu clic a «Continua».%n%nSi els fitxers d'este disc indicat es troben en una carpeta diferent, introduïu-ne la ubicació o bé feu clic a «Navega...».
PathLabel=&Ubicació:
FileNotInDir2=El fitxer «%1» no s'ha trobat en «%2». Introduïu el disc correcte o escolliu una altra carpeta.
SelectDirectoryLabel=Indiqueu on es troba el disc següent.

; *** Installation phase messages
SetupAborted=La instal·lació no ha finalitzat correctament.%n%n%Resoleu el problema i executeu de nou el programa d'instal·lació.
EntryAbortRetryIgnore=Feu clic a «Reintenta» per a intentar-ho de nou, «Ignora» per a continuar igualment, o «Abandona» per a abandonar la instal·lació.

; *** Installation status messages
StatusClosingApplications=S'estan tancant les aplicacions...
StatusCreateDirs=S'estan creant les carpetes...
StatusExtractFiles=S'estan extraient els fitxers...
StatusCreateIcons=S'estan creant les dreceres de l'aplicació...
StatusCreateIniEntries=S'està modificant el fitxer INI...
StatusCreateRegistryEntries=S'està configurant el registre del sistema...
StatusRegisterFiles=S'estan registrant els fitxers...
StatusSavingUninstall=S'està guardant la informació de desinstal·lació...
StatusRunProgram=S'està finalitzant la instal·lació...
StatusRestartingApplications=S'estan reiniciant les aplicacions...
StatusRollback=S'estan desfent els canvis...

; *** Misc. errors
ErrorInternal2=Error intern: %1
ErrorFunctionFailedNoCode=%1 ha fallat
ErrorFunctionFailed=%1 ha fallat; codi %2
ErrorFunctionFailedWithMessage=%1 ha fallat; codi %2.%n%3
ErrorExecutingProgram=No es pot executar el fitxer:%n%1

; *** Registry errors
ErrorRegOpenKey=S'ha produït un error en obrir la clau de registre:%n%1\%2
ErrorRegCreateKey=S'ha produït un error en crear la clau de registre:%n%1\%2
ErrorRegWriteKey=S'ha produït un error en escriure a la clau de registre:%n%1\%2

; *** INI errors
ErrorIniEntry=S'ha produït un error en crear l'entrada INI al fitxer «%1».

; *** File copying errors
FileAbortRetryIgnore=Feu clic a «Reintenta» per a intentar-ho de nou, «Ignora» per a saltar-se este fitxer (no recomanat), o «Abandona» per a abandonar la instal·lació.
FileAbortRetryIgnore2=Feu clic a «Reintenta» per a intentar-ho de nou, «Ignora» per a continuar igualment (no recomanat), o «Abandona» per a abandonar la instal·lació.
SourceIsCorrupted=El fitxer d'origen està malmés
SourceDoesntExist=El fitxer d'origen «%1» no existeix
ExistingFileReadOnly=El fitxer és de només lectura.%n%nFeu clic a «Reintenta» per a traure'n l'atribut de només lectura i tornar-ho a intentar, «Ignora» per a saltar-se'l (no recomanat), o «Abandona» per a abandonar la instal·lació.
ErrorReadingExistingDest=S'ha produït un error en llegir el fitxer:
FileExists=El fitxer ja existeix.%n%nVoleu sobreescriure'l?
ExistingFileNewer=El fitxer existent és més nou que el que s'intenta instal·lar. Es recomana mantindre el fitxer existent.%n%nVoleu mantindre'l?
ErrorChangingAttr=S'ha produït un error en canviar els atributs del fitxer:
ErrorCreatingTemp=S'ha produït un error en crear un fitxer a la carpeta de destinació:
ErrorReadingSource=S'ha produït un error en llegir el fitxer d'origen:
ErrorCopying=S'ha produït un error en copiar un fitxer:
ErrorReplacingExistingFile=S'ha produït un error en reemplaçar el fitxer existent:
ErrorRestartReplace=Ha fallat reemplaçar:
ErrorRenamingTemp=S'ha produït un error en canviar el nom d'un fitxer a la carpeta de destinació:
ErrorRegisterServer=No s'ha pogut registrar el DLL/OCX: %1
ErrorRegSvr32Failed=Ha fallat RegSvr32 amb el codi de eixida %1
ErrorRegisterTypeLib=No s'ha pogut registrar la biblioteca de tipus: %1

; *** Post-installation errors
ErrorOpeningReadme=S'ha produït un error en obrir el fitxer LLEGIUME.TXT.
ErrorRestartingComputer=El programa d'instal·lació no ha pogut reiniciar l'ordinador. Cal fer-ho manualment.

; *** Uninstaller messages
UninstallNotFound=El fitxer «%1» no existeix. No es pot desinstal·lar.
UninstallOpenError=El fitxer «%1» no pot ser obert. No es pot desinstal·lar
UninstallUnsupportedVer=El fitxer de desinstal·lació «%1» està en un format no reconegut per esta versió del desinstal·lador. No es pot desinstal·lar
UninstallUnknownEntry=S'ha trobat una entrada desconeguda (%1) al fitxer de desinstal·lació.
ConfirmUninstall=Esteu segur de voler eliminar completament %1 i tots els seus components?
UninstallOnlyOnWin64=Este programa només es pot desinstal·lar en Windows de 64 bits.
OnlyAdminCanUninstall=Este programa només es pot desinstal·lar per un usuari amb privilegis d'administrador.
UninstallStatusLabel=Espereu mentre s'elimina l'aplicació %1 de l'ordinador.
UninstalledAll=L'aplicació %1 s'ha desinstal·lat correctament de l'ordinador.
UninstalledMost=L'aplicació %1 s'ha desinstal·lat.%n%nAlguns elements no s'han pogut eliminar. Poden ser eliminats manualment.
UninstalledAndNeedsRestart=Per completar la instal·lació de %1, cal reiniciar l'ordinador.%n%nVoleu fer-ho ara?
UninstallDataCorrupted=El fitxer «%1» està malmés. No es pot desinstal·lar.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Desinstal·lació de fitxers compartits
ConfirmDeleteSharedFile2=El sistema indica que el fitxer compartit següent ja no s'utilitza per cap altre programa. Voleu suprimir este fitxer?%n%nSi algun programa encara el fa servir i és eliminat, podria no funcionar correctament. Si no n'esteu segur, trieu «No». Deixar el fitxer al sistema no farà cap mal.
SharedFileNameLabel=Nom del fitxer:
SharedFileLocationLabel=Ubicació:
WizardUninstalling=Estat de la desinstal·lació
StatusUninstalling=S'està desinstal·lant: %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=S'està instal·lant %1.
ShutdownBlockReasonUninstallingApp=S'està desinstal·lant %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versió %2
AdditionalIcons=Icones addicionals:
CreateDesktopIcon=Crea una drecera a l'&escriptori
CreateQuickLaunchIcon=Crea una drecera a la &barra d'accés ràpid
ProgramOnTheWeb=%1 a la Web
UninstallProgram=Desinstal·la %1
LaunchProgram=Executa %1
AssocFileExtension=&Associa %1 amb l'extensió de fitxer %2
AssocingFileExtension=S'està associant %1 amb l'extensió de fitxer %2...
AutoStartProgramGroupDescription=Inici:
AutoStartProgram=Inicia automàticament l'aplicació %1
AddonHostProgramNotFound=No s'ha pogut trobar l'aplicació %1 a la carpeta seleccionada.%n%nVoleu continuar igualment?