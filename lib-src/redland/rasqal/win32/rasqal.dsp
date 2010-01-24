# Microsoft Developer Studio Project File - Name="rasqal" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=rasqal - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "rasqal.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "rasqal.mak" CFG="rasqal - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "rasqal - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "rasqal - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "rasqal - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "RASQAL_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "RASQAL_EXPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386

!ELSEIF  "$(CFG)" == "rasqal - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "c:\bin\Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "RASQAL_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I ".." /I "..\..\raptor-1.3.3" /I "..\..\redland-0.9.19\librdf" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "RASQAL_INTERNAL" /D "_MT" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib raptor.lib librdf.lib /nologo /dll /debug /machine:I386 /pdbtype:sept /libpath:"..\..\raptor-1.3.3\win32\Debug" /libpath:"..\..\redland-0.9.19\librdf\win32\Debug"

!ENDIF 

# Begin Target

# Name "rasqal - Win32 Release"
# Name "rasqal - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\getopt.c
# End Source File
# Begin Source File

SOURCE=..\rasqal_engine.c
# End Source File
# Begin Source File

SOURCE=..\rasqal_expr.c
# End Source File
# Begin Source File

SOURCE=..\rasqal_general.c
# End Source File
# Begin Source File

SOURCE=..\rasqal_literal.c
# End Source File
# Begin Source File

SOURCE=..\rasqal_query.c
# End Source File
# Begin Source File

SOURCE=..\rasqal_raptor.c
# End Source File
# Begin Source File

SOURCE=..\rasqal_redland.c
# End Source File
# Begin Source File

SOURCE=..\rdql_lexer.c
# End Source File
# Begin Source File

SOURCE=..\rdql_parser.c
# End Source File
# Begin Source File

SOURCE=..\roqet.c
# End Source File
# Begin Source File

SOURCE=..\strcasecmp.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\rasqal.h
# End Source File
# Begin Source File

SOURCE=..\rasqal_getopt.h
# End Source File
# Begin Source File

SOURCE=..\rasqal_internal.h
# End Source File
# Begin Source File

SOURCE=..\rdql_common.h
# End Source File
# Begin Source File

SOURCE=..\rdql_lexer.h
# End Source File
# Begin Source File

SOURCE=..\rdql_parser.h
# End Source File
# Begin Source File

SOURCE=..\rdql_parser.h
# End Source File
# Begin Source File

SOURCE=..\sparql_common.h
# End Source File
# Begin Source File

SOURCE=..\sparql_lexer.h
# End Source File
# Begin Source File

SOURCE=..\sparql_parser.h
# End Source File
# Begin Source File

SOURCE=..\sparql_parser.h
# End Source File
# Begin Source File

SOURCE=..\win32_rasqal_config.h
# End Source File
# End Group
# End Target
# End Project
