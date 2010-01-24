# Microsoft Developer Studio Project File - Name="raptor" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=raptor - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "raptor.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "raptor.mak" CFG="raptor - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "raptor - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "raptor - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName "$/RDFDB/DSPs/RDF File/raptor"
# PROP Scc_LocalPath "."
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "raptor - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WIN32_EXPORTS" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "...." /I "../curl-7.12.1/include" /I ".." /I "../../expat-1.95.8/lib" /I "C:\OLD D DRIVE\Expat-1.95.7\Source\lib" /I "C:\OLD D DRIVE\curl\curl-7.10.3\include" /D "RAPTOR_INTERNAL" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WIN32_EXPORTS" /FD /c
# SUBTRACT CPP /YX /Yc /Yu
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 urlmon.lib xmlparse.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386 /libpath:"..\expat\lib"

!ELSEIF  "$(CFG)" == "raptor - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WIN32_EXPORTS" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "../../curl-7.12.1/include" /I ".." /I "../../expat-1.95.8/lib" /I "C:\OLD D DRIVE\Expat-1.95.7\Source\lib" /I "C:\OLD D DRIVE\curl\curl-7.10.3\include" /D "RAPTOR_INTERNAL" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WIN32_EXPORTS" /FR /FD /GZ /c /Tc
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 urlmon.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /map /debug /machine:I386 /pdbtype:sept /libpath:"..\expat\lib"
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "raptor - Win32 Release"
# Name "raptor - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\getopt.c
# End Source File
# Begin Source File

SOURCE=..\ntriples_parse.c
# End Source File
# Begin Source File

SOURCE=..\raptor_expat.c
# End Source File
# Begin Source File

SOURCE=..\raptor_feature.c
# End Source File
# Begin Source File

SOURCE=..\raptor_general.c
# End Source File
# Begin Source File

SOURCE=..\raptor_identifier.c
# End Source File
# Begin Source File

SOURCE=..\raptor_iostream.c
# End Source File
# Begin Source File

SOURCE=..\raptor_libxml.c
# End Source File
# Begin Source File

SOURCE=..\raptor_locator.c
# End Source File
# Begin Source File

SOURCE=..\raptor_namespace.c
# End Source File
# Begin Source File

SOURCE=..\raptor_nfc.c
# End Source File
# Begin Source File

SOURCE=..\raptor_nfc_data.c
# End Source File
# Begin Source File

SOURCE=..\raptor_parse.c
# End Source File
# Begin Source File

SOURCE=..\raptor_qname.c
# End Source File
# Begin Source File

SOURCE=..\raptor_rdfxml.c
# End Source File
# Begin Source File

SOURCE=..\raptor_rfc2396.c
# End Source File
# Begin Source File

SOURCE=..\raptor_rss.c
# End Source File
# Begin Source File

SOURCE=..\raptor_sax2.c
# End Source File
# Begin Source File

SOURCE=..\raptor_sequence.c
# End Source File
# Begin Source File

SOURCE=..\raptor_serialize.c
# End Source File
# Begin Source File

SOURCE=..\raptor_set.c
# End Source File
# Begin Source File

SOURCE=..\raptor_stringbuffer.c
# End Source File
# Begin Source File

SOURCE=..\raptor_uri.c
# End Source File
# Begin Source File

SOURCE=..\raptor_utf8.c
# End Source File
# Begin Source File

SOURCE=..\raptor_win32.c
# End Source File
# Begin Source File

SOURCE=..\raptor_www.c
# End Source File
# Begin Source File

SOURCE=..\raptor_www_curl.c
# End Source File
# Begin Source File

SOURCE=..\raptor_www_libfetch.c
# End Source File
# Begin Source File

SOURCE=..\raptor_www_libwww.c
# End Source File
# Begin Source File

SOURCE=..\raptor_www_libxml.c
# End Source File
# Begin Source File

SOURCE=..\raptor_xml.c
# End Source File
# Begin Source File

SOURCE=..\raptor_xml_writer.c
# End Source File
# Begin Source File

SOURCE=..\strcasecmp.c
# End Source File
# Begin Source File

SOURCE=..\turtle_lexer.c
# End Source File
# Begin Source File

SOURCE=..\turtle_parser.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\ntriples.h
# End Source File
# Begin Source File

SOURCE=..\raptor.h
# End Source File
# Begin Source File

SOURCE=..\raptor_getopt.h
# End Source File
# Begin Source File

SOURCE=..\raptor_internal.h
# End Source File
# Begin Source File

SOURCE=..\raptor_nfc.h
# End Source File
# Begin Source File

SOURCE=..\turtle_common.h
# End Source File
# Begin Source File

SOURCE=..\turtle_lexer.h
# End Source File
# Begin Source File

SOURCE=..\turtle_parser.h
# End Source File
# Begin Source File

SOURCE=..\win32_raptor_config.h
# End Source File
# End Group
# End Target
# End Project
