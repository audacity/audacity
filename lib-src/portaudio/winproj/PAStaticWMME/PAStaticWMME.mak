# Microsoft Developer Studio Generated NMAKE File, Based on PAStaticWMME.dsp

!IF "$(CFG)" == ""

CFG=PAStaticWMME - Win32 Debug

!MESSAGE No configuration specified. Defaulting to PAStaticWMME - Win32 Debug.

!ENDIF 



!IF "$(CFG)" != "PAStaticWMME - Win32 Release" && "$(CFG)" != "PAStaticWMME - Win32 Debug"

!MESSAGE Invalid configuration "$(CFG)" specified.

!MESSAGE You can specify a configuration when running NMAKE

!MESSAGE by defining the macro CFG on the command line. For example:

!MESSAGE 

!MESSAGE NMAKE /f "PAStaticWMME.mak" CFG="PAStaticWMME - Win32 Debug"

!MESSAGE 

!MESSAGE Possible choices for configuration are:

!MESSAGE 

!MESSAGE "PAStaticWMME - Win32 Release" (based on "Win32 (x86) Static Library")

!MESSAGE "PAStaticWMME - Win32 Debug" (based on "Win32 (x86) Static Library")

!MESSAGE 

!ERROR An invalid configuration is specified.

!ENDIF 



!IF "$(OS)" == "Windows_NT"

NULL=

!ELSE 

NULL=nul

!ENDIF 



CPP=cl.exe

RSC=rc.exe



!IF  "$(CFG)" == "PAStaticWMME - Win32 Release"



OUTDIR=.\Release

INTDIR=.\Release



ALL : "..\Lib\PAStaticWMME.lib"





CLEAN :

	-@erase "$(INTDIR)\pa_lib.obj"

	-@erase "$(INTDIR)\pa_win_wmme.obj"

	-@erase "$(INTDIR)\vc60.idb"

	-@erase "..\Lib\PAStaticWMME.lib"



"$(OUTDIR)" :

    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"



CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "../../pa_common" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /Fp"$(INTDIR)\PAStaticWMME.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

BSC32=bscmake.exe

BSC32_FLAGS=/nologo /o"$(OUTDIR)\PAStaticWMME.bsc" 

BSC32_SBRS= \

	

LIB32=link.exe -lib

LIB32_FLAGS=/nologo /out:"../Lib/PAStaticWMME.lib" 

LIB32_OBJS= \

	"$(INTDIR)\pa_lib.obj" \

	"$(INTDIR)\pa_win_wmme.obj"



"..\Lib\PAStaticWMME.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)

    $(LIB32) @<<

  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)

<<



!ELSEIF  "$(CFG)" == "PAStaticWMME - Win32 Debug"



OUTDIR=.\Debug

INTDIR=.\Debug



ALL : "..\Lib\PAStaticWMMED.lib"





CLEAN :

	-@erase "$(INTDIR)\pa_lib.obj"

	-@erase "$(INTDIR)\pa_win_wmme.obj"

	-@erase "$(INTDIR)\vc60.idb"

	-@erase "$(INTDIR)\vc60.pdb"

	-@erase "..\Lib\PAStaticWMMED.lib"



"$(OUTDIR)" :

    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"



CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /I "../../pa_common" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /Fp"$(INTDIR)\PAStaticWMME.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

BSC32=bscmake.exe

BSC32_FLAGS=/nologo /o"$(OUTDIR)\PAStaticWMME.bsc" 

BSC32_SBRS= \

	

LIB32=link.exe -lib

LIB32_FLAGS=/nologo /out:"../Lib/PAStaticWMMED.lib" 

LIB32_OBJS= \

	"$(INTDIR)\pa_lib.obj" \

	"$(INTDIR)\pa_win_wmme.obj"



"..\Lib\PAStaticWMMED.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)

    $(LIB32) @<<

  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)

<<



!ENDIF 



.c{$(INTDIR)}.obj::

   $(CPP) @<<

   $(CPP_PROJ) $< 

<<



.cpp{$(INTDIR)}.obj::

   $(CPP) @<<

   $(CPP_PROJ) $< 

<<



.cxx{$(INTDIR)}.obj::

   $(CPP) @<<

   $(CPP_PROJ) $< 

<<



.c{$(INTDIR)}.sbr::

   $(CPP) @<<

   $(CPP_PROJ) $< 

<<



.cpp{$(INTDIR)}.sbr::

   $(CPP) @<<

   $(CPP_PROJ) $< 

<<



.cxx{$(INTDIR)}.sbr::

   $(CPP) @<<

   $(CPP_PROJ) $< 

<<





!IF "$(NO_EXTERNAL_DEPS)" != "1"

!IF EXISTS("PAStaticWMME.dep")

!INCLUDE "PAStaticWMME.dep"

!ELSE 

!MESSAGE Warning: cannot find "PAStaticWMME.dep"

!ENDIF 

!ENDIF 





!IF "$(CFG)" == "PAStaticWMME - Win32 Release" || "$(CFG)" == "PAStaticWMME - Win32 Debug"

SOURCE=..\..\pa_common\pa_lib.c



"$(INTDIR)\pa_lib.obj" : $(SOURCE) "$(INTDIR)"

	$(CPP) $(CPP_PROJ) $(SOURCE)





SOURCE=..\..\pa_win_wmme\pa_win_wmme.c



"$(INTDIR)\pa_win_wmme.obj" : $(SOURCE) "$(INTDIR)"

	$(CPP) $(CPP_PROJ) $(SOURCE)







!ENDIF 



