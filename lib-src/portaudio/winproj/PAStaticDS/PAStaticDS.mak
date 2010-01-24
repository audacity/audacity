# Microsoft Developer Studio Generated NMAKE File, Based on PAStaticDS.dsp

!IF "$(CFG)" == ""

CFG=PAStaticDS - Win32 Debug

!MESSAGE No configuration specified. Defaulting to PAStaticDS - Win32 Debug.

!ENDIF 



!IF "$(CFG)" != "PAStaticDS - Win32 Release" && "$(CFG)" != "PAStaticDS - Win32 Debug"

!MESSAGE Invalid configuration "$(CFG)" specified.

!MESSAGE You can specify a configuration when running NMAKE

!MESSAGE by defining the macro CFG on the command line. For example:

!MESSAGE 

!MESSAGE NMAKE /f "PAStaticDS.mak" CFG="PAStaticDS - Win32 Debug"

!MESSAGE 

!MESSAGE Possible choices for configuration are:

!MESSAGE 

!MESSAGE "PAStaticDS - Win32 Release" (based on "Win32 (x86) Static Library")

!MESSAGE "PAStaticDS - Win32 Debug" (based on "Win32 (x86) Static Library")

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



!IF  "$(CFG)" == "PAStaticDS - Win32 Release"



OUTDIR=.\Release

INTDIR=.\Release



ALL : "..\Lib\PAStaticDS.lib"





CLEAN :

	-@erase "$(INTDIR)\dsound_wrapper.obj"

	-@erase "$(INTDIR)\pa_dsound.obj"

	-@erase "$(INTDIR)\pa_lib.obj"

	-@erase "$(INTDIR)\vc60.idb"

	-@erase "..\Lib\PAStaticDS.lib"



"$(OUTDIR)" :

    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"



CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "../../pa_common" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /Fp"$(INTDIR)\PAStaticDS.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

BSC32=bscmake.exe

BSC32_FLAGS=/nologo /o"$(OUTDIR)\PAStaticDS.bsc" 

BSC32_SBRS= \

	

LIB32=link.exe -lib

LIB32_FLAGS=/nologo /out:"../Lib/PAStaticDS.lib" 

LIB32_OBJS= \

	"$(INTDIR)\dsound_wrapper.obj" \

	"$(INTDIR)\pa_dsound.obj" \

	"$(INTDIR)\pa_lib.obj"



"..\Lib\PAStaticDS.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)

    $(LIB32) @<<

  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)

<<



!ELSEIF  "$(CFG)" == "PAStaticDS - Win32 Debug"



OUTDIR=.\Debug

INTDIR=.\Debug



ALL : "..\Lib\PAStaticDSD.lib"





CLEAN :

	-@erase "$(INTDIR)\dsound_wrapper.obj"

	-@erase "$(INTDIR)\pa_dsound.obj"

	-@erase "$(INTDIR)\pa_lib.obj"

	-@erase "$(INTDIR)\vc60.idb"

	-@erase "$(INTDIR)\vc60.pdb"

	-@erase "..\Lib\PAStaticDSD.lib"



"$(OUTDIR)" :

    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"



CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /I "../../pa_common" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /Fp"$(INTDIR)\PAStaticDS.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

BSC32=bscmake.exe

BSC32_FLAGS=/nologo /o"$(OUTDIR)\PAStaticDS.bsc" 

BSC32_SBRS= \

	

LIB32=link.exe -lib

LIB32_FLAGS=/nologo /out:"../Lib/PAStaticDSD.lib" 

LIB32_OBJS= \

	"$(INTDIR)\dsound_wrapper.obj" \

	"$(INTDIR)\pa_dsound.obj" \

	"$(INTDIR)\pa_lib.obj"



"..\Lib\PAStaticDSD.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)

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

!IF EXISTS("PAStaticDS.dep")

!INCLUDE "PAStaticDS.dep"

!ELSE 

!MESSAGE Warning: cannot find "PAStaticDS.dep"

!ENDIF 

!ENDIF 





!IF "$(CFG)" == "PAStaticDS - Win32 Release" || "$(CFG)" == "PAStaticDS - Win32 Debug"

SOURCE=..\..\pa_win_ds\dsound_wrapper.c



"$(INTDIR)\dsound_wrapper.obj" : $(SOURCE) "$(INTDIR)"

	$(CPP) $(CPP_PROJ) $(SOURCE)





SOURCE=..\..\pa_win_ds\pa_dsound.c



"$(INTDIR)\pa_dsound.obj" : $(SOURCE) "$(INTDIR)"

	$(CPP) $(CPP_PROJ) $(SOURCE)





SOURCE=..\..\pa_common\pa_lib.c



"$(INTDIR)\pa_lib.obj" : $(SOURCE) "$(INTDIR)"

	$(CPP) $(CPP_PROJ) $(SOURCE)







!ENDIF 



