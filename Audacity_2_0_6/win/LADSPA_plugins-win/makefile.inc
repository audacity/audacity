#
#
#
CFLAGS = /LD /I ..\glibc_includes /I .. /I ..\.. /I ..\..\gverb /I . /FI ..\config.h
LDFLAGS = /NOLOGO /DLL /MANIFEST:NO /SUBSYSTEM:WINDOWS /MACHINE:X86 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib util.lib libfftw3-3.lib libfftw3f-3.lib libfftw3l-3.lib

!IF "$(CONFIG)" == "Debug"
CFLAGS = $(CFLAGS) /Zi /Od
LDFLAGS = $(LDFLAGS) /DEBUG
!ELSE
CFLAGS = $(CFLAGS) /O2 /Ob2
LDFLAGS = $(LDFLAGS)
!ENDIF

all: libfftw3-3.lib libfftw3f-3.lib libfftw3l-3.lib util.lib dlls

#
# Inference rules
#
{..\..}.c.obj:
   $(CC) $(CFLAGS) /c $<

.obj.dll:
   link $(LDFLAGS) /OUT:$(<B).dll $<

#
# Must create the libraries for FFTW
#
libfftw3-3.lib: ..\libfftw3-3.def
   lib /MACHINE:X86 /OUT:$@ /DEF:$?

libfftw3f-3.lib: ..\libfftw3f-3.def
   lib /MACHINE:X86 /OUT:$@ /DEF:$?

libfftw3l-3.lib: ..\libfftw3l-3.def
   lib /MACHINE:X86 /OUT:$@ /DEF:$?

#
# A few of them get ignored
#
analogue_osc_1416.dll: analogue_osc_1416.obj
   echo ***EXCLUDED FROM BUILD*** analogue_osc_1416.dll
   
fm_osc_1415.dll: fm_osc_1415.obj
   echo ***EXCLUDED FROM BUILD*** fm_osc_1415.dll

gsm_1215.dll: gsm_1215.obj
   echo ***EXCLUDED FROM BUILD*** gsm_1215.dll

hermes_filter_1200.dll: hermes_filter_1200.obj
   echo ***EXCLUDED FROM BUILD*** hermes_filter_1200.dll

#
# Special dependencies for util.lib
#
{..\..\util}.c.obj:
   $(CC) $(CFLAGS) /c $<

util.lib: db.obj iir.obj pitchscale.obj rms.obj
   lib /MACHINE:X86 /OUT:$@ $(?F: = )

db.obj: ..\..\util\db.c

iir.obj: ..\..\util\iir.c

pitchscale.obj: ..\..\util\pitchscale.c

rms.obj: ..\..\util\rms.c

#
# Special dependencies for gverb
#
{..\..\gverb}.c.obj:
   $(CC) $(CFLAGS) /c $<

gverb_1216.dll: gverb_1216.obj gverb.obj gverbdsp.obj
   link $(LDFLAGS) /OUT:$@ $?

gverb.obj: ..\..\gverb\gverb.c

gverbdsp.obj: ..\..\gverb\gverbdsp.c

