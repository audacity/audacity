AM_CXXFLAGS = -funroll-loops -I../include -fstrict-aliasing $(SNDFILE_CFLAGS) $(PORTAUDIO_CFLAGS) $(WX_CFLAGS)

libsbsmsincludedir = $(includedir)
libsbsmsinclude_HEADERS = ../include/sbsms.h

lib_LTLIBRARIES = libsbsms.la
libsbsms_la_SOURCES = pitch.cpp peak.cpp sms.cpp track.cpp trackpoint.cpp utils.cpp audio.cpp resample.cpp sbsms.cpp fft.cpp grain.cpp buffer.cpp subband.cpp audio.h buffer.h fft.h utils.h defs.h peak.h trackpoint.h grain.h pitch.h real.h resample.h ../include/sbsms.h sms.h subband.h track.h

libsbsms_la_LDFLAGS = -version-info @SHARED_VERSION_INFO@ @SHLIB_VERSION_ARG@

bin_PROGRAMS = @SBSMS_PROGS@
EXTRA_PROGRAMS = sbsms sbsmsplay wxsbsmsplayer

sbsms_LDADD = libsbsms.la $(SNDFILE_LIBS) $(MAD_LIBS)
sbsms_LDFLAGS = $(os_ldflags)

if ENABLE_PORTAUDIO
sbsmsplay_LDADD = libsbsms.la $(PORTAUDIO_LIBS)
sbsmsplay_LDFLAGS = $(os_ldflags)
sbsmsplay_SOURCES = testplay.cpp play.cpp play.h audiobuffer.cpp audiobuffer.h
endif

if ENABLE_PORTAUDIO
if ENABLE_WX
wxsbsmsplayer_LDADD = libsbsms.la $(PORTAUDIO_LIBS) $(SNDFILE_LIBS) $(MAD_LIBS) $(WX_LIBS)
wxsbsmsplayer_LDFLAGS = $(os_ldflags) $(PORTAUDIO_CFLAGS) $(SNDFILE_CFLAGS)
endif
endif

if ENABLE_SNDFILE
if ENABLE_MP3
sbsms_SOURCES = test.cpp mp3.cpp mp3.h mp3tech.cpp mp3tech.h audiobuffer.cpp audiobuffer.h import.cpp import.h pcm.cpp pcm.h convert.cpp convert.h
else
sbsms_SOURCES = test.cpp import.cpp import.h pcm.cpp pcm.h convert.cpp convert.h
endif
else
sbsms_SOURCES = test.cpp convert.cpp convert.h
endif

if ENABLE_SNDFILE
if ENABLE_MP3
wxsbsmsplayer_SOURCES = wxplayer.cpp wxplayer.h wxplayerframe.cpp wxplayerframe.h wxplayctrl.cpp wxplayctrl.h wxconvertdlg.cpp wxconvertdlg.h play.cpp play.h convert.cpp convert.h mp3.cpp mp3.h mp3tech.cpp mp3tech.h import.cpp import.h pcm.cpp pcm.h audiobuffer.cpp audiobuffer.h
else
wxsbsmsplayer_SOURCES = wxplayer.cpp wxplayer.h wxplayerframe.cpp wxplayerframe.h wxplayctrl.cpp wxplayctrl.h wxconvertdlg.cpp wxconvertdlg.h play.cpp play.h convert.cpp convert.h import.cpp import.h pcm.cpp pcm.h audiobuffer.cpp audiobuffer.h
endif
else
wxsbsmsplayer_SOURCES = wxplayer.cpp wxplayer.h wxplayerframe.cpp wxplayerframe.h wxplayctrl.cpp wxplayctrl.h wxconvertdlg.cpp wxconvertdlg.h play.cpp play.h convert.cpp convert.h audiobuffer.cpp audiobuffer.h
endif

if ENABLE_WX
if ENABLE_PORTAUDIO
if MACOSX
clean: cleanapproot clean-am
cleanapproot:
	sudo rm -rf $(top_builddir)/approot
all: $(top_builddir)/approot/Applications/wxsbsmsplayer.app $(top_builddir)/mac/Info.plist $(top_builddir)/mac/sbsms.icns
$(top_builddir)/approot/Applications/wxsbsmsplayer.app: wxsbsmsplayer
	sudo strip -u -r wxsbsmsplayer
	sudo mkdir -p $(top_builddir)/approot/Applications/wxsbsmsplayer.app/Contents/MacOS
	sudo mkdir -p $(top_builddir)/approot/Applications/wxsbsmsplayer.app/Contents/Resources
	sudo cp -f wxsbsmsplayer $(top_builddir)/approot/Applications/wxsbsmsplayer.app/Contents/MacOS/wxsbsmsplayer
	sudo cp -f $(top_builddir)/mac/Info.plist $(top_builddir)/approot/Applications/wxsbsmsplayer.app/Contents
	sudo cp -f $(top_builddir)/mac/sbsms.icns $(top_builddir)/approot/Applications/wxsbsmsplayer.app/Contents/Resources
	sudo sh -c "echo sbsms > $(top_builddir)/approot/Applications/wxsbsmsplayer.app/Contents/PkgInfo"
	sudo chown -R root:admin $(top_builddir)/approot/Applications
endif
endif
endif
