pkglib_LTLIBRARIES = mod-script-pipe.la

ACLOCAL_AMFLAGS = ${ACLOCAL_AMFLAGS} -I m4

mod_script_pipe_la_CPPFLAGS = $(WX_CXXFLAGS) -DBUILDING_SCRIPT_PIPE -I$(top_srcdir)/../../include -I$(top_srcdir)/../../src -I$(top_builddir)/../../src -I$(top_srcdir)/../portaudio-v19/include 
mod_script_pipe_la_LIBADD = $(WX_LIBS)
mod_script_pipe_la_LDFLAGS = -module

mod_script_pipe_la_SOURCES = \
	PipeServer.cpp \
	PipeServer.h \
	ScripterCallback.cpp \
	ScripterCallback.h
	$(NULL)

