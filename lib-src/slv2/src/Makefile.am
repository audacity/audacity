AM_CFLAGS = -std=c99 @SLV2_CFLAGS@ @REDLAND_CFLAGS@ -DLIBSLV2_SOURCE

lib_LTLIBRARIES = libslv2.la
libslv2_la_LIBADD = @REDLAND_LIBS@ -ldl
libslv2_la_LDFLAGS = -version-info @SLV2_SO_VERSION@

libslv2_la_SOURCES = \
	plugin.c \
	pluginclass.c \
	pluginclasses.c \
	plugininstance.c \
	plugins.c \
	pluginui.c \
	pluginuiinstance.c \
	pluginuis.c \
	port.c \
	query.c \
	scalepoint.c \
	scalepoints.c \
	slv2_internal.h \
	util.c \
	value.c \
	values.c \
	world.c
