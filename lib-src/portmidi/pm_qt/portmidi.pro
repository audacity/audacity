#-------------------------------------------------
#
# Project created by QtCreator 2010-09-02T12:50:47
#
#-------------------------------------------------

QT       -= core gui

TARGET = portmidi
TEMPLATE = lib
CONFIG += staticlib # replace this with DLL for dynamic link on Windows

INCLUDEPATH = pm_common/ porttime/

win32 {
	INCLUDEPATH += pm_win/
	LIBS += -lwinmm
	SOURCES += pm_win/pmwinmm.c \
	    pm_win/pmwin.c
	HEADERS += pm_win/pmwinmm.h
}

# this build hasn't been tested on Linux yet
unix {
	DEFINES += PMALSA
	INCLUDEPATH += pm_linux/
	LIBS += -lasound
	SOURCES += pm_linux/finddefault.c \
	    pm_linux/pmlinux.c \
	    pm_linux/pmlinuxalsa.c
	HEADERS += pm_linux/pmlinux.h pm_linux/pmlinuxalsa.h
}


DEFINES -= UNICODE

SOURCES += \
    pm_common/portmidi.c \
    pm_common/pmutil.c \
    porttime/porttime.c \
    porttime/ptwinmm.c

HEADERS += \
    pm_common/pmutil.h \
    pm_common/pminternal.h \
    pm_common/portmidi.h \
    porttime/porttime.h
