# MAKEFILE FOR PORTMIDI AND PORTTIME

# NOTE: make should be run from the portmidi directory, but this
# Makefile is in pm_linux, so you should run:
#   make -f pm_linux/Makefile
# I suggest putting this command line into a script or alias, e.g.
# do this:
#-----------------
# cd; cd portmidi; cat > m
# make -f pm_linux/Makefile
# <CONTROL-D>
# chmod +x m
#-----------------
# Now you can just type ./m to run make. (the script "m" is not
# part of PortMidi because it is different for OS X and it's so
# simple to create.)

# For debugging, define PM_CHECK_ERRORS
PMFLAGS = -DPM_CHECK_ERRORS
# Otherwise do not define PM_CHECK_ERRORS
# PMFLAGS = 

# Use this for linux alsa (0.9x) version
versions = pm_linux/pmlinuxalsa.o
ALSALIB = -lasound
VFLAGS = -DPMALSA

# Use this for null (a dummy implementation for no Midi I/O:
# versions = pmlinuxnull.o
# ALSALIB = 
# VFLAGS = -DPMNULL

pmlib = pm_linux/libportmidi.a

ptlib = porttime/libporttime.a

CC = gcc $(VFLAGS) $(PMFLAGS) -g -Ipm_common -Iporttime

pmobjects = pm_common/pmutil.o $(versions) pm_linux/pmlinux.o  \
	pm_common/portmidi.o

ptobjects = porttime/porttime.o porttime/ptlinux.o 

current: all

all: $(pmlib) $(ptlib) pm_test/test pm_test/sysex pm_test/midithread \
	pm_test/latency pm_test/midithru pm_test/qtest pm_test/mm

$(pmlib): pm_linux/Makefile $(pmobjects)
	ar -cr $(pmlib) $(pmobjects)

$(ptlib): pm_linux/Makefile $(ptobjects)
	ar -cr $(ptlib) $(ptobjects)

pm_linux/pmlinuxalsa.o: pm_linux/Makefile pm_linux/pmlinuxalsa.c pm_linux/pmlinuxalsa.h
	$(CC) -c pm_linux/pmlinuxalsa.c -o pm_linux/pmlinuxalsa.o

pm_test/test: pm_linux/Makefile pm_test/test.o $(pmlib) $(ptlib)
	$(CC) pm_test/test.o -o pm_test/test $(pmlib) $(ptlib) $(ALSALIB)

pm_test/sysex: pm_linux/Makefile pm_test/sysex.o $(pmlib) $(ptlib)
	$(CC) pm_test/sysex.o -o pm_test/sysex $(pmlib) $(ptlib) $(ALSALIB)

pm_test/midithread: pm_linux/Makefile pm_test/midithread.o $(pmlib) $(ptlib)
	$(CC) pm_test/midithread.o -o pm_test/midithread \
        $(pmlib) $(ptlib) $(ALSALIB)

pm_test/latency: pm_linux/Makefile $(ptlib) pm_test/latency.o 
	$(CC) pm_test/latency.o -o pm_test/latency $(pmlib) $(ptlib) \
        $(ALSALIB) -lpthread -lm

pm_test/midithru: pm_linux/Makefile $(ptlib) pm_test/midithru.o 
	$(CC) pm_test/midithru.o -o pm_test/midithru $(pmlib) $(ptlib) \
        $(ALSALIB) -lpthread -lm

pm_test/mm: pm_linux/Makefile $(ptlib) pm_test/mm.o 
	$(CC) pm_test/mm.o -o pm_test/mm $(pmlib) $(ptlib) \
        $(ALSALIB) -lpthread -lm

porttime/ptlinux.o: pm_linux/Makefile porttime/ptlinux.c
	$(CC) -c porttime/ptlinux.c -o porttime/ptlinux.o

pm_test/qtest: pm_linux/Makefile pm_test/qtest.o $(pmlib) $(ptlib)
	$(CC) pm_test/qtest.o -o pm_test/qtest $(pmlib) $(ptlib) $(ALSALIB)

clean:
	rm -f *.o *~ core* */*.o */*.so */*~ */core* pm_test/*/pm_dll.dll 
	rm -f *.opt *.ncb *.plg pm_win/Debug/pm_dll.lib pm_win/Release/pm_dll.lib
	rm -f pm_test/*.opt pm_test/*.ncb

cleaner: clean

cleanest: cleaner
	rm -f $(pmlib) $(ptlib) pm_test/test pm_test/sysex pm_test/midithread
	rm -f pm_test/latency pm_test/midithru pm_test/qtest pm_test/mm

backup: cleanest
	cd ..; zip -r portmidi.zip portmidi
