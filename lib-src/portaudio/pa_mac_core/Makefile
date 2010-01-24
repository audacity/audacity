# Make PortAudio Library for Mac OS X / CoreAudio

CC = cc

CFLAGS = -g -I../pa_common -I../pablio
PASRC = ../pa_common/pa_lib.c ../pa_common/pa_convert.c \
	../pablio/ringbuffer.c pa_mac_core.c
PAINC = ../pa_common/portaudio.h
PAOBJ = ../pa_common/pa_lib.o ../pa_common/pa_convert.o \
	../pablio/ringbuffer.o pa_mac_core.o 

portaudio.a: $(PAOBJ)
	ar ruv portaudio.a $(PAOBJ)
	ranlib portaudio.a

clean:
	rm -f portaudio.a *.o

%.o: %.c $(PAINC) Makefile
	$(CC) -c $(CFLAGS) $< -o $@
