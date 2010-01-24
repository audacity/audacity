# Make PortAudio Library for Unix/OSS

CC = gcc

override CFLAGS += -g -O -ansi -I../pa_common
PASRC = ../pa_common/pa_lib.c pa_win_wmme.c
PAINC = ../pa_common/portaudio.h
PAOBJ = ../pa_common/pa_lib.o pa_win_wmme.o

portaudio.a: $(PAOBJ)
	ar ruv portaudio.a $(PAOBJ)
	ranlib portaudio.a
#	cp portaudio.a $(DEST)

clean:
	rm -f portaudio.a $(PAOBJ)

distclean: clean

%.o: %.c $(PAINC) Makefile
	$(CC) -c $(CFLAGS) $< -o $@
