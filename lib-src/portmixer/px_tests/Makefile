all: px_test

CFLAGS = -g -Wall -O -I../../portaudio/pa_common -I../px_common

px_test: px_test.o px_unix_oss.o ../px_common/portmixer.h
	gcc -g -o px_test px_test.o px_unix_oss.o -lm \
		-L../../portaudio/pa_unix_oss -lportaudio -lpthread

px_test.o: px_test.c ../px_common/portmixer.h
	gcc $(CFLAGS) -c px_test.c


px_unix_oss.o: ../px_unix_oss/px_unix_oss.c ../px_common/portmixer.h
	gcc $(CFLAGS) -c ../px_unix_oss/px_unix_oss.c


