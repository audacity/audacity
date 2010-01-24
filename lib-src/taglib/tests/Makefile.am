INCLUDES = \
        -I$(top_srcdir)/taglib\
        -I$(top_srcdir)/taglib/toolkit \
        -I$(top_srcdir)/taglib/trueaudio \
        -I$(top_srcdir)/taglib/mpeg \
        -I$(top_srcdir)/taglib/mpeg/id3v1 \
        -I$(top_srcdir)/taglib/mpeg/id3v2 \
        -I$(top_srcdir)/taglib/mpeg/id3v2/frames

test_runner_SOURCES = \
	main.cpp \
	test_list.cpp \
	test_map.cpp \
	test_mpeg.cpp \
	test_synchdata.cpp \
	test_trueaudio.cpp \
	test_bytevector.cpp \
	test_string.cpp \
	test_fileref.cpp \
	test_id3v1.cpp \
	test_id3v2.cpp

if build_tests
TESTS = test_runner
check_PROGRAMS = test_runner
LDADD = ../taglib/libtag.la -lcppunit
endif
