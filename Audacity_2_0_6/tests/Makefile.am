check_PROGRAMS = SequenceTest SimpleBlockFileTest

SequenceTest_CPPFLAGS = $(WX_CXXFLAGS)
SequenceTest_LDADD = $(top_srcdir)/src/libaudacity.la $(WX_LIBS)
SequenceTest_SOURCES = SequenceTest.cpp

SimpleBlockFileTest_CPPFLAGS = $(WX_CXXFLAGS)
SimpleBlockFileTest_LDADD = $(top_srcdir)/src/libaudacity.la $(WX_LIBS)
SimpleBlockFileTest_SOURCES = SimpleBlockFileTest.cpp

TESTS = $(check_PROGRAMS)

EXTRA_DIST = \
	ProjectCheckTests/missing_aliased_and_auf_files_data/e00/d00 \
	ProjectCheckTests/missing_blockfile_data \
	ProjectCheckTests/orphaned_blockfiles_data/e00/d00/e000055a.au \
	ProjectCheckTests/orphaned_blockfiles_data/e00/d00/orphan1.au \
	ProjectCheckTests/orphaned_blockfiles_data/e00/d00/orphan2.au \
	ProjectCheckTests/missing_aliased_and_auf_files.aup \
	ProjectCheckTests/missing_blockfile.aup \
	ProjectCheckTests/orphaned_blockfiles.aup \
	ProjectCheckTests/readme.txt \
	$(NULL)
