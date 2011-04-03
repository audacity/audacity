## Process this file with automake to produce Makefile.in

# Prevent any extension.
EXEEXT =

CXXLD = $(CXX)
CXXLINK = $(LIBTOOL) --tag=CXX --mode=link $(CXXLD) $(AM_CXXFLAGS) \
	$(CXXFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

EXTRA_DIST = sndfile_load.m sndfile_save.m sndfile_play.m \
	octave_test.m octave_test.sh $(oct_module_srcs) PKG_ADD

octconfigdir = $(exec_prefix)/share/octave/site/m
octconfig_DATA = sndfile_load.m sndfile_save.m sndfile_play.m

OCTAVE_DEST_MDIR = @OCTAVE_DEST_MDIR@
OCTAVE_DEST_ODIR = @OCTAVE_DEST_ODIR@/sndfile

OCT_CXXFLAGS = @OCT_CXXFLAGS@
OCT_LIB_DIR = @OCT_LIB_DIR@
OCT_LIBS = @OCT_LIBS@

SNDFILEDIR = $(top_builddir)/src
INCLUDES = -I$(SNDFILEDIR)

oct_module_srcs = sndfile.cc
oct_module_files = sndfile.oct PKG_ADD

# Make these noinst so they can be installed manually.
noinst_DATA = $(oct_module_files)


# Used by shave which cleans up automake generated Makefile output.
V         = @
Q         = $(V:1=)
QUIET_GEN = $(Q:@=@echo '  GEN   '$@;)


# Use Octave's mkoctfile to do all the heavy lifting. Unfortunately, its
# a little dumb so we need to guide it carefully.
sndfile.oct : sndfile.o
	$(QUIET_GEN) $(MKOCTFILE) -v $(INCLUDES) $(top_builddir)/Octave/$+ -L$(SNDFILEDIR)/.libs -L$(SNDFILEDIR) -lsndfile -o $(top_builddir)/Octave/$@ > /dev/null

sndfile.o : sndfile.cc
	$(QUIET_GEN) $(MKOCTFILE) -v $(INCLUDES) -c $+ -o $(top_builddir)/Octave/$@ > /dev/null

# Allow for the test being run in the build dir, but the test script
# being located in the source dir.
check :
	octave_src_dir=$(srcdir) $(srcdir)/octave_test.sh


# Since the octave modules are installed in a special location, a custom install
# and uninstall routine must be specified.
install-exec-local : $(oct_module_files)
	@$(NORMAL_INSTALL)
	test -z "$(OCTAVE_DEST_ODIR)" || $(mkdir_p) "$(DESTDIR)$(OCTAVE_DEST_ODIR)"
	@list='$(oct_module_files)'; for p in $$list; do \
	  p1=`echo $$p|sed 's/$(EXEEXT)$$//'`; \
	  if test -f $$p \
	     || test -f $$p1 \
	  ; then \
	    f=`echo "$$p1" | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'`; \
	   echo " $(INSTALL_PROGRAM_ENV) $(LIBTOOL) --mode=install $(INSTALL) '$$p' '$(DESTDIR)$(OCTAVE_DEST_ODIR)/$$f'"; \
	   $(INSTALL_PROGRAM_ENV) $(LIBTOOL) --mode=install $(INSTALL) "$$p" "$(DESTDIR)$(OCTAVE_DEST_ODIR)/$$f" || exit 1; \
	  else :; fi; \
	done

uninstall-local :
	@$(NORMAL_UNINSTALL)
	@list='$(oct_module_files)'; for p in $$list; do \
	  f=`echo "$$p" | sed 's,^.*/,,;s/$(EXEEXT)$$//;$(transform);s/$$/$(EXEEXT)/'`; \
	  echo " rm -f '$(DESTDIR)$(OCTAVE_DEST_ODIR)/$$f'"; \
	  rm -f "$(DESTDIR)$(OCTAVE_DEST_ODIR)/$$f"; \
	done

clean-local :
	rm -f sndfile.o sndfile.oct
