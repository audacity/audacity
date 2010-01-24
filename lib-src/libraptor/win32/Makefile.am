EXTRA_DIST=README.txt \
raptor.dsp raptor.dsw \
rapper.dsp \
raptortest.cpp raptortest.dsp \
raptor.sln \
rapper.vcproj raptor.vcproj raptortest.vcproj

# Make some empty dirs that may be needed
dist-hook:
	for dir in Debug Release; do \
	  mkdir $(distdir)/$$dir; \
        done
