## Process this file with automake to produce Makefile.in

man_MANS = sndfile-info.1 sndfile-play.1 sndfile-convert.1 sndfile-cmp.1 \
			sndfile-metadata-get.1 sndfile-metadata-set.1 sndfile-concat.1 \
			sndfile-interleave.1 sndfile-deinterleave.1

EXTRA_DIST = sndfile-info.1 sndfile-play.1 sndfile-convert.1 sndfile-cmp.1 \
			sndfile-metadata-get.1 sndfile-concat.1 sndfile-interleave.1

# Same manpage for both programs.
sndfile-metadata-set.1 : sndfile-metadata-get.1
	$(LN_S) $(srcdir)/sndfile-metadata-get.1 $@

sndfile-deinterleave.1 : sndfile-interleave.1
	$(LN_S) $(srcdir)/sndfile-interleave.1 $@
