dist_man_MANS = \
	man/man3/slv2.3 \
	man/man3/slv2_collections.3 \
	man/man3/slv2_data.3 \
	man/man3/slv2_library.3 \
	man/man3/slv2_util.3 \
	man/man3/slv2_world.3

if HAVE_DOXYGEN
man_MANS = $(dist_man_MANS)
all: docs
else
man_mans =
all:
endif

EXTRA_DIST = mainpage.dox reference.doxygen.in $(man_MANS)



%.3:
	make docs

reference.doxygen: @SLV2_SRCDIR@/doc/reference.doxygen.in

distclean-local:
	rm -rf man html

docs: reference.doxygen @SLV2_SRCDIR@/slv2/*.h
	doxygen reference.doxygen

# Don't install man pages with this target,
# only install with install-man (for debian package building)
install-data:
