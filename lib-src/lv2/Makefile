#include srcdir.mk

all:
	./configure --build

check:

clean:
	./configure --clean

distclean: clean
	find . -name __pycache__ \
		 -o -name \*.pyc \
		 -o -name .lock\* \
		 -o -name build |	xargs rm -rf

EXTRA_DIST = $(shell find -type f)

distdir: $(EXTRA_DIST)
	@for file in $^; do \
	   dir=$$(echo "/$$file" | sed -e 's,/[^/]*$$,,'); \
	   if test ! -d "$(distdir)$$dir"; then mkdir -p "$(distdir)$$dir"; fi; \
	   cp -p $$file "$(distdir)/$$file" || exit 1; \
	done

.PHONY: all check clean distclean distdir
