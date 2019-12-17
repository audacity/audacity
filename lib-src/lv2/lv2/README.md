LV2
===

LV2 is a plugin standard for audio systems. It defines a minimal yet extensible
C API for plugin code and a format for plugin "bundles".  See
<http://lv2plug.in> for more information.

This package contains specifications (C headers and Turtle files),
documentation generation tools, and example plugins.

Building and installation requires only Python 2.6.  Building the documentation
requires Doxygen.


Installation
------------

A typical build looks something like this:

    ./waf configure --prefix=/foo
    ./waf
    sudo ./waf install

or, for packaging:

    DESTDIR=/home/packager/lv2root ./waf install

For help on the other available options, run:

    ./waf --help

The bundle installation directory can be set with the --lv2dir option, e.g.:

    ./waf configure --lv2dir=/foo/lib/lv2

Configuring with `--lv2-user` will install bundles to the user-local location.


Packaging
---------

Specification bundles are both a build-time and run-time dependency of programs
that use LV2.  Programs expect their data to be available somewhere in
`LV2_PATH`.

See <http://lv2plug.in/pages/filesystem-hierarchy-standard.html> for details on
the standard installation paths.

Do not split up LV2 bundles, they are self-contained and must remain whole.
Other than that, things may be split to suit distribution needs.  For example,
separate packages for specifications, tools, and plugins, may be good idea.


Header Installation
-------------------

By default symbolic links to headers in bundles are installed to `INCLUDEDIR`.
If symbolic links are a problem, configure with `--copy-headers` and copies
will be installed instead.

Headers are installed in two paths, the universal URI-based style:

    #include "lv2/lv2plug.in/ns/ext/urid/urid.h"

and the newer simple core style:

    #include "lv2/urid/urid.h"

Projects are encouraged to migrate to the latter style, though note that this
style of include path may only be used by official LV2 specifications.


Documentation
-------------

Configuring with the --docs option will build the documentation for all the
included specifications if Doxygen is available.  For example:

    ./waf configure --docs
    ./waf

Specification documentation is also availabe online at <http://lv2plug.in/ns>.
