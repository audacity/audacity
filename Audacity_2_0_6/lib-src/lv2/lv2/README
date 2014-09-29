LV2
===

LV2 is a plugin standard for audio systems. It defines a minimal yet extensible
C API for plugin code and a format for plugin "bundles".  See
<http://lv2plug.in> for more information.

This package contains specifications (a C header and/or a schema in Turtle),
documentation generation tools, and example plugins.

Building and installation requires only Python 2.6 and, optionally, Doxygen.


Installation
------------

A typical build looks something like this:

    ./waf configure --prefix /foo
    ./waf
    sudo ./waf install

or, for packaging:

    DESTDIR=/foo/lv2 ./waf install

For help on the various options available, run:

    ./waf --help

The bundle installation directory can be set with the --lv2-dir option, e.g.:

    ./waf configure --lv2-dir /foo/lib/lv2

Similarly, --lv2-user will install to the user LV2 directory (e.g. ~/.lv2).


Packaging
---------

Specification bundles are both a build and run time dependency of programs that
use LV2.  Programs expect their data to be available somewhere in LV2_PATH.

See <http://lv2plug.in/trac/wiki/Filesystem_Hierarchy_Standard> for details on
the standard installation paths.

Do not split up LV2 bundles, they must remain self-contained.  This is a
requirement, not a suggestion, and it supercedes any rules your distribution
may have which were likely designed for libraries (note LV2 specifications are
just text, not libraries).  You are free to link or copy things anywhere, but
under no circumstances should an incomplete part of a bundle be installed.

Other than that, things may be split up in any way.  In particular it is a good
idea to split specifications, tools, and plugins into separate packages.


Header Installation
-------------------

The install stage installs symbolic links to headers in bundles.  This is to
support universal C includes that do not change from system to system.  For
example, the URID extension's header can always be included like so:

    #include "lv2/lv2plug.in/ns/ext/urid/urid.h"

Note that some specification headers themselves have such include lines, which
must not be modified.  The details of how this is achieved are unimportant and
may be tailored to particular systems.  The requirement is simply that this
style of include must work for any header in a specification.

If you are having problems with symbolic links for whatever reason, configure
with the --copy-headers option, which will make copies instead.


Documentation
-------------

Configuring with the --docs option will build the documentation for all the
included specifications if Doxygen is available.  For example:

    ./waf configure --docs
    ./waf
