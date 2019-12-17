Installation Instructions
=========================

Basic Installation
------------------

Building this software requires only Python.  To install with default options:

    ./waf configure
    ./waf
    ./waf install # or sudo ./waf install

Configuration Options
---------------------

All supported options can be viewed using the command:

    ./waf --help

Most options only need to be passed during the configure stage, for example:

    ./waf configure --prefix=/usr
    ./waf
    ./waf install

Compiler Configuration
----------------------

Several standard environment variables can be used to control how compilers are
invoked:

 * CC:        Path to C compiler
 * CFLAGS:    C compiler options
 * CXX:       Path to C++ compiler
 * CXXFLAGS:  C++ compiler options
 * CPPFLAGS:  C preprocessor options
 * LINKFLAGS: Linker options

Library Versioning
------------------

This library uses semantic versioning <http://semver.org/>.

Several major versions can be installed in parallel.  The shared library name,
include directory, and pkg-config file are suffixed with the major version
number.  For example, a library named "foo" at version 1.x.y might install:

    /usr/include/foo-1/foo/foo.h
    /usr/lib/foo-1.so.1.x.y
    /usr/lib/pkgconfig/foo-1.pc

Dependencies can check for the package "foo-1" with pkg-config.

Packaging
---------

Everything can be installed to a specific root directory by passing a --destdir
option to the install stage (or setting the DESTDIR environment variable),
which adds a prefix to all install paths.  For example:

    ./waf configure --prefix=/usr
    ./waf
    ./waf install --destdir=/tmp/package

Packages should allow parallel installation of several major versions.  For
example, the above would be packaged as "foo-1".