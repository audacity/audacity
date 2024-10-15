Installation Instructions
=========================

Basic Installation
------------------

Building this software requires only Python.  To install with default options:

    ./waf configure
    ./waf
    ./waf install

You may need to become root for the install stage, for example:

    sudo ./waf install

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

Installation Directories
------------------------

The --prefix option (or the PREFIX environment variable) can be used to change
the prefix which all files are installed under.  There are also several options
allowing for more fine-tuned control, see the --help output for details.

Packaging
---------

Everything can be installed to a specific root directory by passing a --destdir
option to the install stage (or setting the DESTDIR environment variable),
which adds a prefix to all install paths.  For example:

    ./waf configure --prefix=/usr
    ./waf
    ./waf install --destdir=/tmp/package
