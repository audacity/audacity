
Contributing
============

The qm-dsp library is maintained in a Github repository at
https://github.com/c4dm/qm-dsp.


Reporting bugs
--------------

Please use Github issues for bug reports. Try to make them as specific
as possible. For example, describe an input that triggers some
particular behaviour, and tell us how that behaviour differs from what
you expected.

If your bug can be reproduced by processing an audio file using one of
the QM Vamp Plugins (https://github.com/c4dm/qm-vamp-plugins), which
are built using this library, that might be a good way to illustrate
the problem.


Pull requests
-------------

We're happy to see pull requests, and can pull them directly in some
circumstances.

 * Please make sure your change compiles without warnings and passes
   the existing tests.

 * Please follow the code style guidelines (see below).

 * Please make it as easy as possible to verify the behaviour of the
   pull request, for example by adding a new test in the `tests`
   directory. This library has only limited test coverage, but we
   would like to expand it, and prefer not to make changes unless they
   are backed by tests.

 * Please provide your changes under terms which permit Queen Mary
   University of London to relicense the code for commercial
   purposes. The qm-dsp library as a whole is provided under the GPL,
   but QM also make commercial licences available separately, and
   cannot accept any pull request whose copyright status would prevent
   that. In practice, this means any non-trivial change not
   originating from QM must be explicitly licensed using a BSD-like
   licence text, either in the source file itself or in an
   accompanying file. See `thread/BlockAllocator.h` for an example of
   typical language.

Please note also that fixes which change the behaviour of the existing
QM Vamp Plugins will need particularly close scrutiny - these are
reasonably widely used and, even where they have defects, changes may
cause problems for users and will at least need to be documented with
the plugins. For this reason it may take some time for such changes to
be reviewed or integrated.


Code style
----------

 * C++ code must compile with the C++98 standard, except for the unit
   tests which are C++14

 * Classes are named `LikeThis` - functions, methods, and local
   variables `likeThis` - class member variables `m_likeThis`

 * Indentation is four spaces at a time (no tabs)

 * The opening brace for a block goes at the end of the line, except
   at the start of a function or class definition where it gets a line
   of its own

 * Please use braces around any conditional or loop block that is not
   on the same line as the test

 * Please keep lines to no more than 80 characters in length

 * Avoid using unsigned int types, unless doing bit manipulation (see
   http://soundsoftware.ac.uk/c-pitfall-unsigned.html for rationale)

