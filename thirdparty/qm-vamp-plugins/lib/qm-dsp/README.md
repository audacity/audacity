
QM-DSP library
==============

This is a C++ library of functions for Digital Signal Processing and
Music Informatics purposes developed in the [Centre for Digital
Music](http://c4dm.eecs.qmul.ac.uk) at Queen Mary, University of
London.

It is used by [QM Vamp Plugins](http://isophonics.net/QMVampPlugins)
amongst other things.

Despite the assertive name "qm-dsp", it is not "the official QM DSP
library", just one library for DSP that happens to have been written
at QM. It got this name because nothing else was using it at the time.


Compiling the library
---------------------

 - Linux: `make -f build/linux/Makefile.linux64`

 - Mac: `make -f build/osx/Makefile.osx`

 - Windows (MSVC): Use the project file `build/msvc/QMDSP.vcxproj`

To build and run unit tests as well, add the `test` target to your
Make invocation, e.g. `make -f build/linux/Makefile.linux64
test`. Tests require the Boost library.


Licence
-------

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.  See the file COPYING included with
this distribution for more information.

This code is Copyright (c) 2006-2019 Queen Mary, University of London,
with the following exceptions:

 - `ext/kissfft` - Copyright (c) 2003-2010 Mark Borgerding

 - `maths/pca/pca.c` - Fionn Murtagh, from StatLib, used with permission

 - `maths/Polyfit.h` - by Allen Miller, David J Taylor and others;
also for Delphi in the the JEDI Math Library, under the Mozilla Public
License

 - `thread/BlockAllocator.h` - derived from FSB Allocator by Juha
Nieminen, under a BSD-style license

See individual files for further authorship details.

If you wish to use this code in a proprietary application or product
for which the terms of the GPL are not appropriate, please contact QM
Innovation https://www.qminnovation.co.uk/ for licensing terms.
