libresample

Real-time library interface by Dominic Mazzoni

Based on resample-1.7:
  http://www-ccrma.stanford.edu/~jos/resample/

License: LGPL - see the file LICENSE.txt for more information

History:

This library is not the highest-quality resampling library
available, nor is it the most flexible, nor is it the
fastest.  But it is pretty good in all of these regards, and
it is quite portable.  The best resampling library I am aware
of is libsamplerate by Erik de Castro Lopo.  It's small, fast,
and very high quality.  However, it uses the GPL for its
license (with commercial options available) and I needed
a more free library.  So I wrote this library, using
the LGPL resample-1.7 library by Julius Smith as a basis.

Resample-1.7 is a fixed-point resampler, and as a result
has only limited precision.  I rewrote it to use single-precision
floating-point arithmetic instead and increased the number
of filter coefficients between time steps significantly.
On modern processors it can resample in real time even
with this extra overhead.

Resample-1.7 was designed to read and write from files, so
I removed all of that code and replaced it with an API that
lets you pass samples in small chunks.  It should be easy
to link to resample-1.7 as a library.

Changes in version 0.1.3:

* Fixed two bugs that were causing subtle problems
  on Intel x86 processors due to differences in roundoff errors.

* Prefixed most function names with lrs and changed header file
  from resample.h to libresample.h, to avoid namespace
  collisions with existing programs and libraries.

* Added resample_dup (thanks to Glenn Maynard)

* Argument to resample_get_filter_width takes a const void *
  (thanks to Glenn Maynard)

* resample-sndfile clips output to -1...1 (thanks to Glenn Maynard)

Usage notes:  

- If the output buffer you pass is too small, resample_process
  may not use any input samples because its internal output
  buffer is too full to process any more.  So do not assume
  that it is an error just because no input samples were
  consumed.  Just keep passing valid output buffers.

- Given a resampling factor f > 1, and a number of input
  samples n, the number of output samples should be between
  floor(n - f) and ceil(n + f).  In other words, if you
  resample 1000 samples at a factor of 8, the number of
  output samples might be between 7992 and 8008.  Do not
  assume that it will be exactly 8000.  If you need exactly
  8000 outputs, pad the input with extra zeros as necessary.

License and warranty:

All of the files in this package are Copyright 2003 by Dominic
Mazzoni <dominic@minorninth.com>.  This library was based heavily
on Resample-1.7, Copyright 1994-2002 by Julius O. Smith III
<jos@ccrma.stanford.edu>, all rights reserved.

Permission to use and copy is granted subject to the terms of the
"GNU Lesser General Public License" (LGPL) as published by the
Free Software Foundation; either version 2.1 of the License,
or any later version.  In addition, Julius O. Smith III requests
that a copy of any modified files be sent by email to
jos@ccrma.stanford.edu so that he may incorporate them into the
CCRMA version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
