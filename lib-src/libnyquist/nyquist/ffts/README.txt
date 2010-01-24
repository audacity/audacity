This directory contains a public domain FFT library which was optimized
for speed on RISC processors such as the PowerPC.  All ffts
use single precision floats, for double precision just use a
global search and replace to change float to double in all 
source files.
Codewarrier Pro 1.0 project files are also supplied.

** Warning **   Perform rigorous testing to
your own standards before using this code.

 (John Green) green_jt@vsdec.npt.nuwc.navy.mil

files:
	fftTiming
Application to time complex ffts

	rfftTiming
Application to time real ffts

// Directory: fft libraries

files:

	fftext.c
Library of in-place fast fourier transforms. Contains forward 
and inverse complex and real transforms.  The real fft's expect the
frequency domain data to have the real part of the fsamp/2 bin (which
has a 0 imaginary part) to be stored in the location for the imaginary
part of the DC bin (the DC bin of real data is also strictly real.)
You must first call an initialization routine fftInit  before calling 
the fft computation routines ffts, iffts, rffts and riffts.
The init routines malloc the memory to store the cosine and
bit reversed counter tables as well as initializing their values.

	fftlib.c
Lower level library of in-place fast fourier transforms. Same as fftext.c but you
need to manage the mallocs for the cosine and bit reversed tables yourself.


	fft2d.c
Library of 2d and 3d complex and 2d real in-place fast fourier transforms.
The init routine fft2dInit must be called before using the 2d routines and
fft3dInit must be called before using the 3d routines.  These init routines
will also call the appropriate 1d init routines in fftext.c

	matlib.c
Matrix transpose routines used by fft2d.c and complex vector multiply
for forming the product of two spectra.

	dxpose.c
Double precision matrix transpose for quick single precision complex transposing

// Directory: timing code
This directory contains the source to fftTiming and rfftTiming

// Directory: Numerical Recipes testing
This directory contains files used to test the various fft routines using
the Numerical Recipes in C routines as a baseline.  These routines can be purchased
in PeeCee (after expanding you can move them to a Mac) format from:
http://cfata2.harvard.edu/numerical-recipes/
Unfortunately Numerical Recipes defines its forward and inverse fft's backwards.
For complex fft's I just use their inverse fft as a forward one, but for real ffts
their forward fft followed by my inverse fft reverses the data.  They also have ugly matrix
and tensor data types and start their indices with one, Fortran style, but these are
minor annoyances.

// Directory: Matlab testing
This directory contains files to test fast 1d and 2d convolution with Matlab used to
verify the results.  An example of using Matlab to test the fft library routines is
also given for the 2d real fft.
