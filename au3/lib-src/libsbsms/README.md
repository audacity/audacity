Content
=======

libsbsms is a library for high quality time and pitch scale modification.  It uses octave subband sinusoidal modeling.

The API is found in sbsms.h.  sbsms_create is called and supplied a callback which feeds sbsms_process samples.  The pitch_create and pitch_process functions are called only if pitch shifting is required.  It simply sticks a resampler on the end of the FIFO.  

NOTE:
When reading the output from sbsms, you must determine a stopping condition for yourself, as the library zero pads the output ad infinitum and never returns 0 samples.  The simplest method for doing so is found in test.cpp.

Compiling
=========

  Call './configure && make' on the console to compile the library, 
  Then 'make install'. The latter has to be called as root.
