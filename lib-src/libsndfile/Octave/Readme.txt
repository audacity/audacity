The libsndfile Modules for GNU Octave
=====================================

These modules are currently known to work with version 3.0 of GNU Octave on
Linux. They have not been tested elsewhere.


Build Requirements
------------------

In order to build these libsndfile related modules for GNU Octave on a Debian
GNU/Linux (or Debian derived) system, you will need (on top of what is normally
required to build libsndfile) the package:

	octaveX.Y-headers

where X.Y matches the version number of your installation of GNU Octave.

The configure script in the top level libsndfile directory will detect the 
presence and correct versions of the Octave build tools. The building of these
modules will only go ahead if everything is correct.


