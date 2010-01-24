This is user contributed win32 configuration for building redland
using MS Windows development environments.

The files here are *.vcproj and msvc.def files for MS Visual Studio
8, provided by John Barstow.

The project files assume that raptor and rasqal have been installed
or compiled in sibling directories, following the directory structure
of the redland release.  The project file may also depend on external
libraries such as SleepyCat, MySQL to provide other functionality.
See librdf.vcproj for the exact paths used, which can be
version-number dependant.

I do not test this configuration since I don't use Windows.  I am
happy to receive patches to fix things though.

Dave
2005-05-19
