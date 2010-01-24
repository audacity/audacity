This is user contributed win32 configuration for building raptor
using MS Windows development environments.

The latest files are the *.vcproj and *.sln files for MS Visual
Studio 8, provided by John Barstow.

The *.dsp *.dsw files are older and from MS Developer Studio provided
by several prople.

The various project files assume that (iconv, libxml, libxml2) or
expat are available as well as curl have been installed or compiled
in sibling top level directories. See the raptor.vcproj (newest) or
raptor.dsp (older) files for the exact paths used, which are
version-number dependant.

It should be relatively easy to change raptor between using libxml2
and expat.  See ..\win32_raptor_config.h near:
/* For using expat on win32 */
...
#else
/* For using libxml2 on win32 */

and pick one path.


I do not test this configuration since I don't use Windows.  I am
happy to receive patches to fix things though.

Dave
2005-05-19
