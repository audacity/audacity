********************************************************************
*                                                                  *
* THIS FILE IS PART OF THE OggVorbis SOFTWARE CODEC SOURCE CODE.   *
* USE, DISTRIBUTION AND REPRODUCTION OF THIS LIBRARY SOURCE IS     *
* GOVERNED BY A BSD-STYLE SOURCE LICENSE INCLUDED WITH THIS SOURCE *
* IN 'COPYING'. PLEASE READ THESE TERMS BEFORE DISTRIBUTING.       *
*                                                                  *
* THE OggVorbis SOURCE CODE IS (C) COPYRIGHT 1994-2011             *
* by the Xiph.Org Foundation http://www.xiph.org/                  *
*                                                                  *
********************************************************************

= WHAT'S HERE =

This source distribution includes libogg and nothing else. Other modules
(eg, the modules libvorbis, vorbis-tools for the Vorbis music codec,
libtheora for the Theora video codec) contain the codec libraries for
use with Ogg bitstreams.

Directory:

./src  		The source for libogg, a BSD-license inplementation of
		the public domain Ogg bitstream format

./include       Library API headers

./doc           Ogg specification and libogg API documents

./win32		Win32 projects and build automation

./macosx	Mac OS X project and build files

= WHAT IS OGG? =

Ogg project codecs use the Ogg bitstream format to arrange the raw,
compressed bitstream into a more robust, useful form.  For example,
the Ogg bitstream makes seeking, time stamping and error recovery
possible, as well as mixing several sepearate, concurrent media
streams into a single physical bitstream.

= CONTACT =

The Ogg homepage is located at 'https://www.xiph.org/ogg/'.
Up to date technical documents, contact information, source code and
pre-built utilities may be found there.

BUILDING FROM TARBALL DISTRIBUTIONS:

./configure
make

and optionally (as root):
make install

This will install the Ogg libraries (static and shared) into
/usr/local/lib, includes into /usr/local/include and API
documentation into /usr/local/share/doc.

BUILDING FROM REPOSITORY SOURCE:

A standard svn build should consist of nothing more than:

./autogen.sh
make

and as root if desired :

make install

BUILDING ON WIN32:

Use the project file in the win32 directory. It should compile out of the box.

CROSS COMPILING FROM LINUX TO WIN32:

It is also possible to cross compile from Linux to windows using the MinGW
cross tools and even to run the test suite under Wine, the Linux/*nix
windows emulator.

On Debian and Ubuntu systems, these cross compiler tools can be installed
by doing:

    sudo apt-get mingw32 mingw32-binutils mingw32-runtime wine

Once these tools are installed its possible to compile and test by
executing the following commands, or something similar depending on
your system:

    ./configure --host=i586-mingw32msvc --target=i586-mingw32msvc \
         --build=i586-linux
    make
    make check

(Build instructions for Ogg codecs such as vorbis are similar and may
be found in those source modules' README files)

$Id: README 18096 2011-09-22 23:32:51Z giles $
