#!/bin/bash

# Copyright 2003, 2004, 2005 Dominic Mazzoni and Matt Brubeck
# Distributed under the GNU General Public License 2.0.
# See the file LICENSE.txt for details.
# Re-written in Bash by Richard Ash 2006 - 2013

function myrmrvf {
	# a replacement for rm -rvf that has it's output controlled
	# by the value of the first argument
	# setting it to 1 makes it verbose, to anything else makes it quiet
	if [ $1 -eq 1 ]
	then	#verbose mode
		shift
		echo "rm -rf $*"
		rm -rf $*
	else
		# quietly
		shift
		rm -rf $*
	fi
	}

function myrmvf {
	# a replacement for rm -vf that has it's output controlled
	# by the value of the first argument
	# setting it to 1 makes it verbose, to anything else makes it quiet
	if [ $1 -eq 1 ]
	then	#verbose mode
		shift
		echo "rm -f $*"
		rm -f $*
	else
		# quietly
		shift
		rm -f $*
	fi
	}

function myfindrm {
	# search the file tree removing files that match the specified pattern in
	# the second argument, with output controlled by the value of the first
	# argument.
	# setting it to 1 makes it verbose, to anything else makes it quiet
	if [ $1 -eq 1 ]; then
		find . -name "$2" -print -delete
	else
		find . -name "$2" -delete
	fi
}

function updsln  {
	# removes unwanted projects from the Windows solution
	# setting it to 1 makes it verbose, to anything else makes it quiet
	if [ $1 -eq 1 ]
	then	#verbose mode
		shift
		echo "sed -e '/$*/,/EndProject/d' win/audacity.sln >win/audacity.sln.new"
		sed -e "/$*/,/EndProject/d" win/audacity.sln >win/audacity.sln.new
		echo "mv win/audacity.sln.new win/audacity.sln"
      mv win/audacity.sln.new win/audacity.sln
	else
		# quietly
		shift
		sed -e "/$*/,/EndProject/d" win/audacity.sln >win/audacity.sln.new
      mv win/audacity.sln.new win/audacity.sln
	fi
	}

function cleanfulltree {
	# does the clean-up op on the full source tree prior to building the full
	# tarball
	printf "making distclean... "
	if [ $1 -eq 1 ]; then
		make distclean;
	else
		make distclean 2>/dev/null > /dev/null;
	fi
	status=${?}
	if [[ ${status} -eq 0 ]] ; then
		printf "Done\n"
	else
		echo "Failed to make distclean: exit status was ${status}"
		exit ${status}
	fi

	echo -n "Checking SVN status... ";
	revision="$(svnversion)"
	regex="[[:digit:]]*"
	if [[ ${revision} =~ ${regex} ]] ; then
		echo "Unmodified working copy at revision ${revision}"
	else
		echo "Modified working copy! Release tarballs must be built from an unmodified working copy! Exiting"
		exit
	fi

	printf "removing SVN directories... ";
	find . -depth -name '.svn' -execdir rm -rf '{}' ';'
	# -depth is needed to avoid find trying to examine directories it has just
	# deleted.
	# The sort of quotes used is critical!
	printf "Done\n"

	printf "removing vim / emacs temp files... ";
	myfindrm $1 "*~"
	printf "Done\n"


	printf "removing executable and other intermediate files... ";
	myrmvf $1 src/audacity src/.depend src/.gchdepend
	myfindrm $1 config.status
	myfindrm $1 config.log
	myfindrm $1 Makefile
	myfindrm $1 config.cache
	find . -depth -name 'autom4te.cache' -execdir rm -rf '{}' ';'
	myfindrm $1 aclocal.m4
	printf "Done\n"

	printf "removing orphaned symlinks in lib-src/ ... ";
	myrmvf $1 lib-src/*.a 
	printf "Done\n"

	printf "removing doxygen output files ... ";
	myrmrvf $1 dox 
	printf "Done\n"

	printf "removing unused libraries from SVN tree ..."
	myrmrvf $1 lib-src/id3lib ;
	myrmrvf $1 lib-src/portburn; 
	myrmrvf $1 lib-src/taglib;
	printf "Done\n"
}

# remove all the things we have in SVN for convenience rather than being
# necessary
function slimtree {
	printf "removing todo lists ... ";
	myrmvf $1 todo.txt
	printf "Done\n"

	# we cannot remove tests/ because subsequent builds fail ...
	printf "removing scripts and tests ... ";
	myrmrvf $1 scripts tests/ProjectCheckTests/
	printf "Done\n"

	printf "removing libraries that should be installed locally..."
	myrmrvf $1 lib-src/expat lib-src/libflac lib-src/libid3tag; 
	myrmrvf $1 lib-src/liblrdf lib-src/libmad lib-src/libogg lib-src/libraptor;
	myrmrvf $1 lib-src/libsamplerate lib-src/libsndfile;
	myrmrvf $1 lib-src/libvorbis lib-src/redland lib-src/slv2 lib-src/soundtouch;
	myrmrvf $1 lib-src/twolame;
	# these bindings aren't built by default, we don't need them
	myrmrvf $1 lib-src/portaudio-v19/bindings/;
	printf "Done\n"

	printf "removing qa ... ";
	myrmrvf $1 qa 
	printf "Done\n"

	printf "removing unused portaudio-v19 directories ... ";
	myrmrvf $1 lib-src/portaudio-v19/docs
	myrmrvf $1 lib-src/portaudio-v19/pa_asio
	myrmrvf $1 lib-src/portaudio-v19/pa_sgi
	myrmrvf $1 lib-src/portaudio-v19/pa_mac_sm
	myrmrvf $1 lib-src/portaudio-v19/test
	myrmrvf $1 lib-src/portaudio-v19/testcvs
	printf "Done\n"

	printf "removing wxstd locale files (since they come with wxWidgets)\n";
	myrmrvf $1 locale/wxstd

	printf "removing Nyquist plug-ins that are just for show ... "
	myrmvf $1 plug-ins/analyze.ny plug-ins/fadein.ny plug-ins/fadeout.ny
	myrmvf $1 plug-ins/undcbias.ny
	printf "Done\n"
	
	printf "Removing developer scripts not needed to build audacity ..."
	myrmrvf $1 scripts/mw2html_audacity 
	printf "Done\n"
}

function fixwinforslim {
	printf "Changing Windows header so that it doesn't try to build with\n";
	printf "support for optional libraries by default.\n";

	echo "" >> "win/configwin.h"
	echo "// The Audacity source tarball does NOT come with" >> "win/configwin.h"
	echo "// any optional libraries." >> "win/configwin.h"
	echo "" >> "win/configwin.h"
	echo "// Delete the following lines if you install them manually." >> "win/configwin.h"
	echo "" >> "win/configwin.h"
	echo "#undef MP3SUPPORT" >> "win/configwin.h"
	echo "#undef USE_FFMPEG" >> "win/configwin.h"
	echo "#undef USE_LIBFLAC" >> "win/configwin.h"
	echo "#undef USE_LIBID3TAG" >> "win/configwin.h"
	echo "#undef USE_LIBLRDF" >> "win/configwin.h"
	echo "#undef USE_LIBMAD" >> "win/configwin.h"
	echo "#undef USE_LIBSAMPLERATE" >> "win/configwin.h"
	echo "#undef USE_LIBTWOLAME" >> "win/configwin.h"
	echo "#undef USE_LIBVORBIS" >> "win/configwin.h"
	echo "#undef USE_SLV2" >> "win/configwin.h"
	echo "#undef USE_SOUNDTOUCH" >> "win/configwin.h"
	echo "#undef EXPERIMENTAL_SCOREALIGN" >> "win/configwin.h"


	printf "removing unwanted projects from VC++ solution\n"
	updsln $mode libflac
	updsln $mode libflac++
	updsln $mode libid3tag
	updsln $mode libmad
	updsln $mode liblrdf
	updsln $mode librdf
	updsln $mode libsamplerate
	updsln $mode twolame
	updsln $mode libvorbis
	updsln $mode libogg
	updsln $mode raptor
	updsln $mode rasqal
	updsln $mode slv2
	updsln $mode soundtouch
	updsln $mode libscorealign
}

echo "Maketarball 2.1.0 -- make an Audacity distribution tarball"

# check number of arguments, if not one then print a usage message
if [ $# == 1 ]; then
	:
	# carry on, looks like they know what they are doing
else
    echo "Script to make directory trees for audacity source tarballs"
    echo "Usage: $0 <mode>"
	echo "Where mode is either \"quiet\" or \"verbose\""
	exit 1
fi

if [ "$1" = "quiet" ]; then
		mode=0
elif [ "$1" = "verbose" ]; then
		mode=1
else
	echo "The argument to $0 must be either \"quiet\" or \"verbose\""
	exit 1
fi

if [ -f "src/Audacity.h" ]
then :
	# a null body - we will just carry straight on
else
	echo "$0 must be run from top-level audacity directory"
	exit 1
fi

# The script relies on make working, so Makefiles need to be present. This
# means that configure must have been run on the sources. In general it doesn't 
# matter what options, but the generation of a Makefile in lib-src/ in
# particular is important. Check that lib-src/Makefile is present and newer than
# lib-src/Makefile.in before continuing

# Mac OS X also has problems if libsndfile isn't configured with automake
# dependency turned off, so we should check that libsndfile is clean, and ask
# for reconfiguration if not.
reconf=0
if [[ -f "lib-src/Makefile" ]] ; then
	# we have a Makefile - is it new enough?
	t2=$(date +%s -r "lib-src/Makefile")
	t1=$(date +%s -r "lib-src/Makefile.in")
	if [[ $t1 -gt $t2 ]] ; then
		# not new enough, reconfigure
		reconf=1
	fi
else
	# if no Makefile, definitly need to configure
	reconf=1
fi

# these are the arguments we will pass to configure when it is run
configargs="--enable-maintainer-mode"

if [[ x"$reconf" = x1 ]] ; then
	echo "Your Makefiles are out of date or missing. (Re)running configure to"
	echo "create up-to-date Makefiles before building tarballs..."
	echo "   ./configure ${configargs}"

   # if we are in silent mode, then redirect the output of configure
   if [ $mode -eq 1 ]; then
      $SHELL -c "./configure ${configargs}"
   else
      $SHELL -c "./configure ${configargs}" > /dev/null 2>&1
   fi
   if [[ ${?} -ne 0 ]] ; then
      echo "Error - configure exited with non-zero status!"
	  exit 1
   fi
fi

# The version number is stored in a C++ header as a set of #defines. Trying to 
# parse this with another language (as was done first with Perl and then with
# awk) is always going to be fragile, so we use a C++ pre-processor (which
# strangely enough we are pretty much garunteed to have) to do it. Essentially
# we have a trivial bit of C++ code stored in-line in this script which is fed
# through the pre-processor to get the version string components where we can
# find them.

if [[ ! -x "config.status" ]] ; then
   echo "config.status is not present or executable - cannot proceed"
   exit 1
fi
echo -n "Getting program version... "
# first off, find out what C++ pre-processor configure has found for us to use
# (because we want the same one that will be used to build Audacity). This is a
# neat trick using the config.status script left behind after configure has
# been run
cppprog="$(echo '@CXX@' | ./config.status --file=-)"

# now create a temporary directory where out source file will live
cppdir="$(mktemp -d mktarball.XXXXXXXXXXX)"
cppfile="${cppdir}/ver.cpp"
# create the test source file
cat > "${cppfile}" << EOFCPP
#include "$(pwd)/src/Audacity.h"
AUDACITY_VERSION
AUDACITY_RELEASE
AUDACITY_REVISION
AUDACITY_SUFFIX
EOFCPP

# pre-process the source (to get macros expanded) and parse the result
major_version=$(${cppprog} "${cppfile}" -E | tail -n 4 | head -n 1)
minor_version=$(${cppprog} "${cppfile}" -E | tail -n 3 | head -n 1)
micro_version=$(${cppprog} "${cppfile}" -E | tail -n 2 | head -n 1)
# this gets the text out of the wxT("") macro it is already wrapped in
version_suffix=$(${cppprog} "${cppfile}" -E | tail -n 1 | cut -d\" -f 2)
# see if the suffix contains a compiler date macro
${cppprog} "${cppfile}" -E | tail -n 1 | grep -v -q '__TDATE__'
version_date=$?

if [[ ${version_date} -ne 0 ]] ; then
		# need to suffix the date on to the suffix string
		version_suffix="${version_suffix}$(date "+%Y%m%d")"
fi
# remove that temp directory and the associated files
rm -fr "${cppdir}"

version="${major_version}.${minor_version}.${micro_version}${version_suffix}"
echo "${version}"
# completes previous echo -n call

# capture some directory information, we'll want it later
sourcedir="$(pwd)"	# where the sources are
cd ..
topdir="$(pwd)"	# one level up where the tarballs come out
tarname="audacity-src-${version}" # the directory we will find inside tarballs
tardir="${topdir}/${tarname}"	# absolute tar directory

printf "making copy of source directory... "
cd "${sourcedir}"	# go to sources dir
cp -r . "${tardir}"
cd "${tardir}"
printf "Done\n"

# now clean out the directory of all the things we don't need in the
# tarball, prior to building the source tarball
cleanfulltree $mode;

# now we have the full source tree, lets slim it down to the bits that 
# you actually need to build audacity on a shared library system with the
# relevant libraries installed on the system (e.g. Linux distros)
slimtree $mode;

# Fix up windows builds for the slimmed state of the tarball
fixwinforslim ${mode}

# Tar up that lot as the source tarball
cd "${topdir}"
printf "Creating source tarball .... "
tar cf "audacity-minsrc-${version}.tar" "${tarname}" 
printf "Done\n"

printf "Compressing source tarball .... "
xz "audacity-minsrc-${version}.tar" 

cd "${tardir}"
printf "Done\n"
