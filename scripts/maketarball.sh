#!/bin/bash

# Copyright 2003, 2004, 2005 Dominic Mazzoni and Matt Brubeck
# Distributed under the GNU General Public License 2.0.
# See the file LICENSE.txt for details.
# Re-written in Bash by Richard Ash 2006 - 2013

function myrmrvf {
   # a replacement for rm -rvf that has it's output controlled
   # by the value of the first argument
   # setting it to 1 makes it verbose, to anything else makes it quiet
   if [ $1 -eq 1 ] ; then  #verbose mode
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
   if [ $1 -eq 1 ] ; then  #verbose mode
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
   if [ $1 -eq 1 ] ; then
      find . -name "$2" -print -delete
   else
      find . -name "$2" -delete
   fi
}

function cleanfulltree {
   # does the clean-up op on the full source tree prior to building the full
   # tarball
   printf "making distclean ... "
   if [ $1 -eq 1 ] ; then
      make distclean
   else
      make distclean 2>/dev/null > /dev/null
   fi
   status=${?}
   if [ ${status} -eq 0 ] ; then
      printf "Done\n"
   else
      echo "Failed to make distclean: exit status was ${status}"
      exit ${status}
   fi

   printf "removing GIT directories ... "
   myrmrvf $1 .git .gitignore
   myrmrvf $1 .gitignore
   myrmrvf $1 .gitattributes
   printf "Done\n"

   printf "removing vim / emacs temp files ... "
   myfindrm $1 "*~"
   printf "Done\n"

   printf "removing Python droppings ... "
   myfindrm $1 "*.pyc"
   printf "Done\n"

   printf "removing executable and other intermediate files ... "
   myrmvf $1 src/audacity src/.depend src/.gchdepend
   myfindrm $1 config.status
   myfindrm $1 config.log
   myfindrm $1 config.cache
   find . -depth -name 'autom4te.cache' -execdir rm -rf '{}' ';'
   find . -depth -name '.deps' -execdir rm -rf '{}' ';'
   myfindrm $1 aclocal.m4
   printf "Done\n"

   printf "removing orphaned symlinks in lib-src/ ... "
   myrmvf $1 lib-src/*.a 
   printf "Done\n"

   printf "removing doxygen output files ... "
   myrmrvf $1 dox 
   printf "Done\n"

   printf "removing unused libraries from GIT tree ..."
   myrmrvf $1 lib-src/libscorealign
   printf "Done\n"
}

# remove all the things we have in GIT for convenience rather than being
# necessary
function slimtree {
   printf "removing todo lists ... "
   myrmvf $1 todo.txt
   printf "Done\n"

   # we cannot remove tests/ because subsequent builds fail ...
   printf "removing scripts and tests ... "
   myrmrvf $1 scripts tests/ProjectCheckTests/
   printf "Done\n"

   printf "removing libraries that should be installed locally ... "
   myrmrvf $1 lib-src/expat lib-src/libid3tag
   myrmrvf $1 lib-src/libmad lib-src/libogg
   myrmrvf $1 lib-src/libvorbis lib-src/soundtouch
   # these bindings aren't built by default, we don't need them
   myrmrvf $1 lib-src/portaudio-v19/bindings/
   printf "Done\n"

   printf "removing qa ... "
   myrmrvf $1 qa 
   printf "Done\n"

   printf "removing unused portaudio-v19 directories ... "
   myrmrvf $1 lib-src/portaudio-v19/docs
   myrmrvf $1 lib-src/portaudio-v19/pa_asio
   myrmrvf $1 lib-src/portaudio-v19/pa_sgi
   myrmrvf $1 lib-src/portaudio-v19/pa_mac_sm
   myrmrvf $1 lib-src/portaudio-v19/testcvs
   printf "Done\n"

   printf "removing unused portmidi directories ... "
   myrmrvf $1 lib-src/portmidi/pm_cl
   myrmrvf $1 lib-src/portmidi/pm_csharp
   myrmrvf $1 lib-src/portmidi/pm_dylib
   myrmrvf $1 lib-src/portmidi/pm_java
   myrmrvf $1 lib-src/portmidi/pm_mingw
   myrmrvf $1 lib-src/portmidi/pm_python
   myrmrvf $1 lib-src/portmidi/pm_qt
   myrmrvf $1 lib-src/portmidi/pm_test
   myrmrvf $1 lib-src/portmidi/portmidi_cdt.zip
   printf "Done\n"

   printf "removing Nyquist plug-ins that are just for show ... "
   myrmvf $1 plug-ins/analyze.ny plug-ins/fadein.ny plug-ins/fadeout.ny
   myrmvf $1 plug-ins/undcbias.ny
   printf "Done\n"
   
   printf "Removing developer scripts not needed to build audacity ... "
   myrmrvf $1 scripts/mw2html_audacity 
   printf "Done\n"

   printf "Removing Mac and Windows build files ... "
   myrmrvf $1 mac
   myrmrvf $1 win
   printf "Done\n"
}

echo "Maketarball 2.1.0 -- make an Audacity distribution tarball"

# check number of arguments, if not one then print a usage message
if [ $# -ne 1 ] ; then
   echo "Script to make directory trees for audacity source tarballs"
   echo "Usage: $0 <mode>"
   echo "Where mode is either \"quiet\" or \"verbose\""
   exit 1
fi

if [ "$1" = "quiet" ] ; then
   mode=0
elif [ "$1" = "verbose" ] ; then
   mode=1
else
   echo "The argument to $0 must be either \"quiet\" or \"verbose\""
   exit 1
fi

if [ ! -f "src/Audacity.h" ] ; then
   echo "$0 must be run from top-level audacity directory"
   exit 1
fi

# capture some directory information, we'll want it later
sourcedir="$(pwd)"   # where the sources are
cd ..
topdir="$(pwd)"   # one level up where the tarballs come out
tmpsrc="${topdir}/$(mktemp -d audacity-src-XXXXXX)" # where initial modifications are done

printf "making copy of source directory ... "
cp -pr "${sourcedir}/." "${tmpsrc}"
cd "${tmpsrc}"
printf "Done\n"

# The script relies on make working, so Makefiles need to be present. This
# means that configure must have been run on the sources. In general it doesn't 
# matter what options, but the generation of a Makefile in lib-src/ in
# particular is important. Check that lib-src/Makefile is present and newer than
# lib-src/Makefile.in before continuing

# Mac OS X also has problems if libsndfile isn't configured with automake
# dependency turned off, so we should check that libsndfile is clean, and ask
# for reconfiguration if not.
reconf=0
if [ -f "lib-src/Makefile" ] ; then
   # we have a Makefile - is it new enough?
   t2=$(date +%s -r "lib-src/Makefile")
   t1=$(date +%s -r "lib-src/Makefile.in")
   if [ $t1 -gt $t2 ] ; then
      # not new enough, reconfigure
      reconf=1
   fi
else
   # if no Makefile, definitely need to configure
   reconf=1
fi

# these are the arguments we will pass to configure when it is run
configargs="--enable-maintainer-mode"

if [ $reconf -eq 1 ] ; then
   echo "Your Makefiles are out of date or missing. (Re)running configure to"
   echo "create up-to-date Makefiles before building tarballs..."
   echo "   ./configure ${configargs}"

   # if we are in silent mode, then redirect the output of configure
   if [ $mode -eq 1 ] ; then
      $SHELL -c "./configure ${configargs}"
   else
      $SHELL -c "./configure ${configargs}" > /dev/null 2>&1
   fi
   if [ ${?} -ne 0 ] ; then
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

if [ ! -x "config.status" ] ; then
   echo "config.status is not present or executable - cannot proceed"
   exit 1
fi

echo -n "Getting program version ... "
# first off, find out what C++ pre-processor configure has found for us to use
# (because we want the same one that will be used to build Audacity). This is a
# neat trick using the config.status script left behind after configure has
# been run
cppprog="$(echo '@CXX@' | ./config.status --file=-)"

# run the preprocessor, convert output to shell variables, and evaluate to
# define them
eval $(cpp -E <<CPPEOF | sed -e 's/wxT("//g' \
                             -e 's/")//g' \
                             -e 's/ //g' \
                             -e "s/__TDATE__/$(date +%Y%m%d)/" \
                             -e 's/=/="/' \
                             -e 's/$/"/' \
                             -e '/^v_/p' \
                             -e 'd'
#include "src/Audacity.h"
v_major=AUDACITY_VERSION
v_minor=AUDACITY_RELEASE
v_micro=AUDACITY_REVISION
v_suffix=AUDACITY_SUFFIX
CPPEOF
)

version="${v_major}.${v_minor}.${v_micro}${v_suffix}"
printf "${version}\n"

# now clean out the directory of all the things we don't need in the
# tarball, prior to building the source tarball
cleanfulltree $mode

# now we have the full source tree, lets slim it down to the bits that 
# you actually need to build audacity on a shared library system with the
# relevant libraries installed on the system (e.g. Linux distros)
slimtree $mode

# Rename the source tree to the versioned name
cd "${topdir}"
printf "Renaming source tree ... "
tarname="audacity-minsrc-${version}" # the directory we will find inside tarballs
mv "${tmpsrc}" "${tarname}"
printf "Done\n"

# Tar up that lot as the source tarball
printf "Creating source tarball ... "
tar cf "${tarname}.tar" "${tarname}" 
printf "Done\n"

printf "Compressing source tarball ... "
xz "${tarname}.tar" 
cd "${tarname}"
printf "Done\n"
