#!/bin/bash


# Check where we're being run from.
if test -d Octave ; then
	cd Octave
	octave_src_dir=$(pwd)
elif test -z "$octave_src_dir" ; then
	echo
	echo "Error : \$octave_src_dir is undefined."
	echo
	exit 1
else
	octave_src_dir=$(cd $octave_src_dir && pwd)
	fi

# Find libsndfile shared object.
libsndfile_lib_location=""

if test -f "../src/.libs/libsndfile.so" ; then
	libsndfile_lib_location="../src/.libs/"
elif test -f "../src/libsndfile.so" ; then
	libsndfile_lib_location="../src/"
elif test -f "../src/.libs/libsndfile.dylib" ; then
	libsndfile_lib_location="../src/.libs/"
elif test -f "../src/libsndfile.dylib" ; then
	libsndfile_lib_location="../src/"
else
	echo
	echo "Not able to find the libsndfile shared lib we've just built."
	echo "This may cause the following test to fail."
	echo
	fi

libsndfile_lib_location=`(cd $libsndfile_lib_location && pwd)`


# Find sndfile.oct
sndfile_oct_location=""

if test -f .libs/sndfile.oct ; then
	sndfile_oct_location=".libs"
elif test -f sndfile.oct ; then
	sndfile_oct_location="."
else
	echo "Not able to find the sndfile.oct binaries we've just built."
	exit 1
	fi

case `file -b $sndfile_oct_location/sndfile.oct` in
	ELF*)
		;;
	Mach*)
		echo "Tests don't work on this platform."
		exit 0
		;;
	*)
		echo "Not able to find the sndfile.oct binary we just built."
		exit 1
		;;
	esac


# Make sure the TERM environment variable doesn't contain anything wrong.
unset TERM
# echo "octave_src_dir : $octave_src_dir"
# echo "libsndfile_lib_location : $libsndfile_lib_location"
# echo "sndfile_oct_location : $sndfile_oct_location"

if test ! -f PKG_ADD ; then
	cp $octave_src_dir/PKG_ADD .
	fi

export LD_LIBRARY_PATH="$libsndfile_lib_location:$LD_LIBRARY_PATH"

octave_script="$octave_src_dir/octave_test.m"

(cd $sndfile_oct_location && octave -qH $octave_script)
res=$?
echo
exit $res
