#!/bin/sh

#  FLAC - Free Lossless Audio Codec
#  Copyright (C) 2002-2009  Josh Coalson
#  Copyright (C) 2011-2013  Xiph.Org Foundation
#
#  This file is part the FLAC project.  FLAC is comprised of several
#  components distributed under different licenses.  The codec libraries
#  are distributed under Xiph.Org's BSD-like license (see the file
#  COPYING.Xiph in this distribution).  All other programs, libraries, and
#  plugins are distributed under the GPL (see COPYING.GPL).  The documentation
#  is distributed under the Gnu FDL (see COPYING.FDL).  Each file in the
#  FLAC distribution contains at the top the terms under which it may be
#  distributed.
#
#  Since this particular file is relevant to all components of FLAC,
#  it may be distributed under the Xiph.Org license, which is the least
#  restrictive of those mentioned above.  See the file COPYING.Xiph in this
#  distribution.

die ()
{
	echo $* 1>&2
	exit 1
}

if [ x = x"$1" ] ; then
	BUILD=debug
else
	BUILD="$1"
fi

# change to 'false' to show all flac/metaflac output (useful for debugging)
if true ; then
	SILENT='--silent'
	TOTALLY_SILENT='--totally-silent'
else
	SILENT=''
	TOTALLY_SILENT=''
fi

LD_LIBRARY_PATH=`pwd`/../src/libFLAC/.libs:$LD_LIBRARY_PATH
LD_LIBRARY_PATH=`pwd`/../src/share/grabbag/.libs:$LD_LIBRARY_PATH
LD_LIBRARY_PATH=`pwd`/../src/share/getopt/.libs:$LD_LIBRARY_PATH
LD_LIBRARY_PATH=`pwd`/../src/share/replaygain_analysis/.libs:$LD_LIBRARY_PATH
LD_LIBRARY_PATH=`pwd`/../src/share/replaygain_synthesis/.libs:$LD_LIBRARY_PATH
LD_LIBRARY_PATH=`pwd`/../src/share/utf8/.libs:$LD_LIBRARY_PATH
LD_LIBRARY_PATH=`pwd`/../objs/$BUILD/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH
export MALLOC_CHECK_=3
export MALLOC_PERTURB_=$(($(date +%s) % 255 + 1))
PATH=`pwd`/../src/flac:$PATH
PATH=`pwd`/../src/metaflac:$PATH
PATH=`pwd`/../objs/$BUILD/bin:$PATH

if echo a | (grep -E '(a|b)') >/dev/null 2>&1
	then EGREP='grep -E'
	else EGREP='egrep'
fi

testdir="metaflac-test-files"
flacfile="metaflac.flac"

flac --help 1>/dev/null 2>/dev/null || die "ERROR can't find flac executable"
metaflac --help 1>/dev/null 2>/dev/null || die "ERROR can't find metaflac executable"

run_flac ()
{
	if [ x"$FLAC__TEST_WITH_VALGRIND" = xyes ] ; then
		echo "valgrind --leak-check=yes --show-reachable=yes --num-callers=50 flac $*" >>test_metaflac.valgrind.log
		valgrind --leak-check=yes --show-reachable=yes --num-callers=50 --log-fd=4 flac $* 4>>test_metaflac.valgrind.log
	else
		flac $*
	fi
}

run_metaflac ()
{
	if [ x"$FLAC__TEST_WITH_VALGRIND" = xyes ] ; then
		echo "valgrind --leak-check=yes --show-reachable=yes --num-callers=50 metaflac $*" >>test_metaflac.valgrind.log
		valgrind --leak-check=yes --show-reachable=yes --num-callers=50 --log-fd=4 metaflac $* 4>>test_metaflac.valgrind.log
	else
		metaflac $*
	fi
}

run_metaflac_silent ()
{
	if [ -z "$SILENT" ] ; then
		run_metaflac $*
	else
		if [ x"$FLAC__TEST_WITH_VALGRIND" = xyes ] ; then
			echo "valgrind --leak-check=yes --show-reachable=yes --num-callers=50 metaflac $*" >>test_metaflac.valgrind.log
			valgrind --leak-check=yes --show-reachable=yes --num-callers=50 --log-fd=4 metaflac $* 2>/dev/null 4>>test_metaflac.valgrind.log
		else
			metaflac $* 2>/dev/null
		fi
	fi
}

check_flac ()
{
	run_flac --silent --test $flacfile || die "ERROR in $flacfile" 1>&2
}

echo "Generating stream..."
bytes=80000
if dd if=/dev/zero ibs=1 count=$bytes | flac --force --verify -0 --input-size=$bytes --output-name=$flacfile --force-raw-format --endian=big --sign=signed --channels=1 --bps=8 --sample-rate=8000 - ; then
	chmod +w $flacfile
else
	die "ERROR during generation"
fi

check_flac

echo

filter ()
{
	# minor danger, changing vendor strings will change the length of the
	# VORBIS_COMMENT block, but if we add "^  length: " to the patterns,
	# we lose info about PADDING size that we need
	# grep pattern 1: remove vendor string
	# grep pattern 2: remove minimum/maximum frame and block size from STREAMINFO
	# grep pattern 3: remove hexdump data from PICTURE metadata blocks
	# sed pattern 1: remove stream offset values from SEEKTABLE points
	$EGREP -v '^  vendor string: |^  m..imum .....size: |^    0000[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]: ' | sed -e 's/, stream_offset.*//'
}
metaflac_test ()
{
	case="$1"
	desc="$2"
	args="$3"
	expect="$testdir/$case-expect.meta"
	echo -n "test $case: $desc... "
	run_metaflac $args $flacfile | filter > $testdir/out.meta || die "ERROR running metaflac"
	diff -w $expect $testdir/out.meta > /dev/null 2>&1 || die "ERROR: metadata does not match expected $expect"
	echo OK
}

metaflac_test case00 "--list" "--list"

metaflac_test case01 "STREAMINFO --show-* shortcuts" "
	--show-md5sum
	--show-min-blocksize
	--show-max-blocksize
	--show-min-framesize
	--show-max-framesize
	--show-sample-rate
	--show-channels
	--show-bps
	--show-total-samples"

run_metaflac --preserve-modtime --add-padding=12345 $flacfile
check_flac
metaflac_test case02 "--add-padding" "--list"

# some flavors of /bin/sh (e.g. Darwin's) won't even handle quoted spaces, so we underscore:
run_metaflac --set-tag="ARTIST=The_artist_formerly_known_as_the_artist..." $flacfile
check_flac
metaflac_test case03 "--set-tag=ARTIST" "--list"

run_metaflac --set-tag="ARTIST=Chuck_Woolery" $flacfile
check_flac
metaflac_test case04 "--set-tag=ARTIST" "--list"

run_metaflac --set-tag="ARTIST=Vern" $flacfile
check_flac
metaflac_test case05 "--set-tag=ARTIST" "--list"

run_metaflac --set-tag="TITLE=He_who_smelt_it_dealt_it" $flacfile
check_flac
metaflac_test case06 "--set-tag=TITLE" "--list"

metaflac_test case07 "--show-vendor-tag --show-tag=ARTIST" "--show-vendor-tag --show-tag=ARTIST"

run_metaflac --remove-first-tag=ARTIST $flacfile
check_flac
metaflac_test case08 "--remove-first-tag=ARTIST" "--list"

run_metaflac --remove-tag=ARTIST $flacfile
check_flac
metaflac_test case09 "--remove-tag=ARTIST" "--list"

metaflac_test case10 "--list --block-type=VORBIS_COMMENT" "--list --block-type=VORBIS_COMMENT"
metaflac_test case11 "--list --block-number=0" "--list --block-number=0"
metaflac_test case12 "--list --block-number=1,2,999" "--list --block-number=1,2,999"
metaflac_test case13 "--list --block-type=VORBIS_COMMENT,PADDING" "--list --block-type=VORBIS_COMMENT,PADDING"
metaflac_test case14 "--list --except-block-type=SEEKTABLE,VORBIS_COMMENT" "--list --except-block-type=SEEKTABLE,VORBIS_COMMENT"
metaflac_test case15 "--list --except-block-type=STREAMINFO" "--list --except-block-type=STREAMINFO"

run_metaflac --add-padding=4321 $flacfile $flacfile
check_flac
metaflac_test case16 "--add-padding=4321 * 2" "--list"

run_metaflac --merge-padding $flacfile
check_flac
metaflac_test case17 "--merge-padding" "--list"

run_metaflac --add-padding=0 $flacfile
check_flac
metaflac_test case18 "--add-padding=0" "--list"

run_metaflac --sort-padding $flacfile
check_flac
metaflac_test case19 "--sort-padding" "--list"

run_metaflac --add-padding=0 $flacfile
check_flac
metaflac_test case20 "--add-padding=0" "--list"

run_metaflac --remove-all-tags $flacfile
check_flac
metaflac_test case21 "--remove-all-tags" "--list"

run_metaflac --remove --block-number=1,99 --dont-use-padding $flacfile
check_flac
metaflac_test case22 "--remove --block-number=1,99 --dont-use-padding" "--list"

run_metaflac --remove --block-number=99 --dont-use-padding $flacfile
check_flac
metaflac_test case23 "--remove --block-number=99 --dont-use-padding" "--list"

run_metaflac --remove --block-type=PADDING $flacfile
check_flac
metaflac_test case24 "--remove --block-type=PADDING" "--list"

run_metaflac --remove --block-type=PADDING --dont-use-padding $flacfile
check_flac
metaflac_test case25 "--remove --block-type=PADDING --dont-use-padding" "--list"

run_metaflac --add-padding=0 $flacfile $flacfile
check_flac
metaflac_test case26 "--add-padding=0 * 2" "--list"

run_metaflac --remove --except-block-type=PADDING $flacfile
check_flac
metaflac_test case27 "--remove --except-block-type=PADDING" "--list"

run_metaflac --remove-all $flacfile
check_flac
metaflac_test case28 "--remove-all" "--list"

run_metaflac --remove-all --dont-use-padding $flacfile
check_flac
metaflac_test case29 "--remove-all --dont-use-padding" "--list"

run_metaflac --remove-all --dont-use-padding $flacfile
check_flac
metaflac_test case30 "--remove-all --dont-use-padding" "--list"

run_metaflac --set-tag="f=0123456789abcdefghij" $flacfile
check_flac
metaflac_test case31 "--set-tag=..." "--list"

run_metaflac --remove-all-tags --set-tag="f=0123456789abcdefghi" $flacfile
check_flac
metaflac_test case32 "--remove-all-tags --set-tag=..." "--list"

run_metaflac --remove-all-tags --set-tag="f=0123456789abcde" $flacfile
check_flac
metaflac_test case33 "--remove-all-tags --set-tag=..." "--list"

run_metaflac --remove-all-tags --set-tag="f=0" $flacfile
check_flac
metaflac_test case34 "--remove-all-tags --set-tag=..." "--list"

run_metaflac --remove-all-tags --set-tag="f=0123456789" $flacfile
check_flac
metaflac_test case35 "--remove-all-tags --set-tag=..." "--list"

run_metaflac --remove-all-tags --set-tag="f=0123456789abcdefghi" $flacfile
check_flac
metaflac_test case36 "--remove-all-tags --set-tag=..." "--list"

run_metaflac --remove-all-tags --set-tag="f=0123456789" $flacfile
check_flac
metaflac_test case37 "--remove-all-tags --set-tag=..." "--list"

run_metaflac --remove-all-tags --set-tag="f=0123456789abcdefghij" $flacfile
check_flac
metaflac_test case38 "--remove-all-tags --set-tag=..." "--list"

echo "TITLE=Tittle" | run_metaflac --import-tags-from=- $flacfile
check_flac
metaflac_test case39 "--import-tags-from=-" "--list"

cat > vc.txt << EOF
artist=Fartist
artist=artits
EOF
run_metaflac --import-tags-from=vc.txt $flacfile
check_flac
metaflac_test case40 "--import-tags-from=[FILE]" "--list"

rm vc.txt

run_metaflac --add-replay-gain $flacfile
check_flac
metaflac_test case41 "--add-replay-gain" "--list"

run_metaflac --remove-replay-gain $flacfile
check_flac
metaflac_test case42 "--remove-replay-gain" "--list"

# CUESHEET blocks
cs_in=cuesheets/good.000.cue
cs_out=metaflac.cue
cs_out2=metaflac2.cue
run_metaflac --import-cuesheet-from="$cs_in" $flacfile
check_flac
metaflac_test case43 "--import-cuesheet-from" "--list"
run_metaflac --export-cuesheet-to=$cs_out $flacfile
run_metaflac --remove --block-type=CUESHEET $flacfile
check_flac
metaflac_test case44 "--remove --block-type=CUESHEET" "--list"
run_metaflac --import-cuesheet-from=$cs_out $flacfile
check_flac
metaflac_test case45 "--import-cuesheet-from" "--list"
run_metaflac --export-cuesheet-to=$cs_out2 $flacfile
echo "comparing cuesheets:"
diff $cs_out $cs_out2 || die "ERROR, cuesheets should be identical"
echo identical

rm -f $cs_out $cs_out2

# PICTURE blocks
ncase=46
for f in \
	0.gif \
	1.gif \
	2.gif \
; do
	run_metaflac --import-picture-from="|image/gif|$f||pictures/$f" $flacfile
	check_flac
	metaflac_test "case$ncase" "--import-picture-from" "--list"
	ncase=`expr $ncase + 1`
done
for f in \
	0.jpg \
	4.jpg \
; do
	run_metaflac --import-picture-from="4|image/jpeg|$f||pictures/$f" $flacfile
	check_flac
	metaflac_test "case$ncase" "--import-picture-from" "--list"
	ncase=`expr $ncase + 1`
done
for f in \
	0.png \
	1.png \
	2.png \
	3.png \
	4.png \
	5.png \
	6.png \
	7.png \
	8.png \
; do
	run_metaflac --import-picture-from="5|image/png|$f||pictures/$f" $flacfile
	check_flac
	metaflac_test "case$ncase" "--import-picture-from" "--list"
	ncase=`expr $ncase + 1`
done
[ $ncase = 60 ] || die "expected case# to be 60"

fn=export-picture-check
echo -n "Testing --export-picture-to... "
run_metaflac --export-picture-to=$fn $flacfile
check_flac
cmp $fn pictures/0.gif || die "ERROR, exported picture file and original differ"
echo OK
rm -f $fn
echo -n "Testing --block-number --export-picture-to... "
run_metaflac --block-number=9 --export-picture-to=$fn $flacfile
check_flac
cmp $fn pictures/0.png || die "ERROR, exported picture file and original differ"
echo OK
rm -f $fn

run_metaflac --remove --block-type=PICTURE $flacfile
check_flac
metaflac_test case60 "--remove --block-type=PICTURE" "--list"
run_metaflac --import-picture-from="1|image/png|standard_icon|32x32x24|pictures/0.png" $flacfile
check_flac
metaflac_test case61 "--import-picture-from" "--list"
run_metaflac --import-picture-from="2|image/png|icon|64x64x24|pictures/1.png" $flacfile
check_flac
metaflac_test case62 "--import-picture-from" "--list"

# UNKNOWN blocks
echo -n "Testing FLAC file with unknown metadata... "
cp -p metaflac.flac.in $flacfile
# remove the VORBIS_COMMENT block so vendor string changes don't interfere with the comparison:
run_metaflac --remove --block-type=VORBIS_COMMENT --dont-use-padding $flacfile
cmp $flacfile metaflac.flac.ok || die "ERROR, $flacfile and metaflac.flac.ok differ"
echo OK

# Replay gain tests - Test the rates which have specific filter table entries
# and verify that harmonics can be processed correctly.

tonegenerator ()
{
	# When using GAWK, use --lint=posix to identify non-POSIX awk usages.
    awk -- '
    BEGIN {
            samplerate = '$1';

            tone = 1000;
            duration = 1;
            bitspersample = 24;

            samplemidpoint = 1;
			for (sps = 0 ; sps < bitspersample - 1 ; sps++) {
				samplemidpoint *= 2;
			}

            samplerange = samplemidpoint - 1;

            pi = 4 * atan2(1,1);

            for (ix = 0; ix < duration * samplerate; ++ix) {
                    sample = sin(2 * pi * tone * ix / samplerate);
                    sample *= samplerange;
                    sample += samplemidpoint;
                    sample = int(sample);
                    for (bx = 0; bx < bitspersample/8; ++bx) {
                            byte[bx] = sample % 256;
                            sample /= 256;
                    }
                    while (bx--) {
                            printf("%c", byte[bx]);
                    }
            }

    }' /dev/null |
    flac --silent \
        --endian=big --channels=1 --bps=24 --sample-rate=$1 --sign=unsigned -
}

REPLAYGAIN_FREQ=
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ  8000/-12.73"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 11025/-12.93"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 11025/-12.93"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 12000/-12.98"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 16000/-13.27"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 18900/-13.41"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 22050/-13.77"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 24000/-13.82"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 28000/-14.06"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 32000/-14.08"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 36000/-14.12"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 37800/-14.18"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 44100/-14.17"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 48000/-14.16:1:2:4"

set -e

for ACTION in $REPLAYGAIN_FREQ ; do
  if [ -n "${ACTION##*:*}" ] ; then
    HARMONICS=1
  else
    HARMONICS="${ACTION#*:}"
  fi
  FREQ="${ACTION%%/*}"
  GAIN="${ACTION#*/}"
  GAIN="${GAIN%%:*}"
  while [ -n "$HARMONICS" ] ; do
    MULTIPLE="${HARMONICS%%:*}"
    if [ x"$MULTIPLE" = x"$HARMONICS" ] ; then
      HARMONICS=
    else
      HARMONICS="${HARMONICS#*:}"
    fi
    RATE=$(($MULTIPLE * FREQ))
    [ $MULTIPLE -eq 1 -o -n "${REPLAYGAIN_FREQ##* $RATE/*}" ] || break
    echo -n "Testing FLAC replaygain $RATE ($FREQ x $MULTIPLE) ... "
    tonegenerator $RATE > $flacfile
    run_metaflac --add-replay-gain $flacfile
    run_metaflac --list $flacfile | grep REPLAYGAIN.*GAIN= |
    while read -r REPLAYGAIN ; do
      MEASUREDGAIN="${REPLAYGAIN##*=}"
      MEASUREDGAIN="${MEASUREDGAIN%% *}"
      if [ x"$MEASUREDGAIN" != x"$GAIN" ] ; then
        die "ERROR, Expected $GAIN db instead of $REPLAYGAIN"
      fi
    done
    echo OK
  done
done


rm -f $testdir/out.flac $testdir/out.meta

exit 0
