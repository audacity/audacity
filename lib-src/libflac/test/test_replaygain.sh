#!/bin/sh -e

#  FLAC - Free Lossless Audio Codec
#  Copyright (C) 2002-2009  Josh Coalson
#  Copyright (C) 2011-2014  Xiph.Org Foundation
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

. ./common.sh

PATH=`pwd`/../src/flac:$PATH
PATH=`pwd`/../src/metaflac:$PATH
PATH=`pwd`/../objs/$BUILD/bin:$PATH

if echo a | (grep -E '(a|b)') >/dev/null 2>&1
	then EGREP='grep -E'
	else EGREP='egrep'
fi

testdir="metaflac-test-files"
flacfile="replaygain.flac"

run_flac ()
{
	if [ x"$FLAC__TEST_WITH_VALGRIND" = xyes ] ; then
		echo "valgrind --leak-check=yes --show-reachable=yes --num-callers=50 flac $*" >>test_replaygain.valgrind.log
		valgrind --leak-check=yes --show-reachable=yes --num-callers=50 --log-fd=4 flac --no-error-on-compression-fail $* 4>>test_replaygain.valgrind.log
	else
		flac${EXE} --no-error-on-compression-fail $*
	fi
}

run_metaflac ()
{
	if [ x"$FLAC__TEST_WITH_VALGRIND" = xyes ] ; then
		echo "valgrind --leak-check=yes --show-reachable=yes --num-callers=50 metaflac $*" >>test_replaygain.valgrind.log
		valgrind --leak-check=yes --show-reachable=yes --num-callers=50 --log-fd=4 metaflac $* 4>>test_replaygain.valgrind.log
	else
		metaflac${EXE} $*
	fi
}

run_metaflac_silent ()
{
	if [ -z "$SILENT" ] ; then
		run_metaflac $*
	else
		if [ x"$FLAC__TEST_WITH_VALGRIND" = xyes ] ; then
			echo "valgrind --leak-check=yes --show-reachable=yes --num-callers=50 metaflac $*" >>test_replaygain.valgrind.log
			valgrind --leak-check=yes --show-reachable=yes --num-callers=50 --log-fd=4 metaflac $* 2>/dev/null 4>>test_replaygain.valgrind.log
		else
			metaflac${EXE} $* 2>/dev/null
		fi
	fi
}

check_flac ()
{
	run_flac --silent --test $flacfile || die "ERROR in $flacfile" 1>&2
}

echo "Generating stream..."
bytes=80000
if dd if=/dev/zero ibs=1 count=$bytes | flac${EXE} --force --verify -0 --input-size=$bytes --output-name=$flacfile --force-raw-format --endian=big --sign=signed --channels=1 --bps=8 --sample-rate=8000 - ; then
	chmod +w $flacfile
else
	die "ERROR during generation"
fi

check_flac


if mawk ; then
	AWK=mawk
else
	# Really hope awk is not gawk, because the following AWK script doesn't
	# work correctly with gawk 4.0.1 but did with earlier versions.
	AWK=awk
	fi

# Replay gain tests - Test the rates which have specific filter table entries
# and verify that harmonics can be processed correctly.

tonegenerator ()
{
	# When using GAWK, use --lint=posix to identify non-POSIX awk usages.
    $AWK -- '
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
    flac${EXE} --silent --no-error-on-compression-fail --force-raw-format \
        --endian=big --channels=1 --bps=24 --sample-rate=$1 --sign=unsigned -
}

REPLAYGAIN_FREQ=
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ  8000/-12.76"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 11025/-12.93"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 12000/-13.00"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 16000/-13.29"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 18900/-13.43"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 22050/-13.77"
REPLAYGAIN_FREQ="$REPLAYGAIN_FREQ 24000/-13.83"
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
