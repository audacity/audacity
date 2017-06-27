#! /bin/sh

#--------------------------------------------------------------------------
# autoconf 2.58 or newer
#
ac_version="`${AUTOCONF:-autoconf} --version 2> /dev/null | head -1 | sed -e 's/^[^0-9]*//' -e 's/[a-z]* *$//'`"
if test -z "$ac_version"; then
  echo "ERROR: autoconf not found."
  echo "       You need autoconf version 2.58 or newer installed."
  exit 1
fi
IFS=.; set $ac_version; IFS=' '
if test "$1" = "2" -a "$2" -lt "58" || test "$1" -lt "2"; then
  echo "ERROR: autoconf version $ac_version found."
  echo "       You need autoconf version 2.58 or newer installed."
  exit 1
fi

echo "Creating configure ..."
${AUTORECONF:-autoreconf} -fvi

echo "Creating conftools/install-sh ..."
# .. for configure, despite not using automake
automake --add-missing 2>/dev/null || true

# toss this; it gets created by autoconf on some systems
rm -rf autom4te*.cache

# exit with the right value, so any calling script can continue
exit 0
