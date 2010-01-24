#!/bin/sh

libtoolize --force || {
	echo "libtoolize failed, exiting..."
	exit 1
}

aclocal $ACLOCAL_FLAGS || {
    echo "aclocal \$ACLOCAL_FLAGS where \$ACLOCAL_FLAGS= failed, exiting..."
    exit 1
}

autoheader || {
    echo "autoheader failed, exiting..."
    exit 1
}

automake --add-missing --foreign || {
    echo "automake --add-missing --foreign failed, exiting..."
    exit 1
}

autoconf || {
    echo "autoconf failed, exiting..."
    exit 1
}

echo "Running ./configure --enable-maintainer-mode $@..."

./configure --enable-maintainer-mode $@
