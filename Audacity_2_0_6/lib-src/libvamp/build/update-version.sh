#!/bin/bash

usage() {
    echo "Usage: $0 <version>"
    echo "  e.g. $0 2.2"
    echo "  e.g. $0 2.2.1"
    exit 2;
}

version=$1
[ -n "$version" ] || usage

major=${version%%.*} # 2.3 -> 2, 2.3.1 -> 2
minor=${version#*.}  # 2.3 -> 3, 2.3.1 -> 3.1
minor=${minor%.*}    # 3 -> 3, 3.1 -> 3

sdkmajor=$major
sdkminor=$minor

hostmajor=$(($major+1)) # there has been one API change in a minor release
hostminor=$minor

acs="`echo $version | tr '.' '_'`"

echo "Major version = $major, minor version = $minor"
echo "SDK current = $sdkmajor, age = $sdkminor"
echo "Host SDK current = $hostmajor, age = $hostminor"
echo "Version string = $version"
echo "acsymbols string = $acs"

p="perl -i -p -e"

$p 's/(This is version) [^ ]+/$1 '$version'/' \
    README

$p 's/(AC_INIT\(vamp-plugin-sdk,) [^,]+,/$1 '$version',/' \
    configure.ac

$p 's/(INSTALL_SDK_LIBNAME\s*=\s*libvamp-sdk.so).*/$1.'$sdkmajor'.'$sdkminor'.0/' \
    Makefile.in

$p 's/(INSTALL_SDK_LINK_ABI\s*=\s*libvamp-sdk.so).*/$1.'$sdkmajor'/' \
    Makefile.in

$p 's/(current)=.*/$1='$sdkmajor'/' \
    build/libvamp-sdk.la.in

$p 's/(age)=.*/$1='$sdkminor'/' \
    build/libvamp-sdk.la.in

$p 's/(INSTALL_HOSTSDK_LIBNAME\s*=\s*libvamp-hostsdk.so).*/$1.'$hostmajor'.'$hostminor'.0/' \
    Makefile.in

$p 's/(INSTALL_HOSTSDK_LINK_ABI\s*=\s*libvamp-hostsdk.so).*/$1.'$hostmajor'/' \
    Makefile.in

$p 's/(current)=.*/$1='$hostmajor'/' \
    build/libvamp-hostsdk.la.in

$p 's/(age)=.*/$1='$hostminor'/' \
    build/libvamp-hostsdk.la.in

$p 's/(PROJECT_NUMBER[^=]*)=.*/$1= '$version'/' \
    build/Doxyfile

$p 's/(VAMP_API_VERSION).*/$1 '$major'/' \
    vamp/vamp.h

$p 's/(VAMP_SDK_VERSION) +"[^"]*"/$1 "'$version'"/' \
    vamp-sdk/plugguard.h

$p 's/(VAMP_SDK_MAJOR_VERSION).*/$1 '$major'/' \
    vamp-sdk/plugguard.h

$p 's/(VAMP_SDK_MINOR_VERSION).*/$1 '$minor'/' \
    vamp-sdk/plugguard.h

$p 's/(VAMP_SDK_VERSION) +"[^"]*"/$1 "'$version'"/' \
    vamp-hostsdk/hostguard.h

$p 's/(VAMP_SDK_MAJOR_VERSION).*/$1 '$major'/' \
    vamp-hostsdk/hostguard.h

$p 's/(VAMP_SDK_MINOR_VERSION).*/$1 '$minor'/' \
    vamp-hostsdk/hostguard.h

$p 's/(VAMP_SDK_MAJOR_VERSION !=) [\d\.]+/$1 '$major'/' \
    src/vamp-sdk/FFT.cpp

$p 's/(VAMP_SDK_MINOR_VERSION !=) [\d\.]+/$1 '$minor'/' \
    src/vamp-sdk/FFT.cpp

$p 's/(VAMP_SDK_MAJOR_VERSION !=) [\d\.]+/$1 '$major'/' \
    src/vamp-sdk/PluginAdapter.cpp

$p 's/(VAMP_SDK_MINOR_VERSION !=) [\d\.]+/$1 '$minor'/' \
    src/vamp-sdk/PluginAdapter.cpp

$p 's/(VAMP_SDK_MAJOR_VERSION !=) [\d\.]+/$1 '$major'/' \
    src/vamp-hostsdk/PluginHostAdapter.cpp

$p 's/(VAMP_SDK_MINOR_VERSION !=) [\d\.]+/$1 '$minor'/' \
    src/vamp-hostsdk/PluginHostAdapter.cpp

for pc in pkgconfig/*.pc.in ; do
    $p 's/(Version:) .*/$1 '$version'/' $pc
done

fgrep -q 'libvampsdk_v_'$acs'_present' src/vamp-sdk/acsymbols.c || \
    $p 's/^$/\nextern void libvampsdk_v_'$acs'_present(void) { }/' \
    src/vamp-sdk/acsymbols.c

fgrep -q 'libvamphostsdk_v_'$acs'_present' src/vamp-hostsdk/acsymbols.c || \
$p 's/^$/\nextern void libvamphostsdk_v_'$acs'_present(void) { }/' \
    src/vamp-hostsdk/acsymbols.c

echo "Done, now check with e.g. hg diff -- and don't forget to update CHANGELOG"
