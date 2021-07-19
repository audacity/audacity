# Spec file is based on https://src.fedoraproject.org/fork/imcinerney/rpms/audacity/tree/im/depupdate
# Compile options:
# invoke with: rpmbuild --with ffmpeg --with local_ffmpeg audacity.spec to use local ffmpeg
%bcond_with     ffmpeg
%bcond_with     local_ffmpeg

#global commit0 53a5c930a4b5b053ab06a8b975458fc51cf41f6c
#global shortcommit0 #(c=#{commit0}; echo ${c:0:7})

Name: audacity

Version: 3.0.3
Release: 0%{?dist}
Summary: Multitrack audio editor
License: GPLv2
URL:     https://www.audacityteam.org/

#Source0: http://www.fosshub.com/Audacity.html/%{name}-minsrc-%{version}.tar.xz
# For alpha git snapshots for testing use the github archive as upstream source:
#Source0: https://github.com/audacity/#{name}/archive/#{commit0}/#{name}-#{commit0}.tar.gz
# ie wget https://github.com/audacity/audacity/archive/ecdb1d81c9312789c6233aba2190572344b22188/audacity-ecdb1d81c9312789c6233aba2190572344b22188.tar.gz
# Fake URL for Docker environment
Source0: %{name}.tar.gz

#define tartopdir audacity-minsrc-%{version}
#define tartopdir audacity-#{commit0}
%define tartopdir audacity

Source1: http://www.fosshub.com/Audacity.html/%{name}-manual-3.0.2.zip

BuildRequires: cmake
BuildRequires: gettext-devel

%if 0%{?rhel} == 7
BuildRequires: devtoolset-7-toolchain, devtoolset-7-libatomic-devel
%endif
BuildRequires: gcc
BuildRequires: gcc-c++

BuildRequires: libjpeg-turbo-devel
BuildRequires: alsa-lib-devel
BuildRequires: desktop-file-utils
BuildRequires: expat-devel
BuildRequires: flac-devel
BuildRequires: git
BuildRequires: jack-audio-connection-kit-devel
BuildRequires: ladspa-devel
BuildRequires: lame-devel
BuildRequires: libid3tag-devel
BuildRequires: libmad-devel
BuildRequires: taglib-devel
%if 0%{?rhel} && 0%{?rhel} == 8
#note: epel-8 currently doesn't have twolame-devel.
%else
BuildRequires: twolame-devel
%endif
BuildRequires: libogg-devel
BuildRequires: libsndfile-devel
BuildRequires: libvorbis-devel
BuildRequires: portaudio-devel >= 19-16
BuildRequires: portmidi-devel
BuildRequires: soundtouch-devel
BuildRequires: soxr-devel
BuildRequires: vamp-plugin-sdk-devel >= 2.0
BuildRequires: zip
BuildRequires: zlib-devel
BuildRequires: python3
BuildRequires: libuuid-devel
# We need /usr/bin/wx-config so that configure can detect the wx-config version:
#if 0#{?rhel} || 0#{?fedora} < 28
#BuildRequires: wxGTK3-devel
#endif
# But we will actually use the --toolkit=gtk2 version using --with-wx-version
#BuildRequires: compat-wxGTK3-gtk2-devel
BuildRequires: wxGTK-devel
BuildRequires: gtk3-devel
BuildRequires: glib2-devel
%if 0%{?rhel} >= 8 || 0%{?fedora}
BuildRequires: libappstream-glib
%endif

%if %{with ffmpeg}
%if ! %{with local_ffmpeg}
BuildRequires: ffmpeg-devel
%endif
%endif

# Note, we also need sqlite compiled with the SQLITE_DBPAGE table enabled, which
# the system version seems to be. (https://github.com/flathub/org.audacityteam.Audacity/issues/62)
BuildRequires: sqlite-devel >= 3.32

# LV2 interface and the plugins used
BuildRequires: lv2-devel >= 1.16
BuildRequires: lilv-devel >= 0.24.6
BuildRequires: serd-devel >= 0.30.2
BuildRequires: sord-devel >= 0.16.4
BuildRequires: sratom-devel >= 0.6.4
BuildRequires: suil-devel  >= 0.10.6

# For new symbols in portaudio
Requires:      portaudio%{?_isa} >= 19-16

ExcludeArch: s390x

%description
Audacity is a cross-platform multitrack audio editor. It allows you to
record sounds directly or to import files in various formats. It features
a few simple effects, all of the editing features you should need, and
unlimited undo. The GUI was built with wxWidgets and the audio I/O
supports PulseAudio, OSS and ALSA under Linux.

%package manual
Summary: Manual for Audacity - Offline Install
BuildArch: noarch
# -manual suits either audacity or audacity-freeworld; both create the path:
Requires: /usr/bin/audacity

%description manual
Audacity Manual can be installed locally if preferred, or accessed on-line
if internet connection is available.
For the most up to date manual content, use the on-line manual.


%prep
%setup -q -n %{tartopdir}

%build
%if 0%{?rhel} == 7
export WX_CONFIG=wx-config-3.0
%endif

%if 0%{?rhel} == 7
. /opt/rh/devtoolset-7/enable
%endif

%if (0%{?fedora} && 0%{?fedora} < 33)
mkdir build
cd build
%cmake \
    .. \
%else
%cmake \
%endif
    -DCMAKE_BUILD_TYPE=Release \
    -Daudacity_conan_enabled=Off \
    -Daudacity_lib_preference=system \
    -Daudacity_use_sqlite=system \
    -Daudacity_use_sndfile=system \
    -Daudacity_use_soxr=system \
    -Daudacity_use_lame=system \
%if 0%{?rhel} == 8
    -Daudacity_use_twolame=off \
%else
    -Daudacity_use_twolame=system \
%endif
    -Daudacity_use_flac=system \
    -Daudacity_use_ladspa=on \
    -Daudacity_use_vorbis=system \
    -Daudacity_use_id3tag=system \
    -Daudacity_use_expat=system \
    -Daudacity_use_soundtouch=system \
    -Daudacity_use_vamp=system \
    -Daudacity_use_lv2=system \
    -Daudacity_use_portaudio=local \
    -Daudacity_use_midi=system \
    -Daudacity_use_ogg=system \
%if %{with ffmpeg}
%if ! %{with local_ffmpeg}
    -Daudacity_use_ffmpeg=loaded \
%endif
%else
    -Daudacity_use_ffmpeg=off \
%endif

%cmake_build


%install
%if (0%{?fedora} && 0%{?fedora} < 33)
cd build
%endif

%cmake_install

%if (0%{?fedora} && 0%{?fedora} < 33)
cd -
%endif

rm -Rf $RPM_BUILD_ROOT%{_datadir}/%{name}/include

# Remove a helper script, that runs audacity in GitHub CI builds
rm -f $RPM_BUILD_ROOT/usr/%{name}

%if 0%{?rhel} >= 8 || 0%{?fedora}
if appstream-util --help | grep -q replace-screenshots ; then
# Update the screenshot shown in the software center
#
# NOTE: It would be *awesome* if this file was pushed upstream.
#
# See http://people.freedesktop.org/~hughsient/appdata/#screenshots for more details.
#
appstream-util replace-screenshots $RPM_BUILD_ROOT%{_datadir}/metainfo/audacity.appdata.xml \
  https://raw.githubusercontent.com/hughsie/fedora-appstream/master/screenshots-extra/audacity/a.png
fi
%endif

mkdir -p $RPM_BUILD_ROOT%{_datadir}/%{name}/help/manual
# audacity manual must be unzipped to correct location
unzip %{SOURCE1} -d $RPM_BUILD_ROOT%{_datadir}/%{name}/help


%{find_lang} %{name}

desktop-file-install --dir $RPM_BUILD_ROOT%{_datadir}/applications \
        $RPM_BUILD_ROOT%{_datadir}/applications/audacity.desktop

mkdir %{buildroot}%{_datadir}/doc/%{name}/nyquist
cp -pr lib-src/libnyquist/nyquist/license.txt %{buildroot}%{_datadir}/doc/%{name}/nyquist
cp -pr lib-src/libnyquist/nyquist/Readme.txt %{buildroot}%{_datadir}/doc/%{name}/nyquist
rm %{buildroot}%{_datadir}/doc/%{name}/LICENSE.txt

%files -f %{name}.lang
%{_bindir}/%{name}
%{_libdir}/%{name}
%dir %{_datadir}/%{name}
%{_datadir}/%{name}/EQDefaultCurves.xml
# Modules go to lib/audacity
#{_datadir}/%{name}/modules/
%{_datadir}/%{name}/nyquist/
%{_datadir}/%{name}/plug-ins/
%exclude %{_datadir}/%{name}/help
%{_mandir}/man*/*
%{_datadir}/applications/*
%{_datadir}/metainfo/%{name}.appdata.xml
%{_datadir}/pixmaps/*
%{_datadir}/icons/hicolor/*/%{name}.png
%{_datadir}/icons/hicolor/scalable/apps/%{name}.svg
%{_datadir}/mime/packages/*
%{_datadir}/doc/%{name}
%license LICENSE.txt

%files manual
%dir %{_datadir}/%{name}
%dir %{_datadir}/%{name}/help
%{_datadir}/%{name}/help/manual/
