# Spec file is based on https://src.fedoraproject.org/fork/imcinerney/rpms/audacity/tree/im/depupdate
%global _privatelibs lib-.*[.]so.*
%global __provides_exclude ^(%{_privatelibs})$
%global __requires_exclude ^(%{_privatelibs})$

Name: audacity

Version: TMPL_AUDACITY_VERSION
Release: 0%{?dist}
Summary: Multitrack audio editor
License: GPLv2
URL:     https://www.audacityteam.org/

Source0: TMPL_AUDACITY_SOURCES

%define tartopdir TMPL_AUDACITY_TAR_DIRNAME

BuildRequires: cmake
BuildRequires: gettext-devel

BuildRequires: gcc
BuildRequires: gcc-c++
BuildRequires: git
BuildRequires: python3

BuildRequires: libjpeg-turbo-devel
BuildRequires: alsa-lib-devel
BuildRequires: desktop-file-utils
BuildRequires: expat-devel
BuildRequires: flac-devel
BuildRequires: jack-audio-connection-kit-devel
BuildRequires: ladspa-devel
BuildRequires: lame-devel
BuildRequires: libid3tag-devel
BuildRequires: taglib-devel
BuildRequires: twolame-devel
BuildRequires: libogg-devel
BuildRequires: libsndfile-devel
BuildRequires: libvorbis-devel
BuildRequires: portaudio-devel >= 19-16
BuildRequires: portmidi-devel
BuildRequires: soundtouch-devel
BuildRequires: soxr-devel
BuildRequires: vamp-plugin-sdk-devel >= 2.0
BuildRequires: zlib-devel
BuildRequires: libuuid-devel
BuildRequires: wxGTK-devel
BuildRequires: gtk3-devel
BuildRequires: glib2-devel
BuildRequires: libappstream-glib
BuildRequires: sqlite-devel >= 3.32
BuildRequires: lv2-devel >= 1.16
BuildRequires: lilv-devel >= 0.24.6
BuildRequires: serd-devel >= 0.30.2
BuildRequires: sord-devel >= 0.16.4
BuildRequires: sratom-devel >= 0.6.4
BuildRequires: suil-devel  >= 0.10.6
BuildRequires: flac-devel
BuildRequires: harfbuzz-devel
BuildRequires: freetype-devel
BuildRequires: fontconfig-devel
BuildRequires: mesa-libEGL-devel
BuildRequires: mpg123-devel
BuildRequires: wavpack-devel

Requires:      portaudio%{?_isa} >= 19-16

ExcludeArch: s390x

%description
Audacity is a cross-platform multitrack audio editor. It allows you to
record sounds directly or to import files in various formats. It features
a few simple effects, all of the editing features you should need, and
unlimited undo. The GUI was built with wxWidgets and the audio I/O
supports PulseAudio, OSS and ALSA under Linux.


%prep
%setup -q -n %{tartopdir}

%build

%cmake \
    -D CMAKE_BUILD_TYPE=Release \
    -D audacity_conan_enabled=Off \
    -D audacity_use_pch=no \
    -D audacity_use_portsmf=local \
    -D audacity_use_sbsms=local \
    -D audacity_has_vst3=Off \

%cmake_build


%install

%cmake_install

rm -Rf $RPM_BUILD_ROOT%{_datadir}/%{name}/include

# Remove a helper script, that runs audacity in GitHub CI builds
rm -f $RPM_BUILD_ROOT/usr/%{name}

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
%{_datadir}/%{name}/EffectsMenuDefaults.xml
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
