
%define name    libsbsms
%define version 2.0.2
%define release 5
%define prefix  /usr/local

Summary:	C++ library for subband sinusoidal modeling time stretching and pitch scaling
Name:		%{name}
Version:	%{version}
Release:	%{release}
Prefix:		%{prefix}
Copyright:	GPL
Group:		Sound
Source0:	%{name}-%{version}.tar.bz2
URL:		
BuildRoot:	/var/tmp/%{name}-%{version}-buildroot

%description
C++ library for subband sinusoidal modeling time stretching and pitch scaling

%package devel
Summary:	C++ library for subband sinusoidal modeling time stretching and pitch scaling
Group:		Development/Libraries
Requires:	%{name} = %{version}

%description devel
C++ library for subband sinusoidal modeling time stretching and pitch scaling

%prep

%setup
if [ -f Makefile.cvs ]; then make -f Makefile.cvs; fi

%build
./configure --prefix=%{prefix}
make
make docs

%install
if [ -d $RPM_BUILD_ROOT ]; then rm -rf $RPM_BUILD_ROOT; fi
mkdir -p $RPM_BUILD_ROOT
make prefix=$RPM_BUILD_ROOT%{prefix} install

%clean
if [ -d $RPM_BUILD_ROOT ]; then rm -rf $RPM_BUILD_ROOT; fi

%files
%defattr(-,root,root)
%doc AUTHORS COPYING ChangeLog NEWS README TODO
%{prefix}/bin/*
%{prefix}/lib/libsbsms.so*
%{prefix}/man/man1/*

%files devel
%defattr(-,root,root)
%doc doc/html/*
%{prefix}/lib/libsbsms.a
%{prefix}/lib/libsbsms.la
%{prefix}/lib/pkgconfig/sbsms.pc
%{prefix}/include/*

%changelog
