/*

Copyright (c) Microsoft Corporation. All rights reserved.

Module Name:

    sdkddkver.h

Abstract:

    Master include file for versioning windows SDK/DDK.

*/

#ifndef _INC_SDKDDKVER
#define _INC_SDKDDKVER

#pragma once

//
// _WIN32_WINNT version constants
//
#define _WIN32_WINNT_NT4                    0x0400
#define _WIN32_WINNT_WIN2K                  0x0500
#define _WIN32_WINNT_WINXP                  0x0501
#define _WIN32_WINNT_WS03                   0x0502
#define _WIN32_WINNT_LONGHORN               0x0600

//
// _WIN32_IE_ version constants
//
#define _WIN32_IE_IE20                      0x0200
#define _WIN32_IE_IE30                      0x0300
#define _WIN32_IE_IE302                     0x0302
#define _WIN32_IE_IE40                      0x0400
#define _WIN32_IE_IE401                     0x0401
#define _WIN32_IE_IE50                      0x0500
#define _WIN32_IE_IE501                     0x0501
#define _WIN32_IE_IE55                      0x0550
#define _WIN32_IE_IE60                      0x0600
#define _WIN32_IE_IE60SP1                   0x0601
#define _WIN32_IE_IE60SP2                   0x0603
#define _WIN32_IE_IE70                      0x0700

//
// IE <-> OS version mapping
//
// NT4 supports IE versions 2.0 -> 6.0 SP1
#define _WIN32_IE_NT4                       _WIN32_IE_IE20
#define _WIN32_IE_NT4SP1                    _WIN32_IE_IE20
#define _WIN32_IE_NT4SP2                    _WIN32_IE_IE20
#define _WIN32_IE_NT4SP3                    _WIN32_IE_IE302
#define _WIN32_IE_NT4SP4                    _WIN32_IE_IE401
#define _WIN32_IE_NT4SP5                    _WIN32_IE_IE401
#define _WIN32_IE_NT4SP6                    _WIN32_IE_IE50
// Win98 supports IE versions 4.01 -> 6.0 SP1
#define _WIN32_IE_WIN98                     _WIN32_IE_IE401
// Win98SE supports IE versions 5.0 -> 6.0 SP1
#define _WIN32_IE_WIN98SE                   _WIN32_IE_IE50
// WinME supports IE versions 5.5 -> 6.0 SP1
#define _WIN32_IE_WINME                     _WIN32_IE_IE55
// Win2k supports IE versions 5.01 -> 6.0 SP1
#define _WIN32_IE_WIN2K                     _WIN32_IE_IE501
#define _WIN32_IE_WIN2KSP1                  _WIN32_IE_IE501
#define _WIN32_IE_WIN2KSP2                  _WIN32_IE_IE501
#define _WIN32_IE_WIN2KSP3                  _WIN32_IE_IE501
#define _WIN32_IE_WIN2KSP4                  _WIN32_IE_IE501
#define _WIN32_IE_XP                        _WIN32_IE_IE60
#define _WIN32_IE_XPSP1                     _WIN32_IE_IE60SP1
#define _WIN32_IE_XPSP2                     _WIN32_IE_IE60SP2
#define _WIN32_IE_WS03                      0x0602
#define _WIN32_IE_WS03SP1                   _WIN32_IE_IE60SP2
#define _WIN32_IE_LONGHORN                  _WIN32_IE_IE70


//
// NTDDI version constants
//
#define NTDDI_WIN2K                         0x05000000
#define NTDDI_WIN2KSP1                      0x05000100
#define NTDDI_WIN2KSP2                      0x05000200
#define NTDDI_WIN2KSP3                      0x05000300
#define NTDDI_WIN2KSP4                      0x05000400

#define NTDDI_WINXP                         0x05010000
#define NTDDI_WINXPSP1                      0x05010100
#define NTDDI_WINXPSP2                      0x05010200

#define NTDDI_WS03                          0x05020000
#define NTDDI_WS03SP1                       0x05020100

#define NTDDI_LONGHORN                      0x06000000

//
// masks for version macros
//
#define OSVERSION_MASK      0xFFFF0000
#define SPVERSION_MASK      0x0000FF00
#define SUBVERSION_MASK     0x000000FF


//
// macros to extract various version fields from the NTDDI version
//
#define OSVER(Version)  ((Version) & OSVERSION_MASK)
#define SPVER(Version)  (((Version) & SPVERSION_MASK) >> 8)
#define SUBVER(Version) (((Version) & SUBVERSION_MASK) )


#if defined(DECLSPEC_DEPRECATED_DDK)

// deprecate in 2k or later
#if (NTDDI_VERSION >= NTDDI_WIN2K)
#define DECLSPEC_DEPRECATED_DDK_WIN2K DECLSPEC_DEPRECATED_DDK
#else
#define DECLSPEC_DEPRECATED_DDK_WIN2K
#endif

// deprecate in XP or later
#if (NTDDI_VERSION >= NTDDI_WINXP)
#define DECLSPEC_DEPRECATED_DDK_WINXP DECLSPEC_DEPRECATED_DDK
#else
#define DECLSPEC_DEPRECATED_DDK_WINXP
#endif

// deprecate in WS03 or later
#if (NTDDI_VERSION >= NTDDI_WS03)
#define DECLSPEC_DEPRECATED_DDK_WIN2003 DECLSPEC_DEPRECATED_DDK
#else
#define DECLSPEC_DEPRECATED_DDK_WIN2003
#endif

// deprecate in WS03 or later
#if (NTDDI_VERSION >= NTDDI_LONGHORN)
#define DECLSPEC_DEPRECATED_DDK_LONGHORN DECLSPEC_DEPRECATED_DDK
#else
#define DECLSPEC_DEPRECATED_DDK_LONGHORN
#endif

#endif // defined(DECLSPEC_DEPRECATED_DDK)


//
// if versions aren't already defined, default to most current
//

#define NTDDI_VERSION_FROM_WIN32_WINNT2(ver)    ver##0000
#define NTDDI_VERSION_FROM_WIN32_WINNT(ver)     NTDDI_VERSION_FROM_WIN32_WINNT2(ver)

#if !defined(_WIN32_WINNT) && !defined(_CHICAGO_)
#define  _WIN32_WINNT   0x0600
#endif

#ifndef NTDDI_VERSION
#ifdef _WIN32_WINNT
// set NTDDI_VERSION based on _WIN32_WINNT
#define NTDDI_VERSION   NTDDI_VERSION_FROM_WIN32_WINNT(_WIN32_WINNT)
#else
#define NTDDI_VERSION   0x06000000
#endif
#endif

#ifndef WINVER
#ifdef _WIN32_WINNT
// set WINVER based on _WIN32_WINNT
#define WINVER          _WIN32_WINNT
#else
#define WINVER          0x0600
#endif
#endif

#ifndef _WIN32_IE
#ifdef _WIN32_WINNT
// set _WIN32_IE based on _WIN32_WINNT
#if (_WIN32_WINNT <= _WIN32_WINNT_NT4)
#define _WIN32_IE       _WIN32_IE_IE50
#elif (_WIN32_WINNT <= _WIN32_WINNT_WIN2K)
#define _WIN32_IE       _WIN32_IE_IE501
#elif (_WIN32_WINNT <= _WIN32_WINNT_WINXP)
#define _WIN32_IE       _WIN32_IE_IE60
#elif (_WIN32_WINNT <= _WIN32_WINNT_WS03)
#define _WIN32_IE       0x0602
#else
#define _WIN32_IE       0x0700
#endif
#else
#define _WIN32_IE       0x0700
#endif
#endif

//
// Sanity check for compatible versions
//
#if defined(_WIN32_WINNT) && !defined(MIDL_PASS) && !defined(RC_INVOKED)

#if (defined(WINVER) && (WINVER < 0x0400) && (_WIN32_WINNT > 0x0400))
#error WINVER setting conflicts with _WIN32_WINNT setting
#endif

#if (((OSVERSION_MASK & NTDDI_VERSION) == NTDDI_WIN2K) && (_WIN32_WINNT != _WIN32_WINNT_WIN2K))
#error NTDDI_VERSION setting conflicts with _WIN32_WINNT setting
#endif

#if (((OSVERSION_MASK & NTDDI_VERSION) == NTDDI_WINXP) && (_WIN32_WINNT != _WIN32_WINNT_WINXP))
#error NTDDI_VERSION setting conflicts with _WIN32_WINNT setting
#endif

#if (((OSVERSION_MASK & NTDDI_VERSION) == NTDDI_WS03) && (_WIN32_WINNT != _WIN32_WINNT_WS03))
#error NTDDI_VERSION setting conflicts with _WIN32_WINNT setting
#endif

#if (((OSVERSION_MASK & NTDDI_VERSION) == NTDDI_LONGHORN) && (_WIN32_WINNT != _WIN32_WINNT_LONGHORN))
#error NTDDI_VERSION setting conflicts with _WIN32_WINNT setting
#endif

#if ((_WIN32_WINNT < _WIN32_WINNT_WIN2K) && (_WIN32_IE > _WIN32_IE_IE60SP1))
#error _WIN32_WINNT settings conflicts with _WIN32_IE setting
#endif

#endif  // defined(_WIN32_WINNT) && !defined(MIDL_PASS) && !defined(_WINRESRC_)


#endif  // !_INC_SDKDDKVER


