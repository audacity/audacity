

/* this ALWAYS GENERATED file contains the definitions for the interfaces */


 /* File created by MIDL compiler version 7.00.0499 */
/* Compiler settings for shtypes.idl:
    Oicf, W1, Zp8, env=Win32 (32b run)
    protocol : dce , ms_ext, c_ext, robust
    error checks: allocation ref bounds_check enum stub_data 
    VC __declspec() decoration level: 
         __declspec(uuid()), __declspec(selectany), __declspec(novtable)
         DECLSPEC_UUID(), MIDL_INTERFACE()
*/
//@@MIDL_FILE_HEADING(  )

#pragma warning( disable: 4049 )  /* more than 64k source lines */


/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 500
#endif

/* verify that the <rpcsal.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCSAL_H_VERSION__
#define __REQUIRED_RPCSAL_H_VERSION__ 100
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif // __RPCNDR_H_VERSION__


#ifndef __shtypes_h__
#define __shtypes_h__

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

/* Forward Declarations */ 

/* header files for imported files */
#include "wtypes.h"

#ifdef __cplusplus
extern "C"{
#endif 


/* interface __MIDL_itf_shtypes_0000_0000 */
/* [local] */ 

//+-------------------------------------------------------------------------
//
//  Microsoft Windows
//  Copyright (c) Microsoft Corporation. All rights reserved.
//
//--------------------------------------------------------------------------
//===========================================================================
//
// Object identifiers in the explorer's name space (ItemID and IDList)
//
//  All the items that the user can browse with the explorer (such as files,
// directories, servers, work-groups, etc.) has an identifier which is unique
// among items within the parent folder. Those identifiers are called item
// IDs (SHITEMID). Since all its parent folders have their own item IDs,
// any items can be uniquely identified by a list of item IDs, which is called
// an ID list (ITEMIDLIST).
//
//  ID lists are almost always allocated by the task allocator (see some
// description below as well as OLE 2.0 SDK) and may be passed across
// some of shell interfaces (such as IShellFolder). Each item ID in an ID list
// is only meaningful to its parent folder (which has generated it), and all
// the clients must treat it as an opaque binary data except the first two
// bytes, which indicates the size of the item ID.
//
//  When a shell extension -- which implements the IShellFolder interace --
// generates an item ID, it may put any information in it, not only the data
// with that it needs to identifies the item, but also some additional
// information, which would help implementing some other functions efficiently.
// For example, the shell's IShellFolder implementation of file system items
// stores the primary (long) name of a file or a directory as the item
// identifier, but it also stores its alternative (short) name, size and date
// etc.
//
//  When an ID list is passed to one of shell APIs (such as SHGetPathFromIDList),
// it is always an absolute path -- relative from the root of the name space,
// which is the desktop folder. When an ID list is passed to one of IShellFolder
// member function, it is always a relative path from the folder (unless it
// is explicitly specified).
//
//===========================================================================
//
// SHITEMID -- Item ID  (mkid)
//     USHORT      cb;             // Size of the ID (including cb itself)
//     BYTE        abID[];         // The item ID (variable length)
//
#include <pshpack1.h>
typedef struct _SHITEMID
    {
    USHORT cb;
    BYTE abID[ 1 ];
    } 	SHITEMID;

#include <poppack.h>
#if defined(_M_IX86)
#define __unaligned
#endif // __unaligned
typedef SHITEMID __unaligned *LPSHITEMID;

typedef const SHITEMID __unaligned *LPCSHITEMID;

//
// ITEMIDLIST -- List if item IDs (combined with 0-terminator)
//
#include <pshpack1.h>
typedef struct _ITEMIDLIST
    {
    SHITEMID mkid;
    } 	ITEMIDLIST;

#if defined(STRICT_TYPED_ITEMIDS) && defined(__cplusplus)
typedef struct _ITEMIDLIST_RELATIVE : ITEMIDLIST {} ITEMIDLIST_RELATIVE;
typedef struct _ITEMID_CHILD : ITEMIDLIST_RELATIVE {} ITEMID_CHILD;
typedef struct _ITEMIDLIST_ABSOLUTE : ITEMIDLIST_RELATIVE {} ITEMIDLIST_ABSOLUTE;
#else // !(defined(STRICT_TYPED_ITEMIDS) && defined(__cplusplus))
typedef ITEMIDLIST ITEMIDLIST_RELATIVE;

typedef ITEMIDLIST ITEMID_CHILD;

typedef ITEMIDLIST ITEMIDLIST_ABSOLUTE;

#endif // defined(STRICT_TYPED_ITEMIDS) && defined(__cplusplus)
#include <poppack.h>
typedef /* [unique] */  __RPC_unique_pointer BYTE_BLOB *wirePIDL;

typedef /* [wire_marshal] */ ITEMIDLIST __unaligned *LPITEMIDLIST;

typedef /* [wire_marshal] */ const ITEMIDLIST __unaligned *LPCITEMIDLIST;

#if defined(STRICT_TYPED_ITEMIDS) && defined(__cplusplus)
typedef /* [wire_marshal] */ ITEMIDLIST_ABSOLUTE *PIDLIST_ABSOLUTE;

typedef /* [wire_marshal] */ const ITEMIDLIST_ABSOLUTE *PCIDLIST_ABSOLUTE;

typedef /* [wire_marshal] */ const ITEMIDLIST_ABSOLUTE __unaligned *PCUIDLIST_ABSOLUTE;

typedef /* [wire_marshal] */ ITEMIDLIST_RELATIVE *PIDLIST_RELATIVE;

typedef /* [wire_marshal] */ const ITEMIDLIST_RELATIVE *PCIDLIST_RELATIVE;

typedef /* [wire_marshal] */ ITEMIDLIST_RELATIVE __unaligned *PUIDLIST_RELATIVE;

typedef /* [wire_marshal] */ const ITEMIDLIST_RELATIVE __unaligned *PCUIDLIST_RELATIVE;

typedef /* [wire_marshal] */ ITEMID_CHILD *PITEMID_CHILD;

typedef /* [wire_marshal] */ const ITEMID_CHILD *PCITEMID_CHILD;

typedef /* [wire_marshal] */ ITEMID_CHILD __unaligned *PUITEMID_CHILD;

typedef /* [wire_marshal] */ const ITEMID_CHILD __unaligned *PCUITEMID_CHILD;

typedef const PCUITEMID_CHILD *PCUITEMID_CHILD_ARRAY;

typedef const PCUIDLIST_RELATIVE *PCUIDLIST_RELATIVE_ARRAY;

typedef const PCIDLIST_ABSOLUTE *PCIDLIST_ABSOLUTE_ARRAY;

typedef const PCUIDLIST_ABSOLUTE *PCUIDLIST_ABSOLUTE_ARRAY;

#else // !(defined(STRICT_TYPED_ITEMIDS) && defined(__cplusplus))
#define PIDLIST_ABSOLUTE         LPITEMIDLIST
#define PCIDLIST_ABSOLUTE        LPCITEMIDLIST
#define PCUIDLIST_ABSOLUTE       LPCITEMIDLIST
#define PIDLIST_RELATIVE         LPITEMIDLIST
#define PCIDLIST_RELATIVE        LPCITEMIDLIST
#define PUIDLIST_RELATIVE        LPITEMIDLIST
#define PCUIDLIST_RELATIVE       LPCITEMIDLIST
#define PITEMID_CHILD            LPITEMIDLIST
#define PCITEMID_CHILD           LPCITEMIDLIST
#define PUITEMID_CHILD           LPITEMIDLIST
#define PCUITEMID_CHILD          LPCITEMIDLIST
#define PCUITEMID_CHILD_ARRAY    LPCITEMIDLIST *
#define PCUIDLIST_RELATIVE_ARRAY LPCITEMIDLIST *
#define PCIDLIST_ABSOLUTE_ARRAY  LPCITEMIDLIST *
#define PCUIDLIST_ABSOLUTE_ARRAY LPCITEMIDLIST *
#endif // defined(STRICT_TYPED_ITEMIDS) && defined(__cplusplus)
#ifdef MIDL_PASS
typedef struct _WIN32_FIND_DATAA
    {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD dwReserved0;
    DWORD dwReserved1;
    CHAR cFileName[ 260 ];
    CHAR cAlternateFileName[ 14 ];
    } 	WIN32_FIND_DATAA;

typedef struct _WIN32_FIND_DATAA *PWIN32_FIND_DATAA;

typedef struct _WIN32_FIND_DATAA *LPWIN32_FIND_DATAA;

typedef struct _WIN32_FIND_DATAW
    {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD dwReserved0;
    DWORD dwReserved1;
    WCHAR cFileName[ 260 ];
    WCHAR cAlternateFileName[ 14 ];
    } 	WIN32_FIND_DATAW;

typedef struct _WIN32_FIND_DATAW *PWIN32_FIND_DATAW;

typedef struct _WIN32_FIND_DATAW *LPWIN32_FIND_DATAW;

#endif // MIDL_PASS
//-------------------------------------------------------------------------
//
// struct STRRET
//
// structure for returning strings from IShellFolder member functions
//
//-------------------------------------------------------------------------
//
//  uType indicate which union member to use 
//    STRRET_WSTR    Use STRRET.pOleStr     must be freed by caller of GetDisplayNameOf
//    STRRET_OFFSET  Use STRRET.uOffset     Offset into SHITEMID for ANSI string 
//    STRRET_CSTR    Use STRRET.cStr        ANSI Buffer
//
typedef /* [v1_enum] */ 
enum tagSTRRET_TYPE
    {	STRRET_WSTR	= 0,
	STRRET_OFFSET	= 0x1,
	STRRET_CSTR	= 0x2
    } 	STRRET_TYPE;

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma warning(push)
#pragma warning(disable:4201) /* nonstandard extension used : nameless struct/union */
#pragma once
#endif
#include <pshpack8.h>
typedef struct _STRRET
    {
    UINT uType;
    union 
        {
        LPWSTR pOleStr;
        UINT uOffset;
        char cStr[ 260 ];
        } 	DUMMYUNIONNAME;
    } 	STRRET;

#include <poppack.h>
#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma warning(pop)
#endif
typedef STRRET *LPSTRRET;

//-------------------------------------------------------------------------
//
// struct SHELLDETAILS
//
// structure for returning strings from IShellDetails
//
//-------------------------------------------------------------------------
//
//  fmt;            // LVCFMT_* value (header only)
//  cxChar;         // Number of 'average' characters (header only)
//  str;            // String information
//
#include <pshpack1.h>
typedef struct _SHELLDETAILS
    {
    int fmt;
    int cxChar;
    STRRET str;
    } 	SHELLDETAILS;

typedef struct _SHELLDETAILS *LPSHELLDETAILS;

#include <poppack.h>

#if (_WIN32_IE >= _WIN32_IE_IE60SP2)
typedef /* [v1_enum] */ 
enum tagPERCEIVED
    {	PERCEIVED_TYPE_FIRST	= -3,
	PERCEIVED_TYPE_CUSTOM	= -3,
	PERCEIVED_TYPE_UNSPECIFIED	= -2,
	PERCEIVED_TYPE_FOLDER	= -1,
	PERCEIVED_TYPE_UNKNOWN	= 0,
	PERCEIVED_TYPE_TEXT	= 1,
	PERCEIVED_TYPE_IMAGE	= 2,
	PERCEIVED_TYPE_AUDIO	= 3,
	PERCEIVED_TYPE_VIDEO	= 4,
	PERCEIVED_TYPE_COMPRESSED	= 5,
	PERCEIVED_TYPE_DOCUMENT	= 6,
	PERCEIVED_TYPE_SYSTEM	= 7,
	PERCEIVED_TYPE_APPLICATION	= 8,
	PERCEIVED_TYPE_GAMEMEDIA	= 9,
	PERCEIVED_TYPE_CONTACTS	= 10,
	PERCEIVED_TYPE_LAST	= 10
    } 	PERCEIVED;

#define PERCEIVEDFLAG_UNDEFINED     0x0000
#define PERCEIVEDFLAG_SOFTCODED     0x0001
#define PERCEIVEDFLAG_HARDCODED     0x0002
#define PERCEIVEDFLAG_NATIVESUPPORT 0x0004
#define PERCEIVEDFLAG_GDIPLUS       0x0010
#define PERCEIVEDFLAG_WMSDK         0x0020
#define PERCEIVEDFLAG_ZIPFOLDER     0x0040
typedef DWORD PERCEIVEDFLAG;

#endif  // _WIN32_IE_IE60SP2

#if (NTDDI_VERSION >= NTDDI_LONGHORN)
typedef struct _COMDLG_FILTERSPEC
    {
    LPCWSTR pszName;
    LPCWSTR pszSpec;
    } 	COMDLG_FILTERSPEC;

typedef struct tagMACHINE_ID
    {
    char szName[ 16 ];
    } 	MACHINE_ID;

typedef struct tagDOMAIN_RELATIVE_OBJECTID
    {
    GUID guidVolume;
    GUID guidObject;
    } 	DOMAIN_RELATIVE_OBJECTID;

typedef GUID KNOWNFOLDERID;

#if 0
typedef KNOWNFOLDERID *REFKNOWNFOLDERID;

#endif // 0
#ifdef __cplusplus
#define REFKNOWNFOLDERID const KNOWNFOLDERID &
#else // !__cplusplus
#define REFKNOWNFOLDERID const KNOWNFOLDERID * __MIDL_CONST
#endif // __cplusplus
#endif  // NTDDI_LONGHORN
typedef GUID FOLDERTYPEID;

#if 0
typedef FOLDERTYPEID *REFFOLDERTYPEID;

#endif // 0
#ifdef __cplusplus
#define REFFOLDERTYPEID const FOLDERTYPEID &
#else // !__cplusplus
#define REFFOLDERTYPEID const FOLDERTYPEID * __MIDL_CONST
#endif // __cplusplus
typedef GUID TASKOWNERID;

#if 0
typedef TASKOWNERID *REFTASKOWNERID;

#endif // 0
#ifdef __cplusplus
#define REFTASKOWNERID const TASKOWNERID &
#else // !__cplusplus
#define REFTASKOWNERID const TASKOWNERID * __MIDL_CONST
#endif // __cplusplus
#ifndef LF_FACESIZE
typedef struct tagLOGFONTA
    {
    LONG lfHeight;
    LONG lfWidth;
    LONG lfEscapement;
    LONG lfOrientation;
    LONG lfWeight;
    BYTE lfItalic;
    BYTE lfUnderline;
    BYTE lfStrikeOut;
    BYTE lfCharSet;
    BYTE lfOutPrecision;
    BYTE lfClipPrecision;
    BYTE lfQuality;
    BYTE lfPitchAndFamily;
    CHAR lfFaceName[ 32 ];
    } 	LOGFONTA;

typedef struct tagLOGFONTW
    {
    LONG lfHeight;
    LONG lfWidth;
    LONG lfEscapement;
    LONG lfOrientation;
    LONG lfWeight;
    BYTE lfItalic;
    BYTE lfUnderline;
    BYTE lfStrikeOut;
    BYTE lfCharSet;
    BYTE lfOutPrecision;
    BYTE lfClipPrecision;
    BYTE lfQuality;
    BYTE lfPitchAndFamily;
    WCHAR lfFaceName[ 32 ];
    } 	LOGFONTW;

typedef LOGFONTA LOGFONT;

#endif // LF_FACESIZE
typedef /* [v1_enum] */ 
enum tagSHCOLSTATE
    {	SHCOLSTATE_TYPE_STR	= 0x1,
	SHCOLSTATE_TYPE_INT	= 0x2,
	SHCOLSTATE_TYPE_DATE	= 0x3,
	SHCOLSTATE_TYPEMASK	= 0xf,
	SHCOLSTATE_ONBYDEFAULT	= 0x10,
	SHCOLSTATE_SLOW	= 0x20,
	SHCOLSTATE_EXTENDED	= 0x40,
	SHCOLSTATE_SECONDARYUI	= 0x80,
	SHCOLSTATE_HIDDEN	= 0x100,
	SHCOLSTATE_PREFER_VARCMP	= 0x200,
	SHCOLSTATE_PREFER_FMTCMP	= 0x400,
	SHCOLSTATE_NOSORTBYFOLDERNESS	= 0x800,
	SHCOLSTATE_VIEWONLY	= 0x10000,
	SHCOLSTATE_BATCHREAD	= 0x20000,
	SHCOLSTATE_NO_GROUPBY	= 0x40000,
	SHCOLSTATE_FIXED_WIDTH	= 0x1000,
	SHCOLSTATE_NODPISCALE	= 0x2000,
	SHCOLSTATE_FIXED_RATIO	= 0x4000,
	SHCOLSTATE_DISPLAYMASK	= 0xf000
    } 	SHCOLSTATE;

typedef DWORD SHCOLSTATEF;

typedef PROPERTYKEY SHCOLUMNID;

typedef const SHCOLUMNID *LPCSHCOLUMNID;



extern RPC_IF_HANDLE __MIDL_itf_shtypes_0000_0000_v0_0_c_ifspec;
extern RPC_IF_HANDLE __MIDL_itf_shtypes_0000_0000_v0_0_s_ifspec;

/* Additional Prototypes for ALL interfaces */

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif



