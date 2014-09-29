/***************************************************************************
    copyright            : (C) 2013 by Tsuda Kageyu
    email                : tsuda.kageyu@gmail.com
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License version   *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *
 *   02110-1301  USA                                                       *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_TUTILS_H
#define TAGLIB_TUTILS_H

// THIS FILE IS NOT A PART OF THE TAGLIB API

#ifndef DO_NOT_DOCUMENT  // tell Doxygen not to document this header

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined(HAVE_MSC_BYTESWAP)
# include <stdlib.h>
#elif defined(HAVE_GLIBC_BYTESWAP)
# include <byteswap.h>
#elif defined(HAVE_MAC_BYTESWAP)
# include <libkern/OSByteOrder.h>
#elif defined(HAVE_OPENBSD_BYTESWAP)
# include <sys/endian.h>
#endif

namespace TagLib
{
  namespace Utils
  {
    inline ushort byteSwap(ushort x)
    {
#if defined(HAVE_GCC_BYTESWAP_16)

      return __builtin_bswap16(x);

#elif defined(HAVE_MSC_BYTESWAP)

      return _byteswap_ushort(x);

#elif defined(HAVE_GLIBC_BYTESWAP)

      return __bswap_16(x);

#elif defined(HAVE_MAC_BYTESWAP)

      return OSSwapInt16(x);

#elif defined(HAVE_OPENBSD_BYTESWAP)

      return swap16(x);

#else

      return ((x >> 8) & 0xff) | ((x & 0xff) << 8);

#endif
    }

    inline uint byteSwap(uint x)
    {
#if defined(HAVE_GCC_BYTESWAP_32)

      return __builtin_bswap32(x);

#elif defined(HAVE_MSC_BYTESWAP)

      return _byteswap_ulong(x);

#elif defined(HAVE_GLIBC_BYTESWAP)

      return __bswap_32(x);

#elif defined(HAVE_MAC_BYTESWAP)

      return OSSwapInt32(x);

#elif defined(HAVE_OPENBSD_BYTESWAP)

      return swap32(x);

#else

      return ((x & 0xff000000u) >> 24)
        | ((x & 0x00ff0000u) >>  8)
        | ((x & 0x0000ff00u) <<  8)
        | ((x & 0x000000ffu) << 24);

#endif
    }

    inline ulonglong byteSwap(ulonglong x)
    {
#if defined(HAVE_GCC_BYTESWAP_64)

      return __builtin_bswap64(x);

#elif defined(HAVE_MSC_BYTESWAP)

      return _byteswap_uint64(x);

#elif defined(HAVE_GLIBC_BYTESWAP)

      return __bswap_64(x);

#elif defined(HAVE_MAC_BYTESWAP)

      return OSSwapInt64(x);

#elif defined(HAVE_OPENBSD_BYTESWAP)

      return swap64(x);

#else

      return ((x & 0xff00000000000000ull) >> 56)
        | ((x & 0x00ff000000000000ull) >> 40)
        | ((x & 0x0000ff0000000000ull) >> 24)
        | ((x & 0x000000ff00000000ull) >> 8)
        | ((x & 0x00000000ff000000ull) << 8)
        | ((x & 0x0000000000ff0000ull) << 24)
        | ((x & 0x000000000000ff00ull) << 40)
        | ((x & 0x00000000000000ffull) << 56);

#endif
    }

    enum ByteOrder
    {
      LittleEndian,
      BigEndian
    };

#ifdef SYSTEM_BYTEORDER

# if SYSTEM_BYTEORDER == 1

    const ByteOrder SystemByteOrder = LittleEndian; 

# else

    const ByteOrder SystemByteOrder = BigEndian; 

# endif

#else

    inline ByteOrder systemByteOrder()
    {
      union {
        int  i;
        char c;
      } u;

      u.i = 1;
      if(u.c == 1)
        return LittleEndian;
      else
        return BigEndian;
    }
    
    const ByteOrder SystemByteOrder = systemByteOrder(); 

#endif
  }
}

#endif

#endif
