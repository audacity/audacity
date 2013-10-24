/***************************************************************************
    copyright           : (C) 2011 by Mathias Panzenböck
    email               : grosser.meister.morti@gmx.net
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it  under the terms of the GNU Lesser General Public License version  *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *
 *   MA  02110-1301  USA                                                   *
 ***************************************************************************/

#ifndef TAGLIB_MODFILEPRIVATE_H
#define TAGLIB_MODFILEPRIVATE_H

// some helper-macros only used internally by (s3m|it|xm)file.cpp
#define READ_ASSERT(cond) \
  if(!(cond)) \
  { \
    setValid(false); \
    return; \
  }

#define READ(setter,type,read) \
  { \
    type number; \
    READ_ASSERT(read(number)); \
    setter(number); \
  }

#define READ_BYTE(setter) READ(setter,uchar,readByte)
#define READ_U16L(setter) READ(setter,ushort,readU16L)
#define READ_U32L(setter) READ(setter,ulong,readU32L)
#define READ_U16B(setter) READ(setter,ushort,readU16B)
#define READ_U32B(setter) READ(setter,ulong,readU32B)

#define READ_STRING(setter,size) \
  { \
    String s; \
    READ_ASSERT(readString(s, size)); \
    setter(s); \
  }

#define READ_AS(type,name,read) \
  type name = 0; \
  READ_ASSERT(read(name));

#define READ_BYTE_AS(name) READ_AS(uchar,name,readByte)
#define READ_U16L_AS(name) READ_AS(ushort,name,readU16L)
#define READ_U32L_AS(name) READ_AS(ulong,name,readU32L)
#define READ_U16B_AS(name) READ_AS(ushort,name,readU16B)
#define READ_U32B_AS(name) READ_AS(ulong,name,readU32B)

#define READ_STRING_AS(name,size) \
  String name; \
  READ_ASSERT(readString(name, size));

#endif
