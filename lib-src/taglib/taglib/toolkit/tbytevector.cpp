/***************************************************************************
    copyright            : (C) 2002 - 2008 by Scott Wheeler
    email                : wheeler@kde.org
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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#include <iostream>

#include <tstring.h>
#include <tdebug.h>

#include <string.h>

#include "tbytevector.h"

// This is a bit ugly to keep writing over and over again.

// A rather obscure feature of the C++ spec that I hadn't thought of that makes
// working with C libs much more effecient.  There's more here:
//
// http://www.informit.com/isapi/product_id~{9C84DAB4-FE6E-49C5-BB0A-FB50331233EA}/content/index.asp

#define DATA(x) (&(x->data[0]))

namespace TagLib {
  static const uint crcTable[256] = {
    0x00000000, 0x04c11db7, 0x09823b6e, 0x0d4326d9, 0x130476dc, 0x17c56b6b,
    0x1a864db2, 0x1e475005, 0x2608edb8, 0x22c9f00f, 0x2f8ad6d6, 0x2b4bcb61,
    0x350c9b64, 0x31cd86d3, 0x3c8ea00a, 0x384fbdbd, 0x4c11db70, 0x48d0c6c7,
    0x4593e01e, 0x4152fda9, 0x5f15adac, 0x5bd4b01b, 0x569796c2, 0x52568b75,
    0x6a1936c8, 0x6ed82b7f, 0x639b0da6, 0x675a1011, 0x791d4014, 0x7ddc5da3,
    0x709f7b7a, 0x745e66cd, 0x9823b6e0, 0x9ce2ab57, 0x91a18d8e, 0x95609039,
    0x8b27c03c, 0x8fe6dd8b, 0x82a5fb52, 0x8664e6e5, 0xbe2b5b58, 0xbaea46ef,
    0xb7a96036, 0xb3687d81, 0xad2f2d84, 0xa9ee3033, 0xa4ad16ea, 0xa06c0b5d,
    0xd4326d90, 0xd0f37027, 0xddb056fe, 0xd9714b49, 0xc7361b4c, 0xc3f706fb,
    0xceb42022, 0xca753d95, 0xf23a8028, 0xf6fb9d9f, 0xfbb8bb46, 0xff79a6f1,
    0xe13ef6f4, 0xe5ffeb43, 0xe8bccd9a, 0xec7dd02d, 0x34867077, 0x30476dc0,
    0x3d044b19, 0x39c556ae, 0x278206ab, 0x23431b1c, 0x2e003dc5, 0x2ac12072,
    0x128e9dcf, 0x164f8078, 0x1b0ca6a1, 0x1fcdbb16, 0x018aeb13, 0x054bf6a4,
    0x0808d07d, 0x0cc9cdca, 0x7897ab07, 0x7c56b6b0, 0x71159069, 0x75d48dde,
    0x6b93dddb, 0x6f52c06c, 0x6211e6b5, 0x66d0fb02, 0x5e9f46bf, 0x5a5e5b08,
    0x571d7dd1, 0x53dc6066, 0x4d9b3063, 0x495a2dd4, 0x44190b0d, 0x40d816ba,
    0xaca5c697, 0xa864db20, 0xa527fdf9, 0xa1e6e04e, 0xbfa1b04b, 0xbb60adfc,
    0xb6238b25, 0xb2e29692, 0x8aad2b2f, 0x8e6c3698, 0x832f1041, 0x87ee0df6,
    0x99a95df3, 0x9d684044, 0x902b669d, 0x94ea7b2a, 0xe0b41de7, 0xe4750050,
    0xe9362689, 0xedf73b3e, 0xf3b06b3b, 0xf771768c, 0xfa325055, 0xfef34de2,
    0xc6bcf05f, 0xc27dede8, 0xcf3ecb31, 0xcbffd686, 0xd5b88683, 0xd1799b34,
    0xdc3abded, 0xd8fba05a, 0x690ce0ee, 0x6dcdfd59, 0x608edb80, 0x644fc637,
    0x7a089632, 0x7ec98b85, 0x738aad5c, 0x774bb0eb, 0x4f040d56, 0x4bc510e1,
    0x46863638, 0x42472b8f, 0x5c007b8a, 0x58c1663d, 0x558240e4, 0x51435d53,
    0x251d3b9e, 0x21dc2629, 0x2c9f00f0, 0x285e1d47, 0x36194d42, 0x32d850f5,
    0x3f9b762c, 0x3b5a6b9b, 0x0315d626, 0x07d4cb91, 0x0a97ed48, 0x0e56f0ff,
    0x1011a0fa, 0x14d0bd4d, 0x19939b94, 0x1d528623, 0xf12f560e, 0xf5ee4bb9,
    0xf8ad6d60, 0xfc6c70d7, 0xe22b20d2, 0xe6ea3d65, 0xeba91bbc, 0xef68060b,
    0xd727bbb6, 0xd3e6a601, 0xdea580d8, 0xda649d6f, 0xc423cd6a, 0xc0e2d0dd,
    0xcda1f604, 0xc960ebb3, 0xbd3e8d7e, 0xb9ff90c9, 0xb4bcb610, 0xb07daba7,
    0xae3afba2, 0xaafbe615, 0xa7b8c0cc, 0xa379dd7b, 0x9b3660c6, 0x9ff77d71,
    0x92b45ba8, 0x9675461f, 0x8832161a, 0x8cf30bad, 0x81b02d74, 0x857130c3,
    0x5d8a9099, 0x594b8d2e, 0x5408abf7, 0x50c9b640, 0x4e8ee645, 0x4a4ffbf2,
    0x470cdd2b, 0x43cdc09c, 0x7b827d21, 0x7f436096, 0x7200464f, 0x76c15bf8,
    0x68860bfd, 0x6c47164a, 0x61043093, 0x65c52d24, 0x119b4be9, 0x155a565e,
    0x18197087, 0x1cd86d30, 0x029f3d35, 0x065e2082, 0x0b1d065b, 0x0fdc1bec,
    0x3793a651, 0x3352bbe6, 0x3e119d3f, 0x3ad08088, 0x2497d08d, 0x2056cd3a,
    0x2d15ebe3, 0x29d4f654, 0xc5a92679, 0xc1683bce, 0xcc2b1d17, 0xc8ea00a0,
    0xd6ad50a5, 0xd26c4d12, 0xdf2f6bcb, 0xdbee767c, 0xe3a1cbc1, 0xe760d676,
    0xea23f0af, 0xeee2ed18, 0xf0a5bd1d, 0xf464a0aa, 0xf9278673, 0xfde69bc4,
    0x89b8fd09, 0x8d79e0be, 0x803ac667, 0x84fbdbd0, 0x9abc8bd5, 0x9e7d9662,
    0x933eb0bb, 0x97ffad0c, 0xafb010b1, 0xab710d06, 0xa6322bdf, 0xa2f33668,
    0xbcb4666d, 0xb8757bda, 0xb5365d03, 0xb1f740b4
  };

  /*!
   * A templatized KMP find that works both with a ByteVector and a ByteVectorMirror.
   */

  template <class Vector>
  int vectorFind(const Vector &v, const Vector &pattern, uint offset, int byteAlign)
  {
    if(pattern.size() > v.size() || offset > v.size() - 1)
      return -1;

    // Let's go ahead and special case a pattern of size one since that's common
    // and easy to make fast.

    if(pattern.size() == 1) {
      char p = pattern[0];
      for(uint i = offset; i < v.size(); i++) {
        if(v[i] == p && (i - offset) % byteAlign == 0)
          return i;
      }
      return -1;
    }

    uchar lastOccurrence[256];

    for(uint i = 0; i < 256; ++i)
      lastOccurrence[i] = uchar(pattern.size());

    for(uint i = 0; i < pattern.size() - 1; ++i)
      lastOccurrence[uchar(pattern[i])] = uchar(pattern.size() - i - 1);

    for(uint i = pattern.size() - 1 + offset; i < v.size(); i += lastOccurrence[uchar(v.at(i))]) {
      int iBuffer = i;
      int iPattern = pattern.size() - 1;

      while(iPattern >= 0 && v.at(iBuffer) == pattern[iPattern]) {
        --iBuffer;
        --iPattern;
      }

      if(-1 == iPattern && (iBuffer + 1 - offset) % byteAlign == 0)
        return iBuffer + 1;
    }

    return -1;
  }

  /*!
   * Wraps the accessors to a ByteVector to make the search algorithm access the
   * elements in reverse.
   *
   * \see vectorFind()
   * \see ByteVector::rfind()
   */

  class ByteVectorMirror
  {
  public:
    ByteVectorMirror(const ByteVector &source) : v(source) {}

    char operator[](int index) const
    {
      return v[v.size() - index - 1];
    }

    char at(int index) const
    {
      return v.at(v.size() - index - 1);
    }

    ByteVectorMirror mid(uint index, uint length = 0xffffffff) const
    {
      return length == 0xffffffff ? v.mid(0, index) : v.mid(index - length, length);
    }

    uint size() const
    {
      return v.size();
    }

    int find(const ByteVectorMirror &pattern, uint offset = 0, int byteAlign = 1) const
    {
      ByteVectorMirror v(*this);

      if(offset > 0) {
        offset = size() - offset - pattern.size();
        if(offset >= size())
          offset = 0;
      }

      const int pos = vectorFind<ByteVectorMirror>(v, pattern, offset, byteAlign);

      // If the offset is zero then we need to adjust the location in the search
      // to be appropriately reversed.  If not we need to account for the fact
      // that the recursive call (called from the above line) has already ajusted
      // for this but that the normal templatized find above will add the offset
      // to the returned value.
      //
      // This is a little confusing at first if you don't first stop to think
      // through the logic involved in the forward search.

      if(pos == -1)
        return -1;

      return size() - pos - pattern.size();
    }

  private:
    const ByteVector &v;
  };

  template <class T>
  T toNumber(const std::vector<char> &data, bool mostSignificantByteFirst)
  {
    T sum = 0;

    if(data.size() <= 0) {
      debug("ByteVectorMirror::toNumber<T>() -- data is empty, returning 0");
      return sum;
    }

    uint size = sizeof(T);
    uint last = data.size() > size ? size - 1 : data.size() - 1;

    for(uint i = 0; i <= last; i++)
      sum |= (T) uchar(data[i]) << ((mostSignificantByteFirst ? last - i : i) * 8);

    return sum;
  }

  template <class T>
  ByteVector fromNumber(T value, bool mostSignificantByteFirst)
  {
    int size = sizeof(T);

    ByteVector v(size, 0);

    for(int i = 0; i < size; i++)
      v[i] = uchar(value >> ((mostSignificantByteFirst ? size - 1 - i : i) * 8) & 0xff);

    return v;
  }
}

using namespace TagLib;

class ByteVector::ByteVectorPrivate : public RefCounter
{
public:
  ByteVectorPrivate() : RefCounter(), size(0) {}
  ByteVectorPrivate(const std::vector<char> &v) : RefCounter(), data(v), size(v.size()) {}
  ByteVectorPrivate(TagLib::uint len, char value) : RefCounter(), data(len, value), size(len) {}

  std::vector<char> data;

  // std::vector<T>::size() is very slow, so we'll cache the value

  uint size;
};

////////////////////////////////////////////////////////////////////////////////
// static members
////////////////////////////////////////////////////////////////////////////////

ByteVector ByteVector::null;

ByteVector ByteVector::fromCString(const char *s, uint length)
{
  ByteVector v;

  if(length == 0xffffffff)
    v.setData(s);
  else
    v.setData(s, length);

  return v;
}

ByteVector ByteVector::fromUInt(uint value, bool mostSignificantByteFirst)
{
  return fromNumber<uint>(value, mostSignificantByteFirst);
}

ByteVector ByteVector::fromShort(short value, bool mostSignificantByteFirst)
{
  return fromNumber<short>(value, mostSignificantByteFirst);
}

ByteVector ByteVector::fromLongLong(long long value, bool mostSignificantByteFirst)
{
  return fromNumber<long long>(value, mostSignificantByteFirst);
}

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

ByteVector::ByteVector()
{
  d = new ByteVectorPrivate;
}

ByteVector::ByteVector(uint size, char value)
{
  d = new ByteVectorPrivate(size, value);
}

ByteVector::ByteVector(const ByteVector &v) : d(v.d)
{
  d->ref();
}

ByteVector::ByteVector(char c)
{
  d = new ByteVectorPrivate;
  d->data.push_back(c);
  d->size = 1;
}

ByteVector::ByteVector(const char *data, uint length)
{
  d = new ByteVectorPrivate;
  setData(data, length);
}

ByteVector::ByteVector(const char *data)
{
  d = new ByteVectorPrivate;
  setData(data);
}

ByteVector::~ByteVector()
{
  if(d->deref())
    delete d;
}

ByteVector &ByteVector::setData(const char *data, uint length)
{
  detach();

  resize(length);

  if(length > 0)
    ::memcpy(DATA(d), data, length);

  return *this;
}

ByteVector &ByteVector::setData(const char *data)
{
  return setData(data, ::strlen(data));
}

char *ByteVector::data()
{
  detach();
  return size() > 0 ? DATA(d) : 0;
}

const char *ByteVector::data() const
{
  return size() > 0 ? DATA(d) : 0;
}

ByteVector ByteVector::mid(uint index, uint length) const
{
  ByteVector v;

  if(index > size())
    return v;

  ConstIterator endIt;

  if(length < 0xffffffff && length + index < size())
    endIt = d->data.begin() + index + length;
  else
    endIt = d->data.end();

  v.d->data.insert(v.d->data.begin(), ConstIterator(d->data.begin() + index), endIt);
  v.d->size = v.d->data.size();

  return v;
}

char ByteVector::at(uint index) const
{
  return index < size() ? d->data[index] : 0;
}

int ByteVector::find(const ByteVector &pattern, uint offset, int byteAlign) const
{
  return vectorFind<ByteVector>(*this, pattern, offset, byteAlign);
}

int ByteVector::rfind(const ByteVector &pattern, uint offset, int byteAlign) const
{
  // Ok, this is a little goofy, but pretty cool after it sinks in.  Instead of
  // reversing the find method's Boyer-Moore search algorithm I created a "mirror"
  // for a ByteVector to reverse the behavior of the accessors.

  ByteVectorMirror v(*this);
  ByteVectorMirror p(pattern);

  return v.find(p, offset, byteAlign);
}

bool ByteVector::containsAt(const ByteVector &pattern, uint offset, uint patternOffset, uint patternLength) const
{
  if(pattern.size() < patternLength)
    patternLength = pattern.size();

  // do some sanity checking -- all of these things are needed for the search to be valid

  if(patternLength > size() || offset >= size() || patternOffset >= pattern.size() || patternLength == 0)
    return false;

  // loop through looking for a mismatch

  for(uint i = 0; i < patternLength - patternOffset; i++) {
    if(at(i + offset) != pattern[i + patternOffset])
      return false;
  }

  return true;
}

bool ByteVector::startsWith(const ByteVector &pattern) const
{
  return containsAt(pattern, 0);
}

bool ByteVector::endsWith(const ByteVector &pattern) const
{
  return containsAt(pattern, size() - pattern.size());
}

ByteVector &ByteVector::replace(const ByteVector &pattern, const ByteVector &with)
{
  if(pattern.size() == 0 || pattern.size() > size())
    return *this;

  const int patternSize = pattern.size();
  const int withSize = with.size();

  int offset = find(pattern);

  while(offset >= 0) {

    const int originalSize = size();

    if(withSize > patternSize)
      resize(originalSize + withSize - patternSize);

    if(patternSize != withSize)
      ::memcpy(data() + offset + withSize, mid(offset + patternSize).data(), originalSize - offset - patternSize);

    if(withSize < patternSize)
      resize(originalSize + withSize - patternSize);

    ::memcpy(data() + offset, with.data(), withSize);

    offset = find(pattern, offset + withSize);
  }

  return *this;
}

int ByteVector::endsWithPartialMatch(const ByteVector &pattern) const
{
  if(pattern.size() > size())
    return -1;

  const int startIndex = size() - pattern.size();

  // try to match the last n-1 bytes from the vector (where n is the pattern
  // size) -- continue trying to match n-2, n-3...1 bytes

  for(uint i = 1; i < pattern.size(); i++) {
    if(containsAt(pattern, startIndex + i, 0, pattern.size() - i))
      return startIndex + i;
  }

  return -1;
}

ByteVector &ByteVector::append(const ByteVector &v)
{
  if(v.d->size == 0)
    return *this; // Simply return if appending nothing.

  detach();

  uint originalSize = d->size;
  resize(d->size + v.d->size);
  ::memcpy(DATA(d) + originalSize, DATA(v.d), v.size());

  return *this;
}

ByteVector &ByteVector::clear()
{
  detach();
  d->data.clear();
  d->size = 0;

  return *this;
}

TagLib::uint ByteVector::size() const
{
  return d->size;
}

ByteVector &ByteVector::resize(uint size, char padding)
{
  if(d->size < size) {
    d->data.reserve(size);
    d->data.insert(d->data.end(), size - d->size, padding);
  }
  else
    d->data.erase(d->data.begin() + size, d->data.end());

  d->size = size;

  return *this;
}

ByteVector::Iterator ByteVector::begin()
{
  return d->data.begin();
}

ByteVector::ConstIterator ByteVector::begin() const
{
  return d->data.begin();
}

ByteVector::Iterator ByteVector::end()
{
  return d->data.end();
}

ByteVector::ConstIterator ByteVector::end() const
{
  return d->data.end();
}

bool ByteVector::isNull() const
{
  return d == null.d;
}

bool ByteVector::isEmpty() const
{
  return d->data.size() == 0;
}

TagLib::uint ByteVector::checksum() const
{
  uint sum = 0;
  for(ByteVector::ConstIterator it = begin(); it != end(); ++it)
    sum = (sum << 8) ^ crcTable[((sum >> 24) & 0xff) ^ uchar(*it)];
  return sum;
}

TagLib::uint ByteVector::toUInt(bool mostSignificantByteFirst) const
{
  return toNumber<uint>(d->data, mostSignificantByteFirst);
}

short ByteVector::toShort(bool mostSignificantByteFirst) const
{
  return toNumber<unsigned short>(d->data, mostSignificantByteFirst);
}

long long ByteVector::toLongLong(bool mostSignificantByteFirst) const
{
  return toNumber<unsigned long long>(d->data, mostSignificantByteFirst);
}

const char &ByteVector::operator[](int index) const
{
  return d->data[index];
}

char &ByteVector::operator[](int index)
{
  detach();

  return d->data[index];
}

bool ByteVector::operator==(const ByteVector &v) const
{
  if(d->size != v.d->size)
    return false;

  return ::memcmp(data(), v.data(), size()) == 0;
}

bool ByteVector::operator!=(const ByteVector &v) const
{
  return !operator==(v);
}

bool ByteVector::operator==(const char *s) const
{
  if(d->size != ::strlen(s))
    return false;

  return ::memcmp(data(), s, d->size) == 0;
}

bool ByteVector::operator!=(const char *s) const
{
  return !operator==(s);
}

bool ByteVector::operator<(const ByteVector &v) const
{
  int result = ::memcmp(data(), v.data(), d->size < v.d->size ? d->size : v.d->size);

  if(result != 0)
    return result < 0;
  else
    return size() < v.size();
}

bool ByteVector::operator>(const ByteVector &v) const
{
  return v < *this;
}

ByteVector ByteVector::operator+(const ByteVector &v) const
{
  ByteVector sum(*this);
  sum.append(v);
  return sum;
}

ByteVector &ByteVector::operator=(const ByteVector &v)
{
  if(&v == this)
    return *this;

  if(d->deref())
    delete d;

  d = v.d;
  d->ref();
  return *this;
}

ByteVector &ByteVector::operator=(char c)
{
  *this = ByteVector(c);
  return *this;
}

ByteVector &ByteVector::operator=(const char *data)
{
  *this = ByteVector(data);
  return *this;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

void ByteVector::detach()
{
  if(d->count() > 1) {
    d->deref();
    d = new ByteVectorPrivate(d->data);
  }
}

////////////////////////////////////////////////////////////////////////////////
// related functions
////////////////////////////////////////////////////////////////////////////////

std::ostream &operator<<(std::ostream &s, const ByteVector &v)
{
  for(TagLib::uint i = 0; i < v.size(); i++)
    s << v[i];
  return s;
}
