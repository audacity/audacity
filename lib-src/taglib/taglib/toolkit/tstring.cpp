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

#include "tstring.h"
#include "unicode.h"
#include "tdebug.h"

#include <iostream>

#include <string.h>

namespace TagLib {

  inline unsigned short byteSwap(unsigned short x)
  {
    return (((x) >> 8) & 0xff) | (((x) & 0xff) << 8);
  }

  inline unsigned short combine(unsigned char c1, unsigned char c2)
  {
    return (c1 << 8) | c2;
  }
}

using namespace TagLib;

class String::StringPrivate : public RefCounter
{
public:
  StringPrivate(const wstring &s) :
    RefCounter(),
    data(s),
    CString(0) {}

  StringPrivate() :
    RefCounter(),
    CString(0) {}

  ~StringPrivate() {
    delete [] CString;
  }

  wstring data;

  /*!
   * This is only used to hold the a pointer to the most recent value of
   * toCString.
   */
  char *CString;
};

String String::null;

////////////////////////////////////////////////////////////////////////////////

String::String()
{
  d = new StringPrivate;
}

String::String(const String &s) : d(s.d)
{
  d->ref();
}

String::String(const std::string &s, Type t)
{
  d = new StringPrivate;

  if(t == UTF16 || t == UTF16BE || t == UTF16LE) {
    debug("String::String() -- A std::string should not contain UTF16.");
    return;
  }

  int length = s.length();
  d->data.resize(length);
  wstring::iterator targetIt = d->data.begin();

  for(std::string::const_iterator it = s.begin(); it != s.end(); it++) {
    *targetIt = uchar(*it);
    ++targetIt;
  }

  prepare(t);
}

String::String(const wstring &s, Type t)
{
  d = new StringPrivate(s);
  prepare(t);
}

String::String(const wchar_t *s, Type t)
{
  d = new StringPrivate(s);
  prepare(t);
}

String::String(const char *s, Type t)
{
  d = new StringPrivate;

  if(t == UTF16 || t == UTF16BE || t == UTF16LE) {
    debug("String::String() -- A const char * should not contain UTF16.");
    return;
  }

  int length = ::strlen(s);
  d->data.resize(length);

  wstring::iterator targetIt = d->data.begin();

  for(int i = 0; i < length; i++) {
    *targetIt = uchar(s[i]);
    ++targetIt;
  }

  prepare(t);
}

String::String(wchar_t c, Type t)
{
  d = new StringPrivate;
  d->data += c;
  prepare(t);
}

String::String(char c, Type t)
{
  d = new StringPrivate;

  if(t == UTF16 || t == UTF16BE || t == UTF16LE) {
    debug("String::String() -- A std::string should not contain UTF16.");
    return;
  }

  d->data += uchar(c);
  prepare(t);
}

String::String(const ByteVector &v, Type t)
{
  d = new StringPrivate;

  if(v.isEmpty())
    return;

  if(t == Latin1 || t == UTF8) {

    int length = 0;
    d->data.resize(v.size());
    wstring::iterator targetIt = d->data.begin();
    for(ByteVector::ConstIterator it = v.begin(); it != v.end() && (*it); ++it) {
      *targetIt = uchar(*it);
      ++targetIt;
      ++length;
    }
    d->data.resize(length);
  }
  else  {
    d->data.resize(v.size() / 2);
    wstring::iterator targetIt = d->data.begin();

    for(ByteVector::ConstIterator it = v.begin();
        it != v.end() && it + 1 != v.end() && combine(*it, *(it + 1));
        it += 2)
    {
      *targetIt = combine(*it, *(it + 1));
      ++targetIt;
    }
  }
  prepare(t);
}

////////////////////////////////////////////////////////////////////////////////

String::~String()
{
  if(d->deref())
    delete d;
}

std::string String::to8Bit(bool unicode) const
{
  std::string s;
  s.resize(d->data.size());

  if(!unicode) {
    std::string::iterator targetIt = s.begin();
    for(wstring::const_iterator it = d->data.begin(); it != d->data.end(); it++) {
      *targetIt = char(*it);
      ++targetIt;
    }
    return s;
  }

  const int outputBufferSize = d->data.size() * 3 + 1;

  Unicode::UTF16 *sourceBuffer = new Unicode::UTF16[d->data.size() + 1];
  Unicode::UTF8  *targetBuffer = new Unicode::UTF8[outputBufferSize];

  for(unsigned int i = 0; i < d->data.size(); i++)
    sourceBuffer[i] = Unicode::UTF16(d->data[i]);

  const Unicode::UTF16 *source = sourceBuffer;
  Unicode::UTF8 *target = targetBuffer;

  Unicode::ConversionResult result =
    Unicode::ConvertUTF16toUTF8(&source, sourceBuffer + d->data.size(),
                                &target, targetBuffer + outputBufferSize,
                                Unicode::lenientConversion);

  if(result != Unicode::conversionOK)
    debug("String::to8Bit() - Unicode conversion error.");

  int newSize = target - targetBuffer;
  s.resize(newSize);
  targetBuffer[newSize] = 0;

  s = (char *) targetBuffer;

  delete [] sourceBuffer;
  delete [] targetBuffer;

  return s;
}

TagLib::wstring String::toWString() const
{
  return d->data;
}

const char *String::toCString(bool unicode) const
{
  delete [] d->CString;

  std::string buffer = to8Bit(unicode);
  d->CString = new char[buffer.size() + 1];
  strcpy(d->CString, buffer.c_str());

  return d->CString;
}

String::Iterator String::begin()
{
  return d->data.begin();
}

String::ConstIterator String::begin() const
{
  return d->data.begin();
}

String::Iterator String::end()
{
  return d->data.end();
}

String::ConstIterator String::end() const
{
  return d->data.end();
}

int String::find(const String &s, int offset) const
{
  wstring::size_type position = d->data.find(s.d->data, offset);

  if(position != wstring::npos)
    return position;
  else
    return -1;
}

bool String::startsWith(const String &s) const
{
  if(s.length() > length())
    return false;

  return substr(0, s.length()) == s;
}

String String::substr(uint position, uint n) const
{
  if(n > position + d->data.size())
    n = d->data.size() - position;

  String s;
  s.d->data = d->data.substr(position, n);
  return s;
}

String &String::append(const String &s)
{
  detach();
  d->data += s.d->data;
  return *this;
}

String String::upper() const
{
  String s;

  static int shift = 'A' - 'a';

  for(wstring::const_iterator it = d->data.begin(); it != d->data.end(); ++it) {
    if(*it >= 'a' && *it <= 'z')
      s.d->data.push_back(*it + shift);
    else
      s.d->data.push_back(*it);
  }

  return s;
}

TagLib::uint String::size() const
{
  return d->data.size();
}

TagLib::uint String::length() const
{
  return size();
}

bool String::isEmpty() const
{
  return d->data.size() == 0;
}

bool String::isNull() const
{
  return d == null.d;
}

ByteVector String::data(Type t) const
{
  ByteVector v;

  switch(t) {

  case Latin1:
  {
    for(wstring::const_iterator it = d->data.begin(); it != d->data.end(); it++)
      v.append(char(*it));
    break;
  }
  case UTF8:
  {
    std::string s = to8Bit(true);
    v.setData(s.c_str(), s.length());
    break;
  }
  case UTF16:
  {
    // Assume that if we're doing UTF16 and not UTF16BE that we want little
    // endian encoding.  (Byte Order Mark)

    v.append(char(0xff));
    v.append(char(0xfe));

    for(wstring::const_iterator it = d->data.begin(); it != d->data.end(); it++) {

      char c1 = *it & 0xff;
      char c2 = *it >> 8;

      v.append(c1);
      v.append(c2);
    }
    break;
  }
  case UTF16BE:
  {
    for(wstring::const_iterator it = d->data.begin(); it != d->data.end(); it++) {

      char c1 = *it >> 8;
      char c2 = *it & 0xff;

      v.append(c1);
      v.append(c2);
    }
    break;
  }
  case UTF16LE:
  {
    for(wstring::const_iterator it = d->data.begin(); it != d->data.end(); it++) {

      char c1 = *it & 0xff;
      char c2 = *it >> 8;

      v.append(c1);
      v.append(c2);
    }
    break;
  }
  }

  return v;
}

int String::toInt() const
{
  int value = 0;

  bool negative = d->data[0] == '-';
  uint i = negative ? 1 : 0;

  for(; i < d->data.size() && d->data[i] >= '0' && d->data[i] <= '9'; i++)
    value = value * 10 + (d->data[i] - '0');

  if(negative)
    value = value * -1;

  return value;
}

String String::stripWhiteSpace() const
{
  wstring::const_iterator begin = d->data.begin();
  wstring::const_iterator end = d->data.end();

  while(begin != end &&
        (*begin == '\t' || *begin == '\n' || *begin == '\f' ||
         *begin == '\r' || *begin == ' '))
  {
    ++begin;
  }

  if(begin == end)
    return null;

  // There must be at least one non-whitespace character here for us to have
  // gotten this far, so we should be safe not doing bounds checking.

  do {
    --end;
  } while(*end == '\t' || *end == '\n' ||
          *end == '\f' || *end == '\r' || *end == ' ');

  return String(wstring(begin, end + 1));
}

bool String::isLatin1() const
{
  for(wstring::const_iterator it = d->data.begin(); it != d->data.end(); it++) {
    if(*it >= 256)
      return false;
  }
  return true;
}

bool String::isAscii() const
{
  for(wstring::const_iterator it = d->data.begin(); it != d->data.end(); it++) {
    if(*it >= 128)
      return false;
  }
  return true;
}

String String::number(int n) // static
{
  if(n == 0)
    return String("0");

  String charStack;

  bool negative = n < 0;

  if(negative)
    n = n * -1;

  while(n > 0) {
    int remainder = n % 10;
    charStack += char(remainder + '0');
    n = (n - remainder) / 10;
  }

  String s;

  if(negative)
    s += '-';

  for(int i = charStack.d->data.size() - 1; i >= 0; i--)
    s += charStack.d->data[i];

  return s;
}

TagLib::wchar &String::operator[](int i)
{
  detach();

  return d->data[i];
}

const TagLib::wchar &String::operator[](int i) const
{
  return d->data[i];
}

bool String::operator==(const String &s) const
{
  return d == s.d || d->data == s.d->data;
}

String &String::operator+=(const String &s)
{
  detach();

  d->data += s.d->data;
  return *this;
}

String &String::operator+=(const wchar_t *s)
{
  detach();

  d->data += s;
  return *this;
}

String &String::operator+=(const char *s)
{
  detach();

  for(int i = 0; s[i] != 0; i++)
    d->data += uchar(s[i]);
  return *this;
}

String &String::operator+=(wchar_t c)
{
  detach();

  d->data += c;
  return *this;
}

String &String::operator+=(char c)
{
  d->data += uchar(c);
  return *this;
}

String &String::operator=(const String &s)
{
  if(&s == this)
    return *this;

  if(d->deref())
    delete d;
  d = s.d;
  d->ref();
  return *this;
}

String &String::operator=(const std::string &s)
{
  if(d->deref())
    delete d;

  d = new StringPrivate;

  d->data.resize(s.size());

  wstring::iterator targetIt = d->data.begin();
  for(std::string::const_iterator it = s.begin(); it != s.end(); it++) {
    *targetIt = uchar(*it);
    ++targetIt;
  }

  return *this;
}

String &String::operator=(const wstring &s)
{
  if(d->deref())
    delete d;
  d = new StringPrivate(s);
  return *this;
}

String &String::operator=(const wchar_t *s)
{
  if(d->deref())
    delete d;
  d = new StringPrivate(s);
  return *this;
}

String &String::operator=(char c)
{
  if(d->deref())
    delete d;
  d = new StringPrivate;
  d->data += uchar(c);
  return *this;
}

String &String::operator=(wchar_t c)
{
  if(d->deref())
    delete d;
  d = new StringPrivate;
  d->data += c;
  return *this;
}

String &String::operator=(const char *s)
{
  if(d->deref())
    delete d;

  d = new StringPrivate;

  int length = ::strlen(s);
  d->data.resize(length);

  wstring::iterator targetIt = d->data.begin();
  for(int i = 0; i < length; i++) {
    *targetIt = uchar(s[i]);
    ++targetIt;
  }

  return *this;
}

String &String::operator=(const ByteVector &v)
{
  if(d->deref())
    delete d;

  d = new StringPrivate;
  d->data.resize(v.size());
  wstring::iterator targetIt = d->data.begin();

  uint i = 0;

  for(ByteVector::ConstIterator it = v.begin(); it != v.end() && (*it); ++it) {
    *targetIt = uchar(*it);
    ++targetIt;
    ++i;
  }

  // If we hit a null in the ByteVector, shrink the string again.

  d->data.resize(i);

  return *this;
}

bool String::operator<(const String &s) const
{
  return d->data < s.d->data;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

void String::detach()
{
  if(d->count() > 1) {
    d->deref();
    d = new StringPrivate(d->data);
  }
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void String::prepare(Type t)
{
  switch(t) {
  case UTF16:
  {
    if(d->data.size() >= 1 && (d->data[0] == 0xfeff || d->data[0] == 0xfffe)) {
      bool swap = d->data[0] != 0xfeff;
      d->data.erase(d->data.begin(), d->data.begin() + 1);
      if(swap) {
        for(uint i = 0; i < d->data.size(); i++)
          d->data[i] = byteSwap((unsigned short)d->data[i]);
      }
    }
    else {
      debug("String::prepare() - Invalid UTF16 string.");
      d->data.erase(d->data.begin(), d->data.end());
    }
    break;
  }
  case UTF8:
  {
    int bufferSize = d->data.size() + 1;
    Unicode::UTF8  *sourceBuffer = new Unicode::UTF8[bufferSize];
    Unicode::UTF16 *targetBuffer = new Unicode::UTF16[bufferSize];

    unsigned int i = 0;
    for(; i < d->data.size(); i++)
      sourceBuffer[i] = Unicode::UTF8(d->data[i]);
    sourceBuffer[i] = 0;

    const Unicode::UTF8 *source = sourceBuffer;
    Unicode::UTF16 *target = targetBuffer;

    Unicode::ConversionResult result =
      Unicode::ConvertUTF8toUTF16(&source, sourceBuffer + bufferSize,
                                  &target, targetBuffer + bufferSize,
                                  Unicode::lenientConversion);

    if(result != Unicode::conversionOK)
      debug("String::prepare() - Unicode conversion error.");


    int newSize = target != targetBuffer ? target - targetBuffer - 1 : 0;
    d->data.resize(newSize);

    for(int i = 0; i < newSize; i++)
      d->data[i] = targetBuffer[i];

    delete [] sourceBuffer;
    delete [] targetBuffer;

    break;
  }
  case UTF16LE:
  {
    for(uint i = 0; i < d->data.size(); i++)
      d->data[i] = byteSwap((unsigned short)d->data[i]);
    break;
  }
  default:
    break;
  }
}

////////////////////////////////////////////////////////////////////////////////
// related functions
////////////////////////////////////////////////////////////////////////////////

const TagLib::String operator+(const TagLib::String &s1, const TagLib::String &s2)
{
  String s(s1);
  s.append(s2);
  return s;
}

const TagLib::String operator+(const char *s1, const TagLib::String &s2)
{
  String s(s1);
  s.append(s2);
  return s;
}

const TagLib::String operator+(const TagLib::String &s1, const char *s2)
{
  String s(s1);
  s.append(s2);
  return s;
}

std::ostream &operator<<(std::ostream &s, const String &str)
{
  s << str.to8Bit();
  return s;
}
