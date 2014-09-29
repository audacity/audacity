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
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *
 *   02110-1301  USA                                                       *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_STRING_H
#define TAGLIB_STRING_H

#include "taglib_export.h"
#include "taglib.h"
#include "tbytevector.h"

#include <string>
#include <iostream>

/*!
 * \relates TagLib::String
 *
 * Converts a QString to a TagLib::String without a requirement to link to Qt.
 *
 * \note consider conversion via usual char-by-char for loop to avoid UTF16->UTF8->UTF16
 * conversion happening in the background
 */
#define QStringToTString(s) TagLib::String(s.utf8().data(), TagLib::String::UTF8)

/*!
 * \relates TagLib::String
 *
 * Converts a TagLib::String to a QString without a requirement to link to Qt.
 *
 * \note consider conversion via usual char-by-char for loop to avoid UTF16->UTF8->UTF16
 * conversion happening in the background
 *
 */
#define TStringToQString(s) QString::fromUtf8(s.toCString(true))

namespace TagLib {

  class StringList;

  //! A \e wide string class suitable for unicode.

  /*!
   * This is an implicitly shared \e wide string.  For storage it uses
   * TagLib::wstring, but as this is an <i>implementation detail</i> this of
   * course could change.  Strings are stored internally as UTF-16(without BOM/
   * CPU byte order)
   *
   * The use of implicit sharing means that copying a string is cheap, the only
   * \e cost comes into play when the copy is modified.  Prior to that the string
   * just has a pointer to the data of the \e parent String.  This also makes
   * this class suitable as a function return type.
   *
   * In addition to adding implicit sharing, this class keeps track of four
   * possible encodings, which are the four supported by the ID3v2 standard.
   */

  class TAGLIB_EXPORT String
  {
  public:

#ifndef DO_NOT_DOCUMENT
    typedef std::basic_string<wchar>::iterator Iterator;
    typedef std::basic_string<wchar>::const_iterator ConstIterator;
#endif

    /**
     * The four types of string encodings supported by the ID3v2 specification.
     * ID3v1 is assumed to be Latin1 and Ogg Vorbis comments use UTF8.
     */
    enum Type {
      /*!
       * IS08859-1, or <i>Latin1</i> encoding.  8 bit characters.
       */
      Latin1 = 0,
      /*!
       * UTF16 with a <i>byte order mark</i>.  16 bit characters.
       */
      UTF16 = 1,
      /*!
       * UTF16 <i>big endian</i>.  16 bit characters.  This is the encoding used
       * internally by TagLib.
       */
      UTF16BE = 2,
      /*!
       * UTF8 encoding.  Characters are usually 8 bits but can be up to 32.
       */
      UTF8 = 3,
      /*!
       * UTF16 <i>little endian</i>.  16 bit characters.
       */
      UTF16LE = 4
    };

    /*!
     * Constructs an empty String.
     */
    String();

    /*!
     * Make a shallow, implicitly shared, copy of \a s.  Because this is
     * implicitly shared, this method is lightweight and suitable for
     * pass-by-value usage.
     */
    String(const String &s);

    /*!
     * Makes a deep copy of the data in \a s.
     *
     * \note This should only be used with the 8-bit codecs Latin1 and UTF8, when
     * used with other codecs it will simply print a warning and exit.
     */
    String(const std::string &s, Type t = Latin1);

    /*!
     * Makes a deep copy of the data in \a s.
     */
    String(const wstring &s, Type t = WCharByteOrder);

    /*!
     * Makes a deep copy of the data in \a s.
     */
    String(const wchar_t *s, Type t = WCharByteOrder);

    /*!
     * Makes a deep copy of the data in \a c.
     *
     * \note This should only be used with the 8-bit codecs Latin1 and UTF8, when
     * used with other codecs it will simply print a warning and exit.
     */
    String(char c, Type t = Latin1);

    /*!
     * Makes a deep copy of the data in \a c.
     */
    String(wchar_t c, Type t = Latin1);


    /*!
     * Makes a deep copy of the data in \a s.
     *
     * \note This should only be used with the 8-bit codecs Latin1 and UTF8, when
     * used with other codecs it will simply print a warning and exit.
     */
    String(const char *s, Type t = Latin1);

    /*!
     * Makes a deep copy of the data in \a s.
     *
     * \note This should only be used with the 8-bit codecs Latin1 and UTF8, when
     * used with other codecs it will simply print a warning and exit.
     */
    String(const ByteVector &v, Type t = Latin1);

    /*!
     * Destroys this String instance.
     */
    virtual ~String();

    /*!
     * Returns a deep copy of this String as an std::string.  The returned string 
     * is encoded in UTF8 if \a unicode is true, otherwise Latin1.
     *
     * \see toCString()
     */
    std::string to8Bit(bool unicode = false) const;

    /*!
     * Returns a deep copy of this String as a wstring.  The returned string is 
     * encoded in UTF-16 (without BOM/CPU byte order).
     *
     * \see toCWString()
     */
    wstring toWString() const;

    /*!
     * Creates and returns a standard C-style (null-terminated) version of this 
     * String.  The returned string is encoded in UTF8 if \a unicode is true, 
     * otherwise Latin1.
     * 
     * The returned string is still owned by this String and should not be deleted 
     * by the user.
     *
     * The returned pointer remains valid until this String instance is destroyed 
     * or toCString() is called again.
     *
     * \warning This however has the side effect that the returned string will remain
     * in memory <b>in addition to</b> other memory that is consumed by this 
     * String instance.  So, this method should not be used on large strings or
     * where memory is critical.  Consider using to8Bit() instead to avoid it.
     *
     * \see to8Bit()
     */
    const char *toCString(bool unicode = false) const;
    
    /*!
     * Returns a standard C-style (null-terminated) wide character version of 
     * this String.  The returned string is encoded in UTF-16 (without BOM/CPU byte 
     * order).
     * 
     * The returned string is still owned by this String and should not be deleted 
     * by the user.
     *
     * The returned pointer remains valid until this String instance is destroyed 
     * or any other method of this String is called.
     *
     * /note This returns a pointer to the String's internal data without any 
     * conversions.
     *
     * \see toWString()
     */
    const wchar_t *toCWString() const;
    
    /*!
     * Returns an iterator pointing to the beginning of the string.
     */
    Iterator begin();

    /*!
     * Returns a const iterator pointing to the beginning of the string.
     */
    ConstIterator begin() const;

    /*!
     * Returns an iterator pointing to the end of the string (the position
     * after the last character).
     */
    Iterator end();

    /*!
     * Returns a const iterator pointing to the end of the string (the position
     * after the last character).
     */
    ConstIterator end() const;

    /*!
     * Finds the first occurrence of pattern \a s in this string starting from
     * \a offset.  If the pattern is not found, -1 is returned.
     */
    int find(const String &s, int offset = 0) const;

    /*!
     * Finds the last occurrence of pattern \a s in this string, searched backwards,
     * either from the end of the string or starting from \a offset. If the pattern
     * is not found, -1 is returned.
     */
    int rfind(const String &s, int offset = -1) const;

    /*!
     * Splits the string on each occurrence of \a separator.
     */
    StringList split(const String &separator = " ") const;

    /*!
     * Returns true if the strings starts with the substring \a s.
     */
    bool startsWith(const String &s) const;

    /*!
     * Extract a substring from this string starting at \a position and
     * continuing for \a n characters.
     */
    String substr(uint position, uint n = 0xffffffff) const;

    /*!
     * Append \a s to the current string and return a reference to the current
     * string.
     */
    String &append(const String &s);

    /*!
     * Returns an upper case version of the string.
     *
     * \warning This only works for the characters in US-ASCII, i.e. A-Z.
     */
    String upper() const;

    /*!
     * Returns the size of the string.
     */
    uint size() const;

    /*!
     * Returns the length of the string.  Equivalent to size().
     */
    uint length() const;

    /*!
     * Returns true if the string is empty.
     *
     * \see isNull()
     */
    bool isEmpty() const;

    /*!
     * Returns true if this string is null -- i.e. it is a copy of the
     * String::null string.
     *
     * \note A string can be empty and not null.
     * \see isEmpty()
     */
    bool isNull() const;

    /*!
     * Returns a ByteVector containing the string's data.  If \a t is Latin1 or
     * UTF8, this will return a vector of 8 bit characters, otherwise it will use
     * 16 bit characters.
     */
    ByteVector data(Type t) const;

    /*!
     * Convert the string to an integer.
     *
     * Returns the integer if the conversion was successful or 0 if the
     * string does not represent a number.
     */
    // BIC: merge with the method below
    int toInt() const;

    /*!
     * Convert the string to an integer.
     *
     * If the conversion was successfull, it sets the value of \a *ok to
     * true and returns the integer. Otherwise it sets \a *ok to false
     * and the result is undefined.
     */
    int toInt(bool *ok) const;

    /*!
     * Returns a string with the leading and trailing whitespace stripped.
     */
    String stripWhiteSpace() const;

    /*!
     * Returns true if the file only uses characters required by Latin1.
     */
    bool isLatin1() const;

    /*!
     * Returns true if the file only uses characters required by (7-bit) ASCII.
     */
    bool isAscii() const;

    /*!
     * Converts the base-10 integer \a n to a string.
     */
    static String number(int n);

    /*!
     * Returns a reference to the character at position \a i.
     */
    wchar &operator[](int i);

    /*!
     * Returns a const reference to the character at position \a i.
     */
    const wchar &operator[](int i) const;

    /*!
     * Compares each character of the String with each character of \a s and
     * returns true if the strings match.
     */
    bool operator==(const String &s) const;

    /*!
     * Compares each character of the String with each character of \a s and
     * returns false if the strings match.
     */
    bool operator!=(const String &s) const;

    /*!
     * Appends \a s to the end of the String.
     */
    String &operator+=(const String &s);

    /*!
     * Appends \a s to the end of the String.
     */
    String &operator+=(const wchar_t* s);

    /*!
     * Appends \a s to the end of the String.
     */
    String &operator+=(const char* s);

    /*!
     * Appends \a s to the end of the String.
     */
    String &operator+=(wchar_t c);

    /*!
     * Appends \a c to the end of the String.
     */
    String &operator+=(char c);

    /*!
     * Performs a shallow, implicitly shared, copy of \a s, overwriting the
     * String's current data.
     */
    String &operator=(const String &s);

    /*!
     * Performs a deep copy of the data in \a s.
     */
    String &operator=(const std::string &s);

    /*!
     * Performs a deep copy of the data in \a s.
     */
    String &operator=(const wstring &s);

    /*!
     * Performs a deep copy of the data in \a s.
     */
    String &operator=(const wchar_t *s);

    /*!
     * Performs a deep copy of the data in \a s.
     */
    String &operator=(char c);

    /*!
     * Performs a deep copy of the data in \a s.
     */
    String &operator=(wchar_t c);

    /*!
     * Performs a deep copy of the data in \a s.
     */
    String &operator=(const char *s);

    /*!
     * Performs a deep copy of the data in \a v.
     */
    String &operator=(const ByteVector &v);

    /*!
     * To be able to use this class in a Map, this operator needed to be
     * implemented.  Returns true if \a s is less than this string in a bytewise
     * comparison.
     */
    bool operator<(const String &s) const;

    /*!
     * A null string provided for convenience.
     */
    static String null;

  protected:
    /*!
     * If this String is being shared via implicit sharing, do a deep copy of the
     * data and separate from the shared members.  This should be called by all
     * non-const subclass members.
     */
    void detach();

  private:
    /*!
     * Converts a \e Latin-1 string into \e UTF-16(without BOM/CPU byte order) 
     * and copies it to the internal buffer.
     */
    void copyFromLatin1(const char *s, size_t length);

    /*!
     * Converts a \e UTF-8 string into \e UTF-16(without BOM/CPU byte order) 
     * and copies it to the internal buffer.
     */
    void copyFromUTF8(const char *s, size_t length);

    /*!
     * Converts a \e UTF-16 (with BOM), UTF-16LE or UTF16-BE string into 
     * \e UTF-16(without BOM/CPU byte order) and copies it to the internal buffer.
     */
    void copyFromUTF16(const wchar_t *s, size_t length, Type t);

    /*!
     * Converts a \e UTF-16 (with BOM), UTF-16LE or UTF16-BE string into 
     * \e UTF-16(without BOM/CPU byte order) and copies it to the internal buffer.
     */
    void copyFromUTF16(const char *s, size_t length, Type t);
    
    /*!
     * Indicates which byte order of UTF-16 is used to store strings internally. 
     *
     * \note \e String::UTF16BE or \e String::UTF16LE
     */
    static const Type WCharByteOrder;

    class StringPrivate;
    StringPrivate *d;
  };
}

/*!
 * \relates TagLib::String
 *
 * Concatenates \a s1 and \a s2 and returns the result as a string.
 */
TAGLIB_EXPORT const TagLib::String operator+(const TagLib::String &s1, const TagLib::String &s2);

/*!
 * \relates TagLib::String
 *
 * Concatenates \a s1 and \a s2 and returns the result as a string.
 */
TAGLIB_EXPORT const TagLib::String operator+(const char *s1, const TagLib::String &s2);

/*!
 * \relates TagLib::String
 *
 * Concatenates \a s1 and \a s2 and returns the result as a string.
 */
TAGLIB_EXPORT const TagLib::String operator+(const TagLib::String &s1, const char *s2);


/*!
 * \relates TagLib::String
 *
 * Send the string to an output stream.
 */
TAGLIB_EXPORT std::ostream &operator<<(std::ostream &s, const TagLib::String &str);

#endif
