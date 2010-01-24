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

#ifndef TAGLIB_ID3V1TAG_H
#define TAGLIB_ID3V1TAG_H

#include "tag.h"
#include "tbytevector.h"
#include "taglib_export.h"

namespace TagLib {

  class File;

  //! An ID3v1 implementation

  namespace ID3v1 {

    //! A abstraction for the string to data encoding in ID3v1 tags.

    /*!
     * ID3v1 should in theory always contain ISO-8859-1 (Latin1) data.  In
     * practice it does not.  TagLib by default only supports ISO-8859-1 data
     * in ID3v1 tags.
     *
     * However by subclassing this class and reimplementing parse() and render()
     * and setting your reimplementation as the default with
     * ID3v1::Tag::setStringHandler() you can define how you would like these
     * transformations to be done.
     *
     * \warning It is advisable <b>not</b> to write non-ISO-8859-1 data to ID3v1
     * tags.  Please consider disabling the writing of ID3v1 tags in the case
     * that the data is ISO-8859-1.
     *
     * \see ID3v1::Tag::setStringHandler()
     */

    class TAGLIB_EXPORT StringHandler
    {
    public:
      // BIC: Add virtual destructor.

      /*!
       * Decode a string from \a data.  The default implementation assumes that
       * \a data is an ISO-8859-1 (Latin1) character array.
       */
      virtual String parse(const ByteVector &data) const;

      /*!
       * Encode a ByteVector with the data from \a s.  The default implementation
       * assumes that \a s is an ISO-8859-1 (Latin1) string.  If the string is
       * does not conform to ISO-8859-1, no value is written.
       *
       * \warning It is recommended that you <b>not</b> override this method, but
       * instead do not write an ID3v1 tag in the case that the data is not
       * ISO-8859-1.
       */
      virtual ByteVector render(const String &s) const;
    };

    //! The main class in the ID3v1 implementation

    /*!
     * This is an implementation of the ID3v1 format.  ID3v1 is both the simplist
     * and most common of tag formats but is rather limited.  Because of its
     * pervasiveness and the way that applications have been written around the
     * fields that it provides, the generic TagLib::Tag API is a mirror of what is
     * provided by ID3v1.
     *
     * ID3v1 tags should generally only contain Latin1 information.  However because
     * many applications do not follow this rule there is now support for overriding
     * the ID3v1 string handling using the ID3v1::StringHandler class.  Please see
     * the documentation for that class for more information.
     *
     * \see StringHandler
     *
     * \note Most fields are truncated to a maximum of 28-30 bytes.  The
     * truncation happens automatically when the tag is rendered.
     */

    class TAGLIB_EXPORT Tag : public TagLib::Tag
    {
    public:
      /*!
       * Create an ID3v1 tag with default values.
       */
      Tag();

      /*!
       * Create an ID3v1 tag and parse the data in \a file starting at
       * \a tagOffset.
       */
      Tag(File *file, long tagOffset);

      /*!
       * Destroys this Tag instance.
       */
      virtual ~Tag();

      /*!
       * Renders the in memory values to a ByteVector suitable for writing to
       * the file.
       */
      ByteVector render() const;

      /*!
       * Returns the string "TAG" suitable for usage in locating the tag in a
       * file.
       */
      static ByteVector fileIdentifier();

      // Reimplementations.

      virtual String title() const;
      virtual String artist() const;
      virtual String album() const;
      virtual String comment() const;
      virtual String genre() const;
      virtual uint year() const;
      virtual uint track() const;

      virtual void setTitle(const String &s);
      virtual void setArtist(const String &s);
      virtual void setAlbum(const String &s);
      virtual void setComment(const String &s);
      virtual void setGenre(const String &s);
      virtual void setYear(uint i);
      virtual void setTrack(uint i);

      /*!
       * Sets the string handler that decides how the ID3v1 data will be
       * converted to and from binary data.
       *
       * \see StringHandler
       */
      static void setStringHandler(const StringHandler *handler);

    protected:
      /*!
       * Reads from the file specified in the constructor.
       */
      void read();
      /*!
       * Pareses the body of the tag in \a data.
       */
      void parse(const ByteVector &data);

    private:
      Tag(const Tag &);
      Tag &operator=(const Tag &);

      class TagPrivate;
      TagPrivate *d;
    };
  }
}

#endif
