/***************************************************************************
    copyright            : (C) 2012 by Tsuda Kageyu
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

#ifndef TAGLIB_INFOTAG_H
#define TAGLIB_INFOTAG_H

#include "tag.h"
#include "tmap.h"
#include "tstring.h"
#include "tstringlist.h"
#include "tbytevector.h"
#include "taglib_export.h"

namespace TagLib {

  class File;

  //! A RIFF Info tag implementation. 
  namespace RIFF {
  namespace Info {

    typedef Map<ByteVector, String> FieldListMap;

    //! A abstraction for the string to data encoding in Info tags.

    /*!
     * RIFF Info tag has no clear definitions about character encodings.
     * In practice, local encoding of each system is largely used and UTF-8 is
     * popular too.
     *
     * Here is an option to read and write tags in your preferrd encoding 
     * by subclassing this class, reimplementing parse() and render() and setting 
     * your reimplementation as the default with Info::Tag::setStringHandler().
     *
     * \see ID3v1::Tag::setStringHandler()
     */

    class TAGLIB_EXPORT StringHandler
    {
    public:
      StringHandler();
      ~StringHandler();

      /*!
       * Decode a string from \a data.  The default implementation assumes that
       * \a data is an UTF-8 character array.
       */
      virtual String parse(const ByteVector &data) const;

      /*!
       * Encode a ByteVector with the data from \a s.  The default implementation
       * assumes that \a s is an UTF-8 string. 
       */
      virtual ByteVector render(const String &s) const;
    };

    //! The main class in the ID3v2 implementation

    /*!
     * This is the main class in the INFO tag implementation.  RIFF INFO tag is a 
     * metadata format found in WAV audio and AVI video files.  Though it is a part 
     * of Microsoft/IBM's RIFF specification, the author could not find the official 
     * documents about it.  So, this implementation is referring to unofficial documents 
     * online and some applications' behaviors especially Windows Explorer.
     */
    class TAGLIB_EXPORT Tag : public TagLib::Tag
    {
    public:
      /*!
       * Constructs an empty Info tag.
       */
      Tag();

      /*!
       * Constructs an Info tag read from \a data which is contents of "LIST" chunk.
       */
      Tag(const ByteVector &data);

      virtual ~Tag();

      // Reimplementations

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

      virtual bool isEmpty() const;

      /*!
       * Returns a copy of the internal fields of the tag.  The returned map directly 
       * reflects the contents of the "INFO" chunk.
       *
       * \note Modifying this map does not affect the tag's internal data.
       * Use setFieldText() and removeField() instead.
       *
       * \see setFieldText()
       * \see removeField()
       */
      FieldListMap fieldListMap() const;

      /*
       * Gets the value of the field with the ID \a id.
       */
      String fieldText(const ByteVector &id) const;
        
      /*
        * Sets the value of the field with the ID \a id to \a s.
        * If the field does not exist, it is created.
        * If \s is empty, the field is removed.
        *
        * \note fieldId must be four-byte long pure ASCII string.  This function 
        * performs nothing if fieldId is invalid.
        */
      void setFieldText(const ByteVector &id, const String &s);

      /*
       * Removes the field with the ID \a id.
       */
      void removeField(const ByteVector &id);

      /*!
       * Render the tag back to binary data, suitable to be written to disk.
       *
       * \note Returns empty ByteVector is the tag contains no fields. 
       */
      ByteVector render() const;

      /*!
       * Sets the string handler that decides how the text data will be
       * converted to and from binary data.
       * If the parameter \a handler is null, the previous handler is
       * released and default UTF-8 handler is restored.
       *
       * \note The caller is responsible for deleting the previous handler
       * as needed after it is released.
       *
       * \see StringHandler
       */
      static void setStringHandler(const StringHandler *handler);
    
    protected:
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
  }}
}

#endif
