/***************************************************************************
    copyright            : (C) 2002 - 2008 by Scott Wheeler
    email                : wheeler@kde.org
    copyright            : (C) 2006 by Aaron VonderHaar
    email                : avh4@users.sourceforge.net
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

#ifndef TAGLIB_GENERALENCAPSULATEDOBJECT_H
#define TAGLIB_GENERALENCAPSULATEDOBJECT_H

#include "id3v2frame.h"
#include "id3v2header.h"
#include "taglib_export.h"

namespace TagLib {

  namespace ID3v2 {

    //! An ID3v2 general encapsulated object frame implementation

    /*!
     * This is an implementation of ID3v2 general encapsulated objects.
     * Arbitrary binary data may be included in tags, stored in GEOB frames.
     * There may be multiple GEOB frames in a single tag.  Each GEOB it
     * labelled with a content description (which may be blank), a required
     * mime-type, and a file name (may be blank).  The content description
     * uniquely identifies the GEOB frame in the tag.
     */

    class TAGLIB_EXPORT GeneralEncapsulatedObjectFrame : public Frame
    {
      friend class FrameFactory;

    public:

      /*!
       * Constructs an empty object frame.  The description, file name and text
       * encoding should be set manually.
       */
      GeneralEncapsulatedObjectFrame();

      /*!
       * Constructs a GeneralEncapsulatedObjectFrame frame based on \a data.
       *
       * \warning This is \em not data for the encapsulated object, for that use
       * setObject().  This constructor is used when reading the frame from the
       * disk.
       */
      explicit GeneralEncapsulatedObjectFrame(const ByteVector &data);

      /*!
       * Destroys the GeneralEncapsulatedObjectFrame instance.
       */
      virtual ~GeneralEncapsulatedObjectFrame();

      /*!
       * Returns a string containing the description, file name and mime-type
       */
      virtual String toString() const;

      /*!
       * Returns the text encoding used for the description and file name.
       *
       * \see setTextEncoding()
       * \see description()
       * \see fileName()
       */
      String::Type textEncoding() const;

      /*!
       * Set the text encoding used for the description and file name.
       *
       * \see description()
       * \see fileName()
       */
      void setTextEncoding(String::Type encoding);

      /*!
       * Returns the mime type of the object.
       */
      String mimeType() const;

      /*!
       * Sets the mime type of the object.
       */
      void setMimeType(const String &type);

      /*!
       * Returns the file name of the object.
       *
       * \see setFileName()
       */
      String fileName() const;

      /*!
       * Sets the file name for the object.
       *
       * \see fileName()
       */
      void setFileName(const String &name);

      /*!
       * Returns the content description of the object.
       *
       * \see setDescription()
       * \see textEncoding()
       * \see setTextEncoding()
       */

      String description() const;

      /*!
       * Sets the content description of the object to \a desc.
       *
       * \see description()
       * \see textEncoding()
       * \see setTextEncoding()
       */

      void setDescription(const String &desc);

      /*!
       * Returns the object data as a ByteVector.
       *
       * \note ByteVector has a data() method that returns a const char * which
       * should make it easy to export this data to external programs.
       *
       * \see setObject()
       * \see mimeType()
       */
      ByteVector object() const;

      /*!
       * Sets the object data to \a data.  \a data should be of the type specified in
       * this frame's mime-type specification.
       *
       * \see object()
       * \see mimeType()
       * \see setMimeType()
       */
      void setObject(const ByteVector &object);

    protected:
      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

    private:
      GeneralEncapsulatedObjectFrame(const ByteVector &data, Header *h);
      GeneralEncapsulatedObjectFrame(const GeneralEncapsulatedObjectFrame &);
      GeneralEncapsulatedObjectFrame &operator=(const GeneralEncapsulatedObjectFrame &);

      class GeneralEncapsulatedObjectFramePrivate;
      GeneralEncapsulatedObjectFramePrivate *d;
    };
  }
}

#endif
