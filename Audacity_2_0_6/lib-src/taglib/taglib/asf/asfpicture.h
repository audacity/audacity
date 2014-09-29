/**************************************************************************
    copyright            : (C) 2010 by Anton Sergunov
    email                : setosha@gmail.com
 **************************************************************************/

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

#ifndef ASFPICTURE_H
#define ASFPICTURE_H

#include "tstring.h"
#include "tbytevector.h"
#include "taglib_export.h"
#include "attachedpictureframe.h"

namespace TagLib
{
  namespace ASF
  {

    //! An ASF attached picture interface implementation

    /*!
     * This is an implementation of ASF attached pictures interface.  Pictures may be
     * included in attributes, one per WM/Picture attribute (but there may be multiple WM/Picture
     * attribute in a single tag).  These pictures are usually in either JPEG or
     * PNG format.
     * \see Attribute::toPicture()
     * \see Attribute::Attribute(const Picture& picture)
     */
    class TAGLIB_EXPORT Picture {
    public:

      /*!
       * This describes the function or content of the picture.
       */
      enum Type {
        //! A type not enumerated below
        Other              = 0x00,
        //! 32x32 PNG image that should be used as the file icon
        FileIcon           = 0x01,
        //! File icon of a different size or format
        OtherFileIcon      = 0x02,
        //! Front cover image of the album
        FrontCover         = 0x03,
        //! Back cover image of the album
        BackCover          = 0x04,
        //! Inside leaflet page of the album
        LeafletPage        = 0x05,
        //! Image from the album itself
        Media              = 0x06,
        //! Picture of the lead artist or soloist
        LeadArtist         = 0x07,
        //! Picture of the artist or performer
        Artist             = 0x08,
        //! Picture of the conductor
        Conductor          = 0x09,
        //! Picture of the band or orchestra
        Band               = 0x0A,
        //! Picture of the composer
        Composer           = 0x0B,
        //! Picture of the lyricist or text writer
        Lyricist           = 0x0C,
        //! Picture of the recording location or studio
        RecordingLocation  = 0x0D,
        //! Picture of the artists during recording
        DuringRecording    = 0x0E,
        //! Picture of the artists during performance
        DuringPerformance  = 0x0F,
        //! Picture from a movie or video related to the track
        MovieScreenCapture = 0x10,
        //! Picture of a large, coloured fish
        ColouredFish       = 0x11,
        //! Illustration related to the track
        Illustration       = 0x12,
        //! Logo of the band or performer
        BandLogo           = 0x13,
        //! Logo of the publisher (record company)
        PublisherLogo      = 0x14
      };

      /*!
       * Constructs an empty picture.
       */
      Picture();

      /*!
       * Construct an picture as a copy of \a other.
       */
      Picture(const Picture& other);

      /*!
       * Destroys the picture.
       */
      virtual ~Picture();

      /*!
       * Copies the contents of \a other into this picture.
       */
      Picture& operator=(const Picture& other);

      /*!
       * Returns true if Picture stores valid picture
       */
      bool isValid() const;

      /*!
       * Returns the mime type of the image. This should in most cases be
       * "image/png" or "image/jpeg".
       * \see setMimeType(const String &)
       * \see picture()
       * \see setPicture(const ByteArray&)
       */
      String mimeType() const;

      /*!
       * Sets the mime type of the image.  This should in most cases be
       * "image/png" or "image/jpeg".
       * \see setMimeType(const String &)
       * \see picture()
       * \see setPicture(const ByteArray&)
       */
      void setMimeType(const String &value);

      /*!
       * Returns the type of the image.
       *
       * \see Type
       * \see setType()
       */
      Type type() const;

      /*!
       * Sets the type for the image.
       *
       * \see Type
       * \see type()
       */
      void setType(const ASF::Picture::Type& t);

      /*!
       * Returns a text description of the image.
       *
       * \see setDescription()
       */
      String description() const;

      /*!
       * Sets a textual description of the image to \a desc.
       *
       * \see description()
       */
      void setDescription(const String &desc);

      /*!
       * Returns the image data as a ByteVector.
       *
       * \note ByteVector has a data() method that returns a const char * which
       * should make it easy to export this data to external programs.
       *
       * \see setPicture()
       * \see mimeType()
       */
      ByteVector picture() const;

      /*!
       * Sets the image data to \a p.  \a p should be of the type specified in
       * this frame's mime-type specification.
       *
       * \see picture()
       * \see mimeType()
       * \see setMimeType()
       */
      void setPicture(const ByteVector &p);

      /*!
       * Returns picture as binary raw data \a value
       */
      ByteVector render() const;

      /*!
       * Returns picture as binary raw data \a value
       */
      int dataSize() const;

#ifndef DO_NOT_DOCUMENT
      /* THIS IS PRIVATE, DON'T TOUCH IT! */
      void parse(const ByteVector& );
      static Picture fromInvalid();
      friend class Attribute;
#endif
      private:
        class PicturePrivate;
        PicturePrivate *d;
      };
  }
}

#endif // ASFPICTURE_H
