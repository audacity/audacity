/**************************************************************************
    copyright            : (C) 2010 by Lukáš Lalinský
    email                : lalinsky@gmail.com
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

#ifndef TAGLIB_FLACPICTURE_H
#define TAGLIB_FLACPICTURE_H

#include "tlist.h"
#include "tstring.h"
#include "tbytevector.h"
#include "taglib_export.h"
#include "flacmetadatablock.h"

namespace TagLib {

  namespace FLAC {

    class TAGLIB_EXPORT Picture : public MetadataBlock
    {
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

      Picture();
      Picture(const ByteVector &data);
      ~Picture();

      /*!
       * Returns the type of the image.
       */
      Type type() const;

      /*!
       * Sets the type of the image.
       */
      void setType(Type type);

      /*!
       * Returns the mime type of the image.  This should in most cases be
       * "image/png" or "image/jpeg".
       */
      String mimeType() const;

      /*!
       * Sets the mime type of the image.  This should in most cases be
       * "image/png" or "image/jpeg".
       */
      void setMimeType(const String &m);

      /*!
       * Returns a text description of the image.
       */

      String description() const;

      /*!
       * Sets a textual description of the image to \a desc.
       */

      void setDescription(const String &desc);

      /*!
       * Returns the width of the image.
       */
      int width() const;

      /*!
       * Sets the width of the image.
       */
      void setWidth(int w);

      /*!
       * Returns the height of the image.
       */
      int height() const;

      /*!
       * Sets the height of the image.
       */
      void setHeight(int h);

      /*!
       * Returns the color depth (in bits-per-pixel) of the image.
       */
      int colorDepth() const;

      /*!
       * Sets the color depth (in bits-per-pixel) of the image.
       */
      void setColorDepth(int depth);

      /*!
       * Returns the number of colors used on the image..
       */
      int numColors() const;

      /*!
       * Sets the number of colors used on the image (for indexed images).
       */
      void setNumColors(int numColors);

      /*!
       * Returns the image data.
       */
      ByteVector data() const;

      /*!
       * Sets the image data.
       */
      void setData(const ByteVector &data);

      /*!
       * Returns the FLAC metadata block type.
       */
      int code() const;

      /*!
       * Render the content to the FLAC picture block format.
       */
      ByteVector render() const;

      /*!
       * Parse the picture data in the FLAC picture block format.
       */
      bool parse(const ByteVector &rawData);

    private:
      Picture(const Picture &item);
      Picture &operator=(const Picture &item);

      class PicturePrivate;
      PicturePrivate *d;
    };

    typedef List<Picture> PictureList;

  }

}

#endif
