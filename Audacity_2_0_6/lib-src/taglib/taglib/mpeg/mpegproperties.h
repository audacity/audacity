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

#ifndef TAGLIB_MPEGPROPERTIES_H
#define TAGLIB_MPEGPROPERTIES_H

#include "taglib_export.h"
#include "audioproperties.h"

#include "mpegheader.h"

namespace TagLib {

  namespace MPEG {

    class File;
    class XingHeader;

    //! An implementation of audio property reading for MP3

    /*!
     * This reads the data from an MPEG Layer III stream found in the
     * AudioProperties API.
     */

    class TAGLIB_EXPORT Properties : public AudioProperties
    {
    public:
      /*!
       * Create an instance of MPEG::Properties with the data read from the
       * MPEG::File \a file.
       */
      Properties(File *file, ReadStyle style = Average);

      /*!
       * Destroys this MPEG Properties instance.
       */
      virtual ~Properties();

      // Reimplementations.

      virtual int length() const;
      virtual int bitrate() const;
      virtual int sampleRate() const;
      virtual int channels() const;

      /*!
       * Returns a pointer to the XingHeader if one exists or null if no
       * XingHeader was found.
       */

      const XingHeader *xingHeader() const;

      /*!
       * Returns the MPEG Version of the file.
       */
      Header::Version version() const;

      /*!
       * Returns the layer version.  This will be between the values 1-3.
       */
      int layer() const;

      /*!
       * Returns true if the MPEG protection bit is enabled.
       */
      bool protectionEnabled() const;

      /*!
       * Returns the channel mode for this frame.
       */
      Header::ChannelMode channelMode() const;

      /*!
       * Returns true if the copyrighted bit is set.
       */
      bool isCopyrighted() const;

      /*!
       * Returns true if the "original" bit is set.
       */
      bool isOriginal() const;

    private:
      Properties(const Properties &);
      Properties &operator=(const Properties &);

      void read();

      class PropertiesPrivate;
      PropertiesPrivate *d;
    };
  }
}

#endif
