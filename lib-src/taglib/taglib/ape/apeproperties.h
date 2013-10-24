/***************************************************************************
    copyright            : (C) 2010 by Alex Novichkov
    email                : novichko@atnet.ru

    copyright            : (C) 2006 by Lukáš Lalinský
    email                : lalinsky@gmail.com
                           (original WavPack implementation)
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

#ifndef TAGLIB_APEPROPERTIES_H
#define TAGLIB_APEPROPERTIES_H

#include "taglib_export.h"
#include "audioproperties.h"

namespace TagLib {

  namespace APE {

    class File;

    //! An implementation of audio property reading for APE

    /*!
     * This reads the data from an APE stream found in the AudioProperties
     * API.
     */

    class TAGLIB_EXPORT Properties : public AudioProperties
    {
    public:
      /*!
       * Create an instance of APE::Properties with the data read from the
       * ByteVector \a data.
       */
      Properties(File *f, ReadStyle style = Average);

      /*!
       * Destroys this APE::Properties instance.
       */
      virtual ~Properties();

      // Reimplementations.

      virtual int length() const;
      virtual int bitrate() const;
      virtual int sampleRate() const;
      virtual int channels() const;

      /*!
       * Returns number of bits per sample.
       */
      int bitsPerSample() const;
      uint sampleFrames() const;

      /*!
       * Returns APE version.
       */
      int version() const;

    private:
      Properties(const Properties &);
      Properties &operator=(const Properties &);

      void read();

      long findDescriptor();
      long findID3v2();

      void analyzeCurrent();
      void analyzeOld();

      class PropertiesPrivate;
      PropertiesPrivate *d;
    };
  }
}

#endif
