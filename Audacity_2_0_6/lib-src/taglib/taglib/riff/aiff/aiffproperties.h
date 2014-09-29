/***************************************************************************
    copyright            : (C) 2008 by Scott Wheeler
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

#ifndef TAGLIB_AIFFPROPERTIES_H
#define TAGLIB_AIFFPROPERTIES_H

#include "audioproperties.h"

namespace TagLib {

  namespace RIFF {

    namespace AIFF {

      class File;

      //! An implementation of audio property reading for AIFF

      /*!
       * This reads the data from an AIFF stream found in the AudioProperties
       * API.
       */

      class TAGLIB_EXPORT Properties : public AudioProperties
      {
      public:
        /*!
         * Create an instance of AIFF::Properties with the data read from the
         * ByteVector \a data.
         */
        Properties(const ByteVector &data, ReadStyle style);

        /*!
         * Destroys this AIFF::Properties instance.
         */
        virtual ~Properties();

        // Reimplementations.

        virtual int length() const;
        virtual int bitrate() const;
        virtual int sampleRate() const;
        virtual int channels() const;

        int sampleWidth() const;
        uint sampleFrames() const;

      private:
        Properties(const Properties &);
        Properties &operator=(const Properties &);

        void read(const ByteVector &data);

        class PropertiesPrivate;
        PropertiesPrivate *d;
      };
    }
  }
}

#endif
