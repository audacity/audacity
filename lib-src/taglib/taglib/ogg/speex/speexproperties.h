/***************************************************************************
    copyright            : (C) 2006 by Lukáš Lalinský
    email                : lalinsky@gmail.com

    copyright            : (C) 2002 - 2008 by Scott Wheeler
    email                : wheeler@kde.org
                           (original Vorbis implementation)
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

#ifndef TAGLIB_SPEEXPROPERTIES_H
#define TAGLIB_SPEEXPROPERTIES_H

#include "audioproperties.h"

namespace TagLib {

  namespace Ogg {

    namespace Speex {

      class File;

      //! An implementation of audio property reading for Ogg Speex

      /*!
       * This reads the data from an Ogg Speex stream found in the AudioProperties
       * API.
       */

      class TAGLIB_EXPORT Properties : public AudioProperties
      {
      public:
        /*!
         * Create an instance of Speex::Properties with the data read from the
         * Speex::File \a file.
         */
        Properties(File *file, ReadStyle style = Average);

        /*!
         * Destroys this Speex::Properties instance.
         */
        virtual ~Properties();

        // Reimplementations.

        virtual int length() const;
        virtual int bitrate() const;
        virtual int sampleRate() const;
        virtual int channels() const;

        /*!
         * Returns the Speex version, currently "0" (as specified by the spec).
         */
        int speexVersion() const;

      private:
        Properties(const Properties &);
        Properties &operator=(const Properties &);

        void read();

        class PropertiesPrivate;
        PropertiesPrivate *d;
      };
    }
  }
}

#endif
