/**************************************************************************
    copyright            : (C) 2007 by Lukáš Lalinský
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

#ifndef TAGLIB_MP4PROPERTIES_H
#define TAGLIB_MP4PROPERTIES_H

#include "taglib_export.h"
#include "audioproperties.h"

namespace TagLib {

  namespace MP4 {

    class Atoms;
    class File;

    //! An implementation of MP4 audio properties
    class TAGLIB_EXPORT Properties : public AudioProperties
    {
    public:
      enum Codec {
        Unknown = 0,
        AAC,
        ALAC
      };

      Properties(File *file, Atoms *atoms, ReadStyle style = Average);
      virtual ~Properties();

      virtual int length() const;
      virtual int bitrate() const;
      virtual int sampleRate() const;
      virtual int channels() const;
      virtual int bitsPerSample() const;
      bool isEncrypted() const;

      //! Audio codec used in the MP4 file
      Codec codec() const;

    private:
      class PropertiesPrivate;
      PropertiesPrivate *d;
    };

  }

}

#endif
