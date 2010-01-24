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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_WAVFILE_H
#define TAGLIB_WAVFILE_H

#include "rifffile.h"
#include "id3v2tag.h"
#include "wavproperties.h"

namespace TagLib {

  namespace RIFF {

    //! An implementation of WAV metadata

    /*!
     * This is implementation of WAV metadata.
     *
     * This supports an ID3v2 tag as well as reading stream from the ID3 RIFF
     * chunk as well as properties from the file.
     */

    namespace WAV {

      //! An implementation of TagLib::File with WAV specific methods

      /*!
       * This implements and provides an interface for WAV files to the
       * TagLib::Tag and TagLib::AudioProperties interfaces by way of implementing
       * the abstract TagLib::File API as well as providing some additional
       * information specific to WAV files.
       */

      class TAGLIB_EXPORT File : public TagLib::RIFF::File
      {
      public:
        /*!
         * Contructs an WAV file from \a file.  If \a readProperties is true the
         * file's audio properties will also be read using \a propertiesStyle.  If
         * false, \a propertiesStyle is ignored.
         */
        File(FileName file, bool readProperties = true,
             Properties::ReadStyle propertiesStyle = Properties::Average);

        /*!
         * Destroys this instance of the File.
         */
        virtual ~File();

        /*!
         * Returns the Tag for this file.
         */
        virtual ID3v2::Tag *tag() const;

        /*!
         * Returns the WAV::Properties for this file.  If no audio properties
         * were read then this will return a null pointer.
         */
        virtual Properties *audioProperties() const;

        /*!
         * Saves the file.
         */
        virtual bool save();

      private:
        File(const File &);
        File &operator=(const File &);

        void read(bool readProperties, Properties::ReadStyle propertiesStyle);

        class FilePrivate;
        FilePrivate *d;
      };
    }
  }
}

#endif
