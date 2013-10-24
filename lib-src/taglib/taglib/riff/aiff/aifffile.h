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

#ifndef TAGLIB_AIFFFILE_H
#define TAGLIB_AIFFFILE_H

#include "rifffile.h"
#include "id3v2tag.h"
#include "aiffproperties.h"

namespace TagLib {

  namespace RIFF {

    //! An implementation of AIFF metadata

    /*!
     * This is implementation of AIFF metadata.
     *
     * This supports an ID3v2 tag as well as reading stream from the ID3 RIFF
     * chunk as well as properties from the file.
     */

    namespace AIFF {

      //! An implementation of TagLib::File with AIFF specific methods

      /*!
       * This implements and provides an interface for AIFF files to the
       * TagLib::Tag and TagLib::AudioProperties interfaces by way of implementing
       * the abstract TagLib::File API as well as providing some additional
       * information specific to AIFF files.
       */

      class TAGLIB_EXPORT File : public TagLib::RIFF::File
      {
      public:
        /*!
         * Constructs an AIFF file from \a file.  If \a readProperties is true the
         * file's audio properties will also be read.
         *
         * \note In the current implementation, \a propertiesStyle is ignored.
         */
        File(FileName file, bool readProperties = true,
             Properties::ReadStyle propertiesStyle = Properties::Average);

        /*!
         * Constructs an AIFF file from \a stream.  If \a readProperties is true the
         * file's audio properties will also be read.
         *
         * \note TagLib will *not* take ownership of the stream, the caller is
         * responsible for deleting it after the File object.
         *
         * \note In the current implementation, \a propertiesStyle is ignored.
         */
        File(IOStream *stream, bool readProperties = true,
             Properties::ReadStyle propertiesStyle = Properties::Average);

        /*!
         * Destroys this instance of the File.
         */
        virtual ~File();

        /*!
         * Returns the Tag for this file.
         *
         * \note This always returns a valid pointer regardless of whether or not 
         * the file on disk has an ID3v2 tag.  Use hasID3v2Tag() to check if the file 
         * on disk actually has an ID3v2 tag.
         *
         * \see hasID3v2Tag()
         */
        virtual ID3v2::Tag *tag() const;

        /*!
         * Implements the unified property interface -- export function.
         * This method forwards to ID3v2::Tag::properties().
         */
        PropertyMap properties() const;

        void removeUnsupportedProperties(const StringList &properties);

        /*!
         * Implements the unified property interface -- import function.
         * This method forwards to ID3v2::Tag::setProperties().
         */
        PropertyMap setProperties(const PropertyMap &);

        /*!
         * Returns the AIFF::Properties for this file.  If no audio properties
         * were read then this will return a null pointer.
         */
        virtual Properties *audioProperties() const;

        /*!
         * Saves the file.
         */
        virtual bool save();

        /*!
         * Returns whether or not the file on disk actually has an ID3v2 tag.
         *
         * \see ID3v2Tag()
         */
        bool hasID3v2Tag() const;

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
