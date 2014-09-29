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

#ifndef TAGLIB_WAVFILE_H
#define TAGLIB_WAVFILE_H

#include "rifffile.h"
#include "id3v2tag.h"
#include "infotag.h"
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
        enum TagTypes {
          //! Empty set.  Matches no tag types.
          NoTags  = 0x0000,
          //! Matches ID3v2 tags.
          ID3v2   = 0x0001,
          //! Matches Info tags.
          Info    = 0x0002,
          //! Matches all tag types.
          AllTags = 0xffff
        };

        /*!
         * Constructs a WAV file from \a file.  If \a readProperties is true the
         * file's audio properties will also be read.
         *
         * \note In the current implementation, \a propertiesStyle is ignored.
         */
        File(FileName file, bool readProperties = true,
             Properties::ReadStyle propertiesStyle = Properties::Average);

        /*!
         * Constructs a WAV file from \a stream.  If \a readProperties is true the
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
         * Returns the ID3v2 Tag for this file.
         * 
         * \note This method does not return all the tags for this file for 
         * backward compatibility.  Will be fixed in TagLib 2.0.
         */
        ID3v2::Tag *tag() const;

        /*!
         * Returns the ID3v2 Tag for this file.
         *
         * \note This always returns a valid pointer regardless of whether or not 
         * the file on disk has an ID3v2 tag.  Use hasID3v2Tag() to check if the 
         * file on disk actually has an ID3v2 tag.
         *
         * \see hasID3v2Tag()
         */
        ID3v2::Tag *ID3v2Tag() const;

        /*!
         * Returns the RIFF INFO Tag for this file.
         *
         * \note This always returns a valid pointer regardless of whether or not 
         * the file on disk has a RIFF INFO tag.  Use hasInfoTag() to check if the 
         * file on disk actually has a RIFF INFO tag.
         *
         * \see hasInfoTag()
         */
        Info::Tag *InfoTag() const;

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
         * Returns the WAV::Properties for this file.  If no audio properties
         * were read then this will return a null pointer.
         */
        virtual Properties *audioProperties() const;

        /*!
         * Saves the file.
         */
        virtual bool save();

        bool save(TagTypes tags, bool stripOthers = true, int id3v2Version = 4);
        
        /*!
         * Returns whether or not the file on disk actually has an ID3v2 tag.
         *
         * \see ID3v2Tag()
         */
        bool hasID3v2Tag() const;

        /*!
         * Returns whether or not the file on disk actually has a RIFF INFO tag.
         *
         * \see InfoTag()
         */
        bool hasInfoTag() const;

      private:
        File(const File &);
        File &operator=(const File &);

        void read(bool readProperties, Properties::ReadStyle propertiesStyle);

        void strip(TagTypes tags);

        /*!
         * Returns the index of the chunk that its name is "LIST" and list type is "INFO".
         */
        uint findInfoTagChunk();

        class FilePrivate;
        FilePrivate *d;
      };
    }
  }
}

#endif
