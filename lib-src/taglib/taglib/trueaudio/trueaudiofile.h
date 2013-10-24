/***************************************************************************
    copyright            : (C) 2006 by Lukáš Lalinský
    email                : lalinsky@gmail.com

    copyright            : (C) 2004 by Allan Sandfeld Jensen
    email                : kde@carewolf.org
                           (original MPC implementation)
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

#ifndef TAGLIB_TRUEAUDIOFILE_H
#define TAGLIB_TRUEAUDIOFILE_H

#include "tfile.h"
#include "trueaudioproperties.h"

namespace TagLib {

  class Tag;

  namespace ID3v2 { class Tag; class FrameFactory; }
  namespace ID3v1 { class Tag; }

  //! An implementation of TrueAudio metadata

  /*!
   * This is implementation of TrueAudio metadata.
   *
   * This supports ID3v1 and ID3v2 tags as well as reading stream
   * properties from the file.
   */

  namespace TrueAudio {

    //! An implementation of TagLib::File with TrueAudio specific methods

    /*!
     * This implements and provides an interface for TrueAudio files to the
     * TagLib::Tag and TagLib::AudioProperties interfaces by way of implementing
     * the abstract TagLib::File API as well as providing some additional
     * information specific to TrueAudio files.
     */

    class TAGLIB_EXPORT File : public TagLib::File
    {
    public:
      /*!
       * This set of flags is used for various operations and is suitable for
       * being OR-ed together.
       */
      enum TagTypes {
        //! Empty set.  Matches no tag types.
        NoTags  = 0x0000,
        //! Matches ID3v1 tags.
        ID3v1   = 0x0001,
        //! Matches ID3v2 tags.
        ID3v2   = 0x0002,
        //! Matches all tag types.
        AllTags = 0xffff
      };

      /*!
       * Constructs a TrueAudio file from \a file.  If \a readProperties is true 
       * the file's audio properties will also be read.
       *
       * \note In the current implementation, \a propertiesStyle is ignored.
       */
      File(FileName file, bool readProperties = true,
           Properties::ReadStyle propertiesStyle = Properties::Average);

      /*!
       * Constructs a TrueAudio file from \a file.  If \a readProperties is true 
       * the file's audio properties will also be read.
       *
       * If this file contains and ID3v2 tag the frames will be created using
       * \a frameFactory.
       *
       * \note In the current implementation, \a propertiesStyle is ignored.
       */
      File(FileName file, ID3v2::FrameFactory *frameFactory,
           bool readProperties = true,
           Properties::ReadStyle propertiesStyle = Properties::Average);

      /*!
       * Constructs a TrueAudio file from \a stream.  If \a readProperties is true
       * the file's audio properties will also be read.
       *
       * \note TagLib will *not* take ownership of the stream, the caller is
       * responsible for deleting it after the File object.
       *
       * \note In the current implementation, \a propertiesStyle is ignored.
       */
      File(IOStream *stream, bool readProperties = true,
           Properties::ReadStyle propertiesStyle = Properties::Average);

      /*!
       * Constructs a TrueAudio file from \a stream.  If \a readProperties is true 
       * the file's audio properties will also be read.
       *
       * \note TagLib will *not* take ownership of the stream, the caller is
       * responsible for deleting it after the File object.
       *
       * If this file contains and ID3v2 tag the frames will be created using
       * \a frameFactory.
       *
       * \note In the current implementation, \a propertiesStyle is ignored.
       */
      File(IOStream *stream, ID3v2::FrameFactory *frameFactory,
           bool readProperties = true,
           Properties::ReadStyle propertiesStyle = Properties::Average);

      /*!
       * Destroys this instance of the File.
       */
      virtual ~File();

      /*!
       * Returns the Tag for this file.
       */
      virtual TagLib::Tag *tag() const;

      /*!
       * Implements the unified property interface -- export function.
       * If the file contains both ID3v1 and v2 tags, only ID3v2 will be
       * converted to the PropertyMap.
       */
      PropertyMap properties() const;

      /*!
       * Implements the unified property interface -- import function.
       * Creates in ID3v2 tag if necessary. If an ID3v1 tag exists, it will
       * be updated as well, within the limitations of ID3v1.
       */
      PropertyMap setProperties(const PropertyMap &);

      void removeUnsupportedProperties(const StringList &properties);

      /*!
       * Returns the TrueAudio::Properties for this file.  If no audio properties
       * were read then this will return a null pointer.
       */
      virtual Properties *audioProperties() const;

      /*!
       * Set the ID3v2::FrameFactory to something other than the default.
       *
       * \see ID3v2FrameFactory
       */
      void setID3v2FrameFactory(const ID3v2::FrameFactory *factory);

      /*!
       * Saves the file.
       */
      virtual bool save();

      /*!
       * Returns a pointer to the ID3v1 tag of the file.
       *
       * If \a create is false (the default) this may return a null pointer
       * if there is no valid ID3v1 tag.  If \a create is true it will create
       * an ID3v1 tag if one does not exist and returns a valid pointer.
       *
       * \note This may return a valid pointer regardless of whether or not the 
       * file on disk has an ID3v1 tag.  Use hasID3v1Tag() to check if the file 
       * on disk actually has an ID3v1 tag.
       *
       * \note The Tag <b>is still</b> owned by the MPEG::File and should not be
       * deleted by the user.  It will be deleted when the file (object) is
       * destroyed.
       *
       * \see hasID3v1Tag()
       */
      ID3v1::Tag *ID3v1Tag(bool create = false);

      /*!
       * Returns a pointer to the ID3v2 tag of the file.
       *
       * If \a create is false (the default) this may return a null pointer
       * if there is no valid ID3v2 tag.  If \a create is true it will create
       * an ID3v2 tag if one does not exist and returns a valid pointer.
       *
       * \note This may return a valid pointer regardless of whether or not the 
       * file on disk has an ID3v2 tag.  Use hasID3v2Tag() to check if the file 
       * on disk actually has an ID3v2 tag.
       *
       * \note The Tag <b>is still</b> owned by the MPEG::File and should not be
       * deleted by the user.  It will be deleted when the file (object) is
       * destroyed.
       *
       * \see hasID3v2Tag()
       */
      ID3v2::Tag *ID3v2Tag(bool create = false);

      /*!
       * This will remove the tags that match the OR-ed together TagTypes from the
       * file.  By default it removes all tags.
       *
       * \note This will also invalidate pointers to the tags
       * as their memory will be freed.
       * \note In order to make the removal permanent save() still needs to be called
       */
      void strip(int tags = AllTags);
      
      /*!
       * Returns whether or not the file on disk actually has an ID3v1 tag.
       *
       * \see ID3v1Tag()
       */
      bool hasID3v1Tag() const;

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
      void scan();
      long findID3v1();
      long findID3v2();

      class FilePrivate;
      FilePrivate *d;
    };
  }
}

#endif
