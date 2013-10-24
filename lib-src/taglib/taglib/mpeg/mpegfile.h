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

#ifndef TAGLIB_MPEGFILE_H
#define TAGLIB_MPEGFILE_H

#include "taglib_export.h"
#include "tfile.h"
#include "tag.h"

#include "mpegproperties.h"

namespace TagLib {

  namespace ID3v2 { class Tag; class FrameFactory; }
  namespace ID3v1 { class Tag; }
  namespace APE { class Tag; }

  //! An implementation of TagLib::File with MPEG (MP3) specific methods

  namespace MPEG {

    //! An MPEG file class with some useful methods specific to MPEG

    /*!
     * This implements the generic TagLib::File API and additionally provides
     * access to properties that are distinct to MPEG files, notably access
     * to the different ID3 tags.
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
        //! Matches APE tags.
        APE     = 0x0004,
        //! Matches all tag types.
        AllTags = 0xffff
      };

      /*!
       * Constructs an MPEG file from \a file.  If \a readProperties is true the
       * file's audio properties will also be read.
       *
       * \note In the current implementation, \a propertiesStyle is ignored.
       *
       * \deprecated This constructor will be dropped in favor of the one below
       * in a future version.
       */
      File(FileName file, bool readProperties = true,
           Properties::ReadStyle propertiesStyle = Properties::Average);

      /*!
       * Constructs an MPEG file from \a file.  If \a readProperties is true the
       * file's audio properties will also be read.
       *
       * If this file contains and ID3v2 tag the frames will be created using
       * \a frameFactory.
       *
       * \note In the current implementation, \a propertiesStyle is ignored.
       */
      // BIC: merge with the above constructor
      File(FileName file, ID3v2::FrameFactory *frameFactory,
           bool readProperties = true,
           Properties::ReadStyle propertiesStyle = Properties::Average);

      /*!
       * Constructs an MPEG file from \a stream.  If \a readProperties is true the
       * file's audio properties will also be read.
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
       * Returns a pointer to a tag that is the union of the ID3v2 and ID3v1
       * tags. The ID3v2 tag is given priority in reading the information -- if
       * requested information exists in both the ID3v2 tag and the ID3v1 tag,
       * the information from the ID3v2 tag will be returned.
       *
       * If you would like more granular control over the content of the tags,
       * with the concession of generality, use the tag-type specific calls.
       *
       * \note As this tag is not implemented as an ID3v2 tag or an ID3v1 tag,
       * but a union of the two this pointer may not be cast to the specific
       * tag types.
       *
       * \see ID3v1Tag()
       * \see ID3v2Tag()
       * \see APETag()
       */
      virtual Tag *tag() const;

      /*!
       * Implements the reading part of the unified property interface.
       * If the file contains more than one tag, only the
       * first one (in the order ID3v2, APE, ID3v1) will be converted to the
       * PropertyMap.
       */
      PropertyMap properties() const;

      void removeUnsupportedProperties(const StringList &properties);

      /*!
       * Implements the writing part of the unified tag dictionary interface.
       * In order to avoid problems with deprecated tag formats, this method
       * always creates an ID3v2 tag if necessary.
       * If an ID3v1 tag  exists, it will be updated as well, within the
       * limitations of that format.
       * The returned PropertyMap refers to the ID3v2 tag only.
       */
      PropertyMap setProperties(const PropertyMap &);

      /*!
       * Returns the MPEG::Properties for this file.  If no audio properties
       * were read then this will return a null pointer.
       */
      virtual Properties *audioProperties() const;

      /*!
       * Save the file.  If at least one tag -- ID3v1 or ID3v2 -- exists this
       * will duplicate its content into the other tag.  This returns true
       * if saving was successful.
       *
       * If neither exists or if both tags are empty, this will strip the tags
       * from the file.
       *
       * This is the same as calling save(AllTags);
       *
       * If you would like more granular control over the content of the tags,
       * with the concession of generality, use paramaterized save call below.
       *
       * \see save(int tags)
       */
      virtual bool save();

      /*!
       * Save the file.  This will attempt to save all of the tag types that are
       * specified by OR-ing together TagTypes values.  The save() method above
       * uses AllTags.  This returns true if saving was successful.
       *
       * This strips all tags not included in the mask, but does not modify them
       * in memory, so later calls to save() which make use of these tags will
       * remain valid.  This also strips empty tags.
       */
      bool save(int tags);

      /*!
       * Save the file.  This will attempt to save all of the tag types that are
       * specified by OR-ing together TagTypes values.  The save() method above
       * uses AllTags.  This returns true if saving was successful.
       *
       * If \a stripOthers is true this strips all tags not included in the mask,
       * but does not modify them in memory, so later calls to save() which make
       * use of these tags will remain valid.  This also strips empty tags.
       */
      // BIC: combine with the above method
      bool save(int tags, bool stripOthers);

      /*!
       * Save the file.  This will attempt to save all of the tag types that are
       * specified by OR-ing together TagTypes values.  The save() method above
       * uses AllTags.  This returns true if saving was successful.
       *
       * If \a stripOthers is true this strips all tags not included in the mask,
       * but does not modify them in memory, so later calls to save() which make
       * use of these tags will remain valid.  This also strips empty tags.
       *
       * The \a id3v2Version parameter specifies the version of the saved
       * ID3v2 tag. It can be either 4 or 3.
       */
      // BIC: combine with the above method
      bool save(int tags, bool stripOthers, int id3v2Version);

      /*!
       * Save the file.  This will attempt to save all of the tag types that are
       * specified by OR-ing together TagTypes values.  The save() method above
       * uses AllTags.  This returns true if saving was successful.
       *
       * If \a stripOthers is true this strips all tags not included in the mask,
       * but does not modify them in memory, so later calls to save() which make
       * use of these tags will remain valid.  This also strips empty tags.
       *
       * The \a id3v2Version parameter specifies the version of the saved
       * ID3v2 tag. It can be either 4 or 3.
       *
       * If \a duplicateTags is true and at least one tag -- ID3v1 or ID3v2 --
       * exists this will duplicate its content into the other tag.
       */
      // BIC: combine with the above method
      bool save(int tags, bool stripOthers, int id3v2Version, bool duplicateTags);

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
       * Returns a pointer to the APE tag of the file.
       *
       * If \a create is false (the default) this may return a null pointer
       * if there is no valid APE tag.  If \a create is true it will create
       * an APE tag if one does not exist and returns a valid pointer.
       *
       * \note This may return a valid pointer regardless of whether or not the 
       * file on disk has an APE tag.  Use hasAPETag() to check if the file 
       * on disk actually has an APE tag.
       *
       * \note The Tag <b>is still</b> owned by the MPEG::File and should not be
       * deleted by the user.  It will be deleted when the file (object) is
       * destroyed.
       *
       * \see hasAPETag()
       */
      APE::Tag *APETag(bool create = false);

      /*!
       * This will strip the tags that match the OR-ed together TagTypes from the
       * file.  By default it strips all tags.  It returns true if the tags are
       * successfully stripped.
       *
       * This is equivalent to strip(tags, true)
       *
       * \note This will also invalidate pointers to the ID3 and APE tags
       * as their memory will be freed.
       */
      bool strip(int tags = AllTags);

      /*!
       * This will strip the tags that match the OR-ed together TagTypes from the
       * file.  By default it strips all tags.  It returns true if the tags are
       * successfully stripped.
       *
       * If \a freeMemory is true the ID3 and APE tags will be deleted and
       * pointers to them will be invalidated.
       */
      // BIC: merge with the method above
      bool strip(int tags, bool freeMemory);

      /*!
       * Set the ID3v2::FrameFactory to something other than the default.
       *
       * \see ID3v2FrameFactory
       */
      void setID3v2FrameFactory(const ID3v2::FrameFactory *factory);

      /*!
       * Returns the position in the file of the first MPEG frame.
       */
      long firstFrameOffset();

      /*!
       * Returns the position in the file of the next MPEG frame,
       * using the current position as start
       */
      long nextFrameOffset(long position);

      /*!
       * Returns the position in the file of the previous MPEG frame,
       * using the current position as start
       */
      long previousFrameOffset(long position);

      /*!
       * Returns the position in the file of the last MPEG frame.
       */
      long lastFrameOffset();

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

      /*!
       * Returns whether or not the file on disk actually has an APE tag.
       *
       * \see APETag()
       */
      bool hasAPETag() const;

    private:
      File(const File &);
      File &operator=(const File &);

      void read(bool readProperties, Properties::ReadStyle propertiesStyle);
      long findID3v2();
      long findID3v1();
      void findAPE();

      /*!
       * MPEG frames can be recognized by the bit pattern 11111111 111, so the
       * first byte is easy to check for, however checking to see if the second byte
       * starts with \e 111 is a bit more tricky, hence this member function.
       */
      static bool secondSynchByte(char byte);

      class FilePrivate;
      FilePrivate *d;
    };
  }
}

#endif
