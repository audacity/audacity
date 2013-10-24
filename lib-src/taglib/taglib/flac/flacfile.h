/***************************************************************************
    copyright            : (C) 2003 by Allan Sandfeld Jensen
    email                : kde@carewolf.org
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

#ifndef TAGLIB_FLACFILE_H
#define TAGLIB_FLACFILE_H

#include "taglib_export.h"
#include "tfile.h"
#include "tlist.h"
#include "tag.h"

#include "flacpicture.h"
#include "flacproperties.h"

namespace TagLib {

  class Tag;
  namespace ID3v2 { class FrameFactory; class Tag; }
  namespace ID3v1 { class Tag; }
  namespace Ogg { class XiphComment; }

  //! An implementation of FLAC metadata

  /*!
   * This is implementation of FLAC metadata for non-Ogg FLAC files.  At some
   * point when Ogg / FLAC is more common there will be a similar implementation
   * under the Ogg hiearchy.
   *
   * This supports ID3v1, ID3v2 and Xiph style comments as well as reading stream
   * properties from the file.
   */

  namespace FLAC {

    //! An implementation of TagLib::File with FLAC specific methods

    /*!
     * This implements and provides an interface for FLAC files to the
     * TagLib::Tag and TagLib::AudioProperties interfaces by way of implementing
     * the abstract TagLib::File API as well as providing some additional
     * information specific to FLAC files.
     */

    class TAGLIB_EXPORT File : public TagLib::File
    {
    public:
      /*!
       * Constructs a FLAC file from \a file.  If \a readProperties is true the
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
       * Constructs an APE file from \a file.  If \a readProperties is true the
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
       * Constructs a FLAC file from \a stream.  If \a readProperties is true the
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
      // BIC: merge with the above constructor
      File(IOStream *stream, ID3v2::FrameFactory *frameFactory,
           bool readProperties = true,
           Properties::ReadStyle propertiesStyle = Properties::Average);

      /*!
       * Destroys this instance of the File.
       */
      virtual ~File();

      /*!
       * Returns the Tag for this file.  This will be a union of XiphComment,
       * ID3v1 and ID3v2 tags.
       *
       * \see ID3v2Tag()
       * \see ID3v1Tag()
       * \see XiphComment()
       */
      virtual TagLib::Tag *tag() const;

      /*!
       * Implements the unified property interface -- export function.
       * If the file contains more than one tag (e.g. XiphComment and ID3v1),
       * only the first one (in the order XiphComment, ID3v2, ID3v1) will be
       * converted to the PropertyMap.
       */
      PropertyMap properties() const;

      void removeUnsupportedProperties(const StringList &);

      /*!
       * Implements the unified property interface -- import function.
       * This always creates a Xiph comment, if none exists. The return value
       * relates to the Xiph comment only.
       * Ignores any changes to ID3v1 or ID3v2 comments since they are not allowed
       * in the FLAC specification.
       */
      PropertyMap setProperties(const PropertyMap &);

      /*!
       * Returns the FLAC::Properties for this file.  If no audio properties
       * were read then this will return a null pointer.
       */
      virtual Properties *audioProperties() const;

      /*!
       * Save the file.  This will primarily save the XiphComment, but
       * will also keep any old ID3-tags up to date. If the file
       * has no XiphComment, one will be constructed from the ID3-tags.
       *
       * This returns true if the save was successful.
       */
      virtual bool save();

      /*!
       * Returns a pointer to the ID3v2 tag of the file.
       *
       * If \a create is false (the default) this returns a null pointer
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
       * If \a create is false (the default) this returns a null pointer
       * if there is no valid APE tag.  If \a create is true it will create
       * an APE tag if one does not exist and returns a valid pointer.
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
       * Returns a pointer to the XiphComment for the file.
       *
       * If \a create is false (the default) this returns a null pointer
       * if there is no valid XiphComment.  If \a create is true it will create
       * a XiphComment if one does not exist and returns a valid pointer.
       *
       * \note This may return a valid pointer regardless of whether or not the 
       * file on disk has a XiphComment.  Use hasXiphComment() to check if the 
       * file on disk actually has a XiphComment.
       * 
       * \note The Tag <b>is still</b> owned by the FLAC::File and should not be
       * deleted by the user.  It will be deleted when the file (object) is
       * destroyed.
       *
       * \see hasXiphComment()
       */
      Ogg::XiphComment *xiphComment(bool create = false);

      /*!
       * Set the ID3v2::FrameFactory to something other than the default.  This
       * can be used to specify the way that ID3v2 frames will be interpreted
       * when
       *
       * \see ID3v2FrameFactory
       */
      void setID3v2FrameFactory(const ID3v2::FrameFactory *factory);

      /*!
       * Returns the block of data used by FLAC::Properties for parsing the
       * stream properties.
       *
       * \deprecated This method will not be public in a future release.
       */
      ByteVector streamInfoData(); // BIC: remove

      /*!
       * Returns the length of the audio-stream, used by FLAC::Properties for
       * calculating the bitrate.
       *
       * \deprecated This method will not be public in a future release.
       */
      long streamLength();  // BIC: remove

      /*!
       * Returns a list of pictures attached to the FLAC file.
       */
      List<Picture *> pictureList();

      /*!
       * Removes an attached picture. If \a del is true the picture's memory
       * will be freed; if it is false, it must be deleted by the user.
       */
      void removePicture(Picture *picture, bool del = true);

      /*!
       * Remove all attached images.
       */
      void removePictures();

      /*!
       * Add a new picture to the file. The file takes ownership of the
       * picture and will handle freeing its memory.
       *
       * \note The file will be saved only after calling save().
       */
      void addPicture(Picture *picture);

      /*!
       * Returns whether or not the file on disk actually has a XiphComment.
       *
       * \see xiphComment()
       */
      bool hasXiphComment() const;

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
      long findID3v2();
      long findID3v1();
      ByteVector xiphCommentData() const;
      long findPaddingBreak(long nextPageOffset, long targetOffset, bool *isLast);

      class FilePrivate;
      FilePrivate *d;
    };
  }
}

#endif
