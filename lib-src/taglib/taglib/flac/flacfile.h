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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_FLACFILE_H
#define TAGLIB_FLACFILE_H

#include "taglib_export.h"
#include "tfile.h"

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
       * Contructs a FLAC file from \a file.  If \a readProperties is true the
       * file's audio properties will also be read using \a propertiesStyle.  If
       * false, \a propertiesStyle is ignored.
       *
       * \deprecated This constructor will be dropped in favor of the one below
       * in a future version.
       */
      File(FileName file, bool readProperties = true,
           Properties::ReadStyle propertiesStyle = Properties::Average);

      /*!
       * Contructs a FLAC file from \a file.  If \a readProperties is true the
       * file's audio properties will also be read using \a propertiesStyle.  If
       * false, \a propertiesStyle is ignored.
       *
       * If this file contains and ID3v2 tag the frames will be created using
       * \a frameFactory.
       */
      // BIC: merge with the above constructor
      File(FileName file, ID3v2::FrameFactory *frameFactory,
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
       * If \a create is false (the default) this will return a null pointer
       * if there is no valid ID3v2 tag.  If \a create is true it will create
       * an ID3v2 tag if one does not exist.
       *
       * \note The Tag <b>is still</b> owned by the FLAC::File and should not be
       * deleted by the user.  It will be deleted when the file (object) is
       * destroyed.
       */
      ID3v2::Tag *ID3v2Tag(bool create = false);

      /*!
       * Returns a pointer to the ID3v1 tag of the file.
       *
       * If \a create is false (the default) this will return a null pointer
       * if there is no valid ID3v1 tag.  If \a create is true it will create
       * an ID3v1 tag if one does not exist.
       *
       * \note The Tag <b>is still</b> owned by the FLAC::File and should not be
       * deleted by the user.  It will be deleted when the file (object) is
       * destroyed.
       */
      ID3v1::Tag *ID3v1Tag(bool create = false);

      /*!
       * Returns a pointer to the XiphComment for the file.
       *
       * If \a create is false (the default) this will return a null pointer
       * if there is no valid XiphComment.  If \a create is true it will create
       * a XiphComment if one does not exist.
       *
       * \note The Tag <b>is still</b> owned by the FLAC::File and should not be
       * deleted by the user.  It will be deleted when the file (object) is
       * destroyed.
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

    private:
      File(const File &);
      File &operator=(const File &);

      void read(bool readProperties, Properties::ReadStyle propertiesStyle);
      void scan();
      long findID3v2();
      long findID3v1();
      ByteVector xiphCommentData() const;

      class FilePrivate;
      FilePrivate *d;
    };
  }
}

#endif
