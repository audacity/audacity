/***************************************************************************
    copyright            : (C) 2004 by Allan Sandfeld Jensen
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

#ifndef TAGLIB_OGGFLACFILE_H
#define TAGLIB_OGGFLACFILE_H

#include "taglib_export.h"
#include "oggfile.h"
#include "xiphcomment.h"

#include "flacproperties.h"

namespace TagLib {

  class Tag;

  namespace Ogg {

  //! An implementation of Ogg FLAC metadata

  /*!
   * This is implementation of FLAC metadata for Ogg FLAC files.  For "pure"
   * FLAC files look under the FLAC hiearchy.
   *
   * Unlike "pure" FLAC-files, Ogg FLAC only supports Xiph-comments,
   * while the audio-properties are the same.
   */
  namespace FLAC {

    using TagLib::FLAC::Properties;

    //! An implementation of TagLib::File with Ogg/FLAC specific methods

    /*!
     * This implements and provides an interface for Ogg/FLAC files to the
     * TagLib::Tag and TagLib::AudioProperties interfaces by way of implementing
     * the abstract TagLib::File API as well as providing some additional
     * information specific to Ogg FLAC files.
     */

    class TAGLIB_EXPORT File : public Ogg::File
    {
    public:
      /*!
       * Constructs an Ogg/FLAC file from \a file.  If \a readProperties is true 
       * the file's audio properties will also be read.
       *
       * \note In the current implementation, \a propertiesStyle is ignored.
       */
      File(FileName file, bool readProperties = true,
           Properties::ReadStyle propertiesStyle = Properties::Average);

      /*!
       * Constructs an Ogg/FLAC file from \a stream.  If \a readProperties is true 
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
       * Destroys this instance of the File.
       */
      virtual ~File();

      /*!
       * Returns the Tag for this file.  This will always be a XiphComment.
       *
       * \note This always returns a valid pointer regardless of whether or not 
       * the file on disk has a XiphComment.  Use hasXiphComment() to check if 
       * the file on disk actually has a XiphComment.
       * 
       * \note The Tag <b>is still</b> owned by the FLAC::File and should not be
       * deleted by the user.  It will be deleted when the file (object) is
       * destroyed.
       *
       * \see hasXiphComment()
       */
      virtual XiphComment *tag() const;

      /*!
       * Returns the FLAC::Properties for this file.  If no audio properties
       * were read then this will return a null pointer.
       */
      virtual Properties *audioProperties() const;


      /*! 
       * Implements the unified property interface -- export function.
       * This forwards directly to XiphComment::properties().
       */
      PropertyMap properties() const;

      /*! 
       * Implements the unified tag dictionary interface -- import function.
       * Like properties(), this is a forwarder to the file's XiphComment.
       */
      PropertyMap setProperties(const PropertyMap &); 


      /*!
       * Save the file.  This will primarily save and update the XiphComment.
       * Returns true if the save is successful.
       */
      virtual bool save();

      /*!
       * Returns the length of the audio-stream, used by FLAC::Properties for
       * calculating the bitrate.
       */
      long streamLength();

      /*!
       * Returns whether or not the file on disk actually has a XiphComment.
       *
       * \see tag()
       */
      bool hasXiphComment() const;

    private:
      File(const File &);
      File &operator=(const File &);

      void read(bool readProperties, Properties::ReadStyle propertiesStyle);
      void scan();
      ByteVector streamInfoData();
      ByteVector xiphCommentData();

      class FilePrivate;
      FilePrivate *d;
    };
  } // namespace FLAC
  } // namespace Ogg
} // namespace TagLib

#endif
