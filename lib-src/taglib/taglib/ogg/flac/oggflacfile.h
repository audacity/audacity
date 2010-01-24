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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
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
       * Contructs an Ogg/FLAC file from \a file.  If \a readProperties is true
       * the file's audio properties will also be read using \a propertiesStyle.
       * If false, \a propertiesStyle is ignored.
       */
      File(FileName file, bool readProperties = true,
           Properties::ReadStyle propertiesStyle = Properties::Average);

      /*!
       * Destroys this instance of the File.
       */
      virtual ~File();

      /*!
       * Returns the Tag for this file.  This will always be a XiphComment.
       */
      virtual XiphComment *tag() const;

      /*!
       * Returns the FLAC::Properties for this file.  If no audio properties
       * were read then this will return a null pointer.
       */
      virtual Properties *audioProperties() const;

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
