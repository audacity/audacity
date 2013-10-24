/***************************************************************************
    copyright           : (C) 2011 by Mathias Panzenböck
    email               : grosser.meister.morti@gmx.net
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it  under the terms of the GNU Lesser General Public License version  *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *
 *   MA  02110-1301  USA                                                   *
 ***************************************************************************/

#ifndef TAGLIB_MODTAG_H
#define TAGLIB_MODTAG_H

#include "tag.h"

namespace TagLib {

  namespace Mod {

    /*!
     * Tags for module files (Mod, S3M, IT, XM).
     *
     * Note that only the \a title is supported as such by most
     * module file formats.  Except for XM files the \a trackerName
     * is derived from the file format or the flavour of the file
     * format.  For XM files it is stored in the file.
     *
     * The \a comment tag is not strictly supported by module files,
     * but it is common practice to abuse instrument/sample/pattern
     * names as multiline comments.  TagLib does so as well.
     */
    class TAGLIB_EXPORT Tag : public TagLib::Tag
    {
    public:
      Tag();
      virtual ~Tag();

      /*!
       * Returns the track name; if no track name is present in the tag
       * String::null will be returned.
       */
      String title() const;

      /*!
       * Not supported by module files.  Therefore always returns String::null.
       */
      String artist() const;

      /*!
       * Not supported by module files.  Therefore always returns String::null.
       */
      String album() const;

      /*!
       * Returns the track comment derived from the instrument/sample/pattern
       * names; if no comment is present in the tag String::null will be
       * returned.
       */
      String comment() const;

      /*!
       * Not supported by module files.  Therefore always returns String::null.
       */
      String genre() const;

      /*!
       * Not supported by module files.  Therefore always returns 0.
       */
      uint year() const;

      /*!
       * Not supported by module files.  Therefore always returns 0.
       */
      uint track() const;

      /*!
       * Returns the name of the tracker used to create/edit the module file.
       * Only XM files store this tag to the file as such, for other formats
       * (Mod, S3M, IT) this is derived from the file type or the flavour of
       * the file type.  Therefore only XM files might have an empty
       * (String::null) tracker name.
       */
      String trackerName() const;

      /*!
       * Sets the title to \a title.  If \a title is String::null then this
       * value will be cleared.
       *
       * The length limits per file type are (1 characetr = 1 byte):
       * Mod 20 characters, S3M 27 characters, IT 25 characters and XM 20
       * characters.
       */
      void setTitle(const String &title);

      /*!
       * Not supported by module files and therefore ignored.
       */
      void setArtist(const String &artist);

      /*!
       * Not supported by module files and therefore ignored.
       */
      void setAlbum(const String &album);

      /*!
       * Sets the comment to \a comment.  If \a comment is String::null then
       * this value will be cleared.
       *
       * Note that module file formats don't actually support a comment tag.
       * Instead the names of instruments/patterns/samples are abused as
       * a multiline comment. Because of this the number of lines in a
       * module file is fixed to the number of instruments/patterns/samples.
       *
       * Also note that the instrument/pattern/sample name length is limited
       * an thus the line length in comments are limited. Too big comments
       * will be truncated.
       *
       * The line length limits per file type are (1 characetr = 1 byte):
       * Mod 22 characters, S3M 27 characters, IT 25 characters and XM 22
       * characters.
       */
      void setComment(const String &comment);

      /*!
       * Not supported by module files and therefore ignored.
       */
      void setGenre(const String &genre);

      /*!
       * Not supported by module files and therefore ignored.
       */
      void setYear(uint year);

      /*!
       * Not supported by module files and therefore ignored.
       */
      void setTrack(uint track);

      /*!
       * Sets the tracker name to \a trackerName.  If \a trackerName is
       * String::null then this value will be cleared.
       *
       * Note that only XM files support this tag.  Setting the
       * tracker name for other module file formats will be ignored.
       *
       * The length of this tag is limited to 20 characters (1 character
       * = 1 byte).
       */
      void setTrackerName(const String &trackerName);

      /*!
       * Implements the unified property interface -- export function.
       * Since the module tag is very limited, the exported map is as well.
       */
      PropertyMap properties() const;

      /*!
       * Implements the unified property interface -- import function.
       * Because of the limitations of the module file tag, any tags besides
       * COMMENT, TITLE and, if it is an XM file, TRACKERNAME, will be
       * returened. Additionally, if the map contains tags with multiple values,
       * all but the first will be contained in the returned map of unsupported
       * properties.
       */
      PropertyMap setProperties(const PropertyMap &);

    private:
      Tag(const Tag &);
      Tag &operator=(const Tag &);

      class TagPrivate;
      TagPrivate *d;
    };

  }

}

#endif
