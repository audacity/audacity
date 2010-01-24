/**************************************************************************
    copyright            : (C) 2005-2007 by Lukáš Lalinský
    email                : lalinsky@gmail.com
 **************************************************************************/

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

#ifndef TAGLIB_ASFFILE_H
#define TAGLIB_ASFFILE_H

#include <tag.h>
#include <tfile.h>
#include "taglib_export.h"
#include "asfproperties.h"
#include "asftag.h"

namespace TagLib {

  //! An implementation of ASF (WMA) metadata
  namespace ASF {

    /*!
     * This implements and provides an interface for ASF files to the
     * TagLib::Tag and TagLib::AudioProperties interfaces by way of implementing
     * the abstract TagLib::File API as well as providing some additional
     * information specific to ASF files.
     */
    class TAGLIB_EXPORT File : public TagLib::File
    {
    public:

      /*!
       * Contructs an ASF file from \a file.  If \a readProperties is true the
       * file's audio properties will also be read using \a propertiesStyle.  If
       * false, \a propertiesStyle is ignored.
       *
       * \note In the current implementation, both \a readProperties and
       * \a propertiesStyle are ignored.
       */
      File(FileName file, bool readProperties = true, Properties::ReadStyle propertiesStyle = Properties::Average);

      /*!
       * Destroys this instance of the File.
       */
      virtual ~File();

      /*!
       * Returns a pointer to the ASF tag of the file.
       *
       * ASF::Tag implements the tag interface, so this serves as the
       * reimplementation of TagLib::File::tag().
       *
       * \note The Tag <b>is still</b> owned by the ASF::File and should not be
       * deleted by the user.  It will be deleted when the file (object) is
       * destroyed.
       */
      virtual Tag *tag() const;

      /*!
       * Returns the ASF audio properties for this file.
       */
      virtual Properties *audioProperties() const;

      /*!
       * Save the file.
       *
       * This returns true if the save was successful.
       */
      virtual bool save();

    private:

      int readBYTE();
      int readWORD();
      unsigned int readDWORD();
      long long readQWORD();
      static ByteVector renderString(const String &str, bool includeLength = false);
      String readString(int len);
      void read(bool readProperties, Properties::ReadStyle propertiesStyle);

      friend class Attribute;

      class BaseObject;
      class UnknownObject;
      class FilePropertiesObject;
      class StreamPropertiesObject;
      class ContentDescriptionObject;
      class ExtendedContentDescriptionObject;
      class HeaderExtensionObject;
      class MetadataObject;
      class MetadataLibraryObject;

      class FilePrivate;
      FilePrivate *d;
    };

  }

}

#endif
