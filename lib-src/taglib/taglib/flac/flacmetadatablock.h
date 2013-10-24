/**************************************************************************
    copyright            : (C) 2010 by Lukáš Lalinský
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
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *
 *   02110-1301  USA                                                       *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_FLACMETADATABLOCK_H
#define TAGLIB_FLACMETADATABLOCK_H

#include "tlist.h"
#include "tbytevector.h"
#include "taglib_export.h"

namespace TagLib {

  namespace FLAC {

    class TAGLIB_EXPORT MetadataBlock
    {
    public:
      MetadataBlock();
      virtual ~MetadataBlock();

      enum BlockType {
        StreamInfo = 0,
        Padding,
        Application,
        SeekTable,
        VorbisComment,
        CueSheet,
        Picture
      };

      /*!
       * Returns the FLAC metadata block type.
       */
      virtual int code() const = 0;

      /*!
       * Render the content of the block.
       */
      virtual ByteVector render() const = 0;

    private:
      MetadataBlock(const MetadataBlock &item);
      MetadataBlock &operator=(const MetadataBlock &item);

      class MetadataBlockPrivate;
      MetadataBlockPrivate *d;
    };

  }

}

#endif
