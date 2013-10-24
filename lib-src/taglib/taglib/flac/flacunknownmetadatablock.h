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

#ifndef TAGLIB_FLACUNKNOWNMETADATABLOCK_H
#define TAGLIB_FLACUNKNOWNMETADATABLOCK_H

#include "tlist.h"
#include "tbytevector.h"
#include "taglib_export.h"
#include "flacmetadatablock.h"

namespace TagLib {

  namespace FLAC {

    class TAGLIB_EXPORT UnknownMetadataBlock : public MetadataBlock
    {
    public:
      UnknownMetadataBlock(int blockType, const ByteVector &data);
      ~UnknownMetadataBlock();

      /*!
       * Returns the FLAC metadata block type.
       */
      int code() const;

      /*!
       * Sets the FLAC metadata block type.
       */
      void setCode(int code);

      /*!
       * Returns the FLAC metadata block type.
       */
      ByteVector data() const;

      /*!
       * Sets the FLAC metadata block type.
       */
      void setData(const ByteVector &data);

      /*!
       * Render the content of the block.
       */
      ByteVector render() const;

    private:
      UnknownMetadataBlock(const MetadataBlock &item);
      UnknownMetadataBlock &operator=(const MetadataBlock &item);

      class UnknownMetadataBlockPrivate;
      UnknownMetadataBlockPrivate *d;
    };

  }

}

#endif
