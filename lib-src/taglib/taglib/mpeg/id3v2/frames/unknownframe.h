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

#ifndef TAGLIB_UNKNOWNFRAME_H
#define TAGLIB_UNKNOWNFRAME_H

#include "id3v2frame.h"
#include "taglib_export.h"

namespace TagLib {

  namespace ID3v2 {

    //! A frame type \e unknown to TagLib.

    /*!
     * This class represents a frame type not known (or more often simply
     * unimplemented) in TagLib.  This is here provide a basic API for
     * manipulating the binary data of unknown frames and to provide a means
     * of rendering such \e unknown frames.
     *
     * Please note that a cleaner way of handling frame types that TagLib
     * does not understand is to subclass ID3v2::Frame and ID3v2::FrameFactory
     * to have your frame type supported through the standard ID3v2 mechanism.
     */

    class TAGLIB_EXPORT UnknownFrame : public Frame
    {
      friend class FrameFactory;

    public:
      UnknownFrame(const ByteVector &data);
      virtual ~UnknownFrame();

      virtual String toString() const;

      /*!
       * Returns the field data (everything but the header) for this frame.
       */
      ByteVector data() const;

    protected:
      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

    private:
      UnknownFrame(const ByteVector &data, Header *h);
      UnknownFrame(const UnknownFrame &);
      UnknownFrame &operator=(const UnknownFrame &);

      class UnknownFramePrivate;
      UnknownFramePrivate *d;
    };

  }
}
#endif
