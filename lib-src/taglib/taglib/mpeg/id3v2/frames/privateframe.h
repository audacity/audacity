/***************************************************************************
    copyright            : (C) 2008 by Serkan Kalyoncu
    copyright            : (C) 2008 by Scott Wheeler
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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_PRIVATEFRAME_H
#define TAGLIB_PRIVATEFRAME_H

#include "id3v2frame.h"
#include "taglib_export.h"

namespace TagLib {

  namespace ID3v2 {

    //! An implementation of ID3v2 privateframe

    class TAGLIB_EXPORT PrivateFrame : public Frame
    {
      friend class FrameFactory;

    public:
      /*!
       * Construct an empty private frame.
       */
      PrivateFrame();

      /*!
       * Construct a private frame based on the data in \a data.
       *
       * \note This is the constructor used when parsing the frame from a file.
       */
      explicit PrivateFrame(const ByteVector &data);

      /*!
       * Destroys this private frame instance.
       */
      virtual ~PrivateFrame();

      /*!
       * Returns the text of this private frame, currently just the owner.
       *
       * \see text()
       */
      virtual String toString() const;

      /*!
       * \return The owner of the private frame.
       * \note This should contain an email address or link to a website.
       */
      String owner() const;

      /*!
       *
       */
      ByteVector data() const;

      /*!
       * Sets the owner of the frame to \a s.
       * \note This should contain an email address or link to a website.
       */
      void setOwner(const String &s);

      /*!
       *
       */
      void setData(const ByteVector &v);

    protected:
      // Reimplementations.

      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

    private:
      /*!
       * The constructor used by the FrameFactory.
       */
      PrivateFrame(const ByteVector &data, Header *h);

      PrivateFrame(const PrivateFrame &);
      PrivateFrame &operator=(const PrivateFrame &);

      class PrivateFramePrivate;
      PrivateFramePrivate *d;
    };

  }
}
#endif
