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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_UNIQUEFILEIDENTIFIERFRAME
#define TAGLIB_UNIQUEFILEIDENTIFIERFRAME

#include <id3v2frame.h>

namespace TagLib {

  namespace ID3v2 {

    /*!
     * This is an implementation of ID3v2 unique file identifier frames.  This
     * frame is used to identify the file in an arbitrary database identified
     * by the owner field.
     */

    //! An implementation of ID3v2 unique identifier frames

    class TAGLIB_EXPORT UniqueFileIdentifierFrame : public ID3v2::Frame
    {
      friend class FrameFactory;

    public:
      /*!
       * Creates a uniqe file identifier frame based on \a data.
       */
      UniqueFileIdentifierFrame(const ByteVector &data);

      /*!
       * Creates a unique file identifier frame with the owner \a owner and
       * the identification \a id.
       */
      UniqueFileIdentifierFrame(const String &owner, const ByteVector &id);

      /*!
       * Destroys the frame.
       */
      ~UniqueFileIdentifierFrame();

      /*!
       * Returns the owner for the frame; essentially this is the key for
       * determining which identification scheme this key belongs to.  This
       * will usually either be an email address or URL for the person or tool
       * used to create the unique identifier.
       *
       * \see setOwner()
       */
      String owner() const;

      /*!
       * Returns the unique identifier.  Though sometimes this is a text string
       * it also may be binary data and as much should be assumed when handling
       * it.
       */
      ByteVector identifier() const;

      /*!
       * Sets the owner of the identification scheme to \a s.
       *
       * \see owner()
       */
      void setOwner(const String &s);

      /*!
       * Sets the unique file identifier to \a v.
       *
       * \see identifier()
       */
      void setIdentifier(const ByteVector &v);

      virtual String toString() const;

    protected:
      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

    private:
      UniqueFileIdentifierFrame(const UniqueFileIdentifierFrame &);
      UniqueFileIdentifierFrame &operator=(UniqueFileIdentifierFrame &);

      UniqueFileIdentifierFrame(const ByteVector &data, Header *h);

      class UniqueFileIdentifierFramePrivate;
      UniqueFileIdentifierFramePrivate *d;
    };
  }
}

#endif
