/***************************************************************************
    copyright            : (C) 2008 by Lukas Lalinsky
    email                : lalinsky@gmail.com
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

#ifndef TAGLIB_POPULARIMETERFRAME_H
#define TAGLIB_POPULARIMETERFRAME_H

#include "id3v2frame.h"
#include "taglib_export.h"

namespace TagLib {

  namespace ID3v2 {

    //! An implementation of ID3v2 "popularimeter"

    /*!
     * This implements the ID3v2 popularimeter (POPM frame).  It concists of
     * an email, a rating and an optional counter.
     */

    class TAGLIB_EXPORT PopularimeterFrame : public Frame
    {
      friend class FrameFactory;

    public:
      /*!
       * Construct an empty popularimeter frame.
       */
      explicit PopularimeterFrame();

      /*!
       * Construct a popularimeter based on the data in \a data.
       */
      explicit PopularimeterFrame(const ByteVector &data);

      /*!
       * Destroys this PopularimeterFrame instance.
       */
      virtual ~PopularimeterFrame();

      /*!
       * Returns the text of this popularimeter.
       *
       * \see text()
       */
      virtual String toString() const;

      /*!
       * Returns the email.
       *
       * \see setEmail()
       */
      String email() const;

      /*!
       * Set the email.
       *
       * \see email()
       */
      void setEmail(const String &email);

      /*!
       * Returns the rating.
       *
       * \see setRating()
       */
      int rating() const;

      /*!
       * Set the rating.
       *
       * \see rating()
       */
      void setRating(int rating);

      /*!
       * Returns the counter.
       *
       * \see setCounter()
       */
      uint counter() const;

      /*!
       * Set the counter.
       *
       * \see counter()
       */
      void setCounter(uint counter);

    protected:
      // Reimplementations.

      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

    private:
      /*!
       * The constructor used by the FrameFactory.
       */
      PopularimeterFrame(const ByteVector &data, Header *h);
      PopularimeterFrame(const PopularimeterFrame &);
      PopularimeterFrame &operator=(const PopularimeterFrame &);

      class PopularimeterFramePrivate;
      PopularimeterFramePrivate *d;
    };

  }
}
#endif
