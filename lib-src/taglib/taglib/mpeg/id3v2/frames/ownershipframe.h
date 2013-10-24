/***************************************************************************
    copyright            : (C) 2012 by Rupert Daniel
    email                : rupert@cancelmonday.com
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

#ifndef TAGLIB_OWNERSHIPFRAME_H
#define TAGLIB_OWNERSHIPFRAME_H

#include "id3v2frame.h"
#include "taglib_export.h"

namespace TagLib {

  namespace ID3v2 {

    //! An implementation of ID3v2 "ownership"

    /*!
     * This implements the ID3v2 ownership (OWNE frame).  It consists of
     * a price paid, a date purchased (YYYYMMDD) and the name of the seller.
     */

    class TAGLIB_EXPORT OwnershipFrame : public Frame
    {
      friend class FrameFactory;

    public:
      /*!
       * Construct an empty ownership frame.
       */
      explicit OwnershipFrame(String::Type encoding = String::Latin1);

      /*!
       * Construct a ownership based on the data in \a data.
       */
      explicit OwnershipFrame(const ByteVector &data);

      /*!
       * Destroys this OwnershipFrame instance.
       */
      virtual ~OwnershipFrame();

      /*!
       * Returns the text of this popularimeter.
       *
       * \see text()
       */
      virtual String toString() const;

      /*!
       * Returns the date purchased.
       *
       * \see setDatePurchased()
       */
      String datePurchased() const;

      /*!
       * Set the date purchased.
       *
       * \see datePurchased()
       */
      void setDatePurchased(const String &datePurchased);

      /*!
       * Returns the price paid.
       *
       * \see setPricePaid()
       */
      String pricePaid() const;

      /*!
       * Set the price paid.
       *
       * \see pricePaid()
       */
      void setPricePaid(const String &pricePaid);
      
      /*!
       * Returns the seller.
       *
       * \see setSeller()
       */
      String seller() const;
      
      /*!
       * Set the seller.
       *
       * \see seller()
       */
      void setSeller(const String &seller);
      
      /*!
       * Returns the text encoding that will be used in rendering this frame.
       * This defaults to the type that was either specified in the constructor
       * or read from the frame when parsed.
       *
       * \see setTextEncoding()
       * \see render()
       */
      String::Type textEncoding() const;
      
      /*!
       * Sets the text encoding to be used when rendering this frame to
       * \a encoding.
       *
       * \see textEncoding()
       * \see render()
       */
      void setTextEncoding(String::Type encoding);

    protected:
      // Reimplementations.

      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

    private:
      /*!
       * The constructor used by the FrameFactory.
       */
      OwnershipFrame(const ByteVector &data, Header *h);
      OwnershipFrame(const OwnershipFrame &);
      OwnershipFrame &operator=(const OwnershipFrame &);

      class OwnershipFramePrivate;
      OwnershipFramePrivate *d;
    };

  }
}
#endif
