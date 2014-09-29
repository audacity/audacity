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

#ifndef TAGLIB_OGGPAGEHEADER_H
#define TAGLIB_OGGPAGEHEADER_H

#include "tlist.h"
#include "tbytevector.h"
#include "taglib_export.h"

namespace TagLib {

  namespace Ogg {

    class File;

    //! An implementation of the page headers associated with each Ogg::Page

    /*!
     * This class implements Ogg page headers which contain the information
     * about Ogg pages needed to break them into packets which can be passed on
     * to the codecs.
     */

    class TAGLIB_EXPORT PageHeader
    {
    public:
      /*!
       * Reads a PageHeader from \a file starting at \a pageOffset.  The defaults
       * create a page with no (and as such, invalid) data that must be set
       * later.
       */
      PageHeader(File *file = 0, long pageOffset = -1);

      /*!
       * Deletes this instance of the PageHeader.
       */
      virtual ~PageHeader();

      /*!
       * Returns true if the header parsed properly and is valid.
       */
      bool isValid() const;

      /*!
       * Ogg pages contain a list of packets (which are used by the contained
       * codecs).  The sizes of these pages is encoded in the page header.  This
       * returns a list of the packet sizes in bytes.
       *
       * \see setPacketSizes()
       */
      List<int> packetSizes() const;

      /*!
       * Sets the sizes of the packets in this page to \a sizes.  Internally this
       * updates the lacing values in the header.
       *
       * \see packetSizes()
       */
      void setPacketSizes(const List<int> &sizes);

      /*!
       * Some packets can be <i>continued</i> across multiple pages.  If the
       * first packet in the current page is a continuation this will return
       * true.  If this is page starts with a new packet this will return false.
       *
       * \see lastPacketCompleted()
       * \see setFirstPacketContinued()
       */
      bool firstPacketContinued() const;

      /*!
       * Sets the internal flag indicating if the first packet in this page is
       * continued to \a continued.
       *
       * \see firstPacketContinued()
       */
      void setFirstPacketContinued(bool continued);

      /*!
       * Returns true if the last packet of this page is completely contained in
       * this page.
       *
       * \see firstPacketContinued()
       * \see setLastPacketCompleted()
       */
      bool lastPacketCompleted() const;

      /*!
       * Sets the internal flag indicating if the last packet in this page is
       * complete to \a completed.
       *
       * \see lastPacketCompleted()
       */
      void setLastPacketCompleted(bool completed);

      /*!
       * This returns true if this is the first page of the Ogg (logical) stream.
       *
       * \see setFirstPageOfStream()
       */
      bool firstPageOfStream() const;

      /*!
       * Marks this page as the first page of the Ogg stream.
       *
       * \see firstPageOfStream()
       */
      void setFirstPageOfStream(bool first);

      /*!
       * This returns true if this is the last page of the Ogg (logical) stream.
       *
       * \see setLastPageOfStream()
       */
      bool lastPageOfStream() const;

      /*!
       * Marks this page as the last page of the Ogg stream.
       *
       * \see lastPageOfStream()
       */
      void setLastPageOfStream(bool last);

      /*!
       * A special value of containing the position of the packet to be
       * interpreted by the codec.  In the case of Vorbis this contains the PCM
       * value and is used to calculate the length of the stream.
       *
       * \see setAbsoluteGranularPosition()
       */
      long long absoluteGranularPosition() const;

      /*!
       * A special value of containing the position of the packet to be
       * interpreted by the codec.  It is only supported here so that it may be
       * coppied from one page to another.
       *
       * \see absoluteGranularPosition()
       */
      void setAbsoluteGranularPosition(long long agp);

      /*!
       * Every Ogg logical stream is given a random serial number which is common
       * to every page in that logical stream.  This returns the serial number of
       * the stream associated with this packet.
       *
       * \see setStreamSerialNumber()
       */
      uint streamSerialNumber() const;

      /*!
       * Every Ogg logical stream is given a random serial number which is common
       * to every page in that logical stream.  This sets this pages serial
       * number.  This method should be used when adding new pages to a logical
       * stream.
       *
       * \see streamSerialNumber()
       */
      void setStreamSerialNumber(uint n);

      /*!
       * Returns the index of the page within the Ogg stream.  This helps make it
       * possible to determine if pages have been lost.
       *
       * \see setPageSequenceNumber()
       */
      int pageSequenceNumber() const;

      /*!
       * Sets the page's position in the stream to \a sequenceNumber.
       *
       * \see pageSequenceNumber()
       */
      void setPageSequenceNumber(int sequenceNumber);

      /*!
       * Returns the complete header size.
       */
      int size() const;

      /*!
       * Returns the size of the data portion of the page -- i.e. the size of the
       * page less the header size.
       */
      int dataSize() const;

      /*!
       * Render the page header to binary data.
       *
       * \note The checksum -- bytes 22 - 25 -- will be left empty and must be
       * filled in when rendering the entire page.
       */
      ByteVector render() const;

    private:
      PageHeader(const PageHeader &);
      PageHeader &operator=(const PageHeader &);

      void read();
      ByteVector lacingValues() const;

      class PageHeaderPrivate;
      PageHeaderPrivate *d;
    };

  }
}

#endif
