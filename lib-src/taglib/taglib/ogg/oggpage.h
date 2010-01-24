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

#ifndef TAGLIB_OGGPAGE_H
#define TAGLIB_OGGPAGE_H

#include "taglib_export.h"
#include "tbytevectorlist.h"

namespace TagLib {

  namespace Ogg {

    class File;
    class PageHeader;

    //! An implementation of Ogg pages

    /*!
     * This is an implementation of the pages that make up an Ogg stream.
     * This handles parsing pages and breaking them down into packets and handles
     * the details of packets spanning multiple pages and pages that contiain
     * multiple packets.
     *
     * In most Xiph.org formats the comments are found in the first few packets,
     * this however is a reasonably complete implementation of Ogg pages that
     * could potentially be useful for non-meta data purposes.
     */

    class TAGLIB_EXPORT Page
    {
    public:
      /*!
       * Read an Ogg page from the \a file at the position \a pageOffset.
       */
      Page(File *file, long pageOffset);

      virtual ~Page();

      /*!
       * Returns the page's position within the file (in bytes).
       */
      long fileOffset() const;

      /*!
       * Returns a pointer to the header for this page.  This pointer will become
       * invalid when the page is deleted.
       */
      const PageHeader *header() const;

      /*!
       * Returns the index of the first packet wholly or partially contained in
       * this page.
       *
       * \see setFirstPacketIndex()
       */
      int firstPacketIndex() const;

      /*!
       * Sets the index of the first packet in the page.
       *
       * \see firstPacketIndex()
       */
      void setFirstPacketIndex(int index);

      /*!
       * When checking to see if a page contains a given packet this set of flags
       * represents the possible values for that packets status in the page.
       *
       * \see containsPacket()
       */
      enum ContainsPacketFlags {
        //! No part of the packet is contained in the page
        DoesNotContainPacket = 0x0000,
        //! The packet is wholly contained in the page
        CompletePacket       = 0x0001,
        //! The page starts with the given packet
        BeginsWithPacket     = 0x0002,
        //! The page ends with the given packet
        EndsWithPacket       = 0x0004
      };

      /*!
       * Checks to see if the specified \a packet is contained in the current
       * page.
       *
       * \see ContainsPacketFlags
       */
      ContainsPacketFlags containsPacket(int index) const;

      /*!
       * Returns the number of packets (whole or partial) in this page.
       */
      uint packetCount() const;

      /*!
       * Returns a list of the packets in this page.
       *
       * \note Either or both the first and last packets may be only partial.
       * \see PageHeader::firstPacketContinued()
       */
      ByteVectorList packets() const;

      /*!
       * Returns the size of the page in bytes.
       */
      int size() const;

      ByteVector render() const;

      /*!
       * Defines a strategy for pagination, or grouping pages into Ogg packets,
       * for use with pagination methods.
       *
       * \note Yes, I'm aware that this is not a canonical "Strategy Pattern",
       * the term was simply convenient.
       */
      enum PaginationStrategy {
        /*!
         * Attempt to put the specified set of packets into a single Ogg packet.
         * If the sum of the packet data is greater than will fit into a single
         * Ogg page -- 65280 bytes -- this will fall back to repagination using
         * the recommended page sizes.
         */
        SinglePagePerGroup,
        /*!
         * Split the packet or group of packets into pages that conform to the
         * sizes recommended in the Ogg standard.
         */
        Repaginate
      };

      /*!
       * Pack \a packets into Ogg pages using the \a strategy for pagination.
       * The page number indicater inside of the rendered packets will start
       * with \a firstPage and be incremented for each page rendered.
       * \a containsLastPacket should be set to true if \a packets contains the
       * last page in the stream and will set the appropriate flag in the last
       * rendered Ogg page's header.  \a streamSerialNumber should be set to
       * the serial number for this stream.
       *
       * \note The "absolute granule position" is currently always zeroed using
       * this method as this suffices for the comment headers.
       *
       * \warning The pages returned by this method must be deleted by the user.
       * You can use List<T>::setAutoDelete(true) to set these pages to be
       * automatically deleted when this list passes out of scope.
       *
       * \see PaginationStrategy
       * \see List::setAutoDelete()
       */
      static List<Page *> paginate(const ByteVectorList &packets,
                                   PaginationStrategy strategy,
                                   uint streamSerialNumber,
                                   int firstPage,
                                   bool firstPacketContinued = false,
                                   bool lastPacketCompleted = true,
                                   bool containsLastPacket = false);

    protected:
      /*!
       * Creates an Ogg packet based on the data in \a packets.  The page number
       * for each page will be set to \a pageNumber.
       */
      Page(const ByteVectorList &packets,
           uint streamSerialNumber,
           int pageNumber,
           bool firstPacketContinued = false,
           bool lastPacketCompleted = true,
           bool containsLastPacket = false);

    private:
      Page(const Page &);
      Page &operator=(const Page &);

      class PagePrivate;
      PagePrivate *d;
    };
  }
}
#endif
