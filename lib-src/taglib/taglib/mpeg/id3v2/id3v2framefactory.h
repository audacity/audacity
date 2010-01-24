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

#ifndef TAGLIB_ID3V2FRAMEFACTORY_H
#define TAGLIB_ID3V2FRAMEFACTORY_H

#include "taglib_export.h"
#include "tbytevector.h"
#include "id3v2frame.h"
#include "id3v2header.h"

namespace TagLib {

  namespace ID3v2 {

    class TAGLIB_EXPORT TextIdentificationFrame;

    //! A factory for creating ID3v2 frames during parsing

    /*!
     * This factory abstracts away the frame creation process and instantiates
     * the appropriate ID3v2::Frame subclasses based on the contents of the
     * data.
     *
     * Reimplementing this factory is the key to adding support for frame types
     * not directly supported by TagLib to your application.  To do so you would
     * subclass this factory reimplement createFrame().  Then by setting your
     * factory to be the default factory in ID3v2::Tag constructor or with
     * MPEG::File::setID3v2FrameFactory() you can implement behavior that will
     * allow for new ID3v2::Frame subclasses (also provided by you) to be used.
     *
     * This implements both <i>abstract factory</i> and <i>singleton</i> patterns
     * of which more information is available on the web and in software design
     * textbooks (Notably <i>Design Patters</i>).
     *
     * \note You do not need to use this factory to create new frames to add to
     * an ID3v2::Tag.  You can instantiate frame subclasses directly (with new)
     * and add them to a tag using ID3v2::Tag::addFrame()
     *
     * \see ID3v2::Tag::addFrame()
     */

    class TAGLIB_EXPORT FrameFactory
    {
    public:
      static FrameFactory *instance();
      /*!
       * Create a frame based on \a data.  \a synchSafeInts should only be set
       * false if we are parsing an old tag (v2.3 or older) that does not support
       * synchsafe ints.
       *
       * \deprecated Please use the method below that accepts a ID3v2::Header
       * instance in new code.
       */
      Frame *createFrame(const ByteVector &data, bool synchSafeInts) const;

      /*!
       * Create a frame based on \a data.  \a version should indicate the ID3v2
       * version of the tag.  As ID3v2.4 is the most current version of the
       * standard 4 is the default.
       *
       * \deprecated Please use the method below that accepts a ID3v2::Header
       * instance in new code.
       */
      Frame *createFrame(const ByteVector &data, uint version = 4) const;

      /*!
       * Create a frame based on \a data.  \a tagHeader should be a valid
       * ID3v2::Header instance.
       */
      // BIC: make virtual
      Frame *createFrame(const ByteVector &data, Header *tagHeader) const;

      /*!
       * Returns the default text encoding for text frames.  If setTextEncoding()
       * has not been explicitly called this will only be used for new text
       * frames.  However, if this value has been set explicitly all frames will be
       * converted to this type (unless it's explitly set differently for the
       * individual frame) when being rendered.
       *
       * \see setDefaultTextEncoding()
       */
      String::Type defaultTextEncoding() const;

      /*!
       * Set the default text encoding for all text frames that are created to
       * \a encoding.  If no value is set the frames with either default to the
       * encoding type that was parsed and new frames default to Latin1.
       *
       * Valid string types for ID3v2 tags are Latin1, UTF8, UTF16 and UTF16BE.
       *
       * \see defaultTextEncoding()
       */
      void setDefaultTextEncoding(String::Type encoding);

    protected:
      /*!
       * Constructs a frame factory.  Because this is a singleton this method is
       * protected, but may be used for subclasses.
       */
      FrameFactory();

      /*!
       * Destroys the frame factory.  In most cases this will never be called (as
       * is typical of singletons).
       */
      virtual ~FrameFactory();

      /*!
       * This method checks for compliance to the current ID3v2 standard (2.4)
       * and does nothing in the common case.  However if a frame is found that
       * is not compatible with the current standard, this method either updates
       * the frame or indicates that it should be discarded.
       *
       * This method with return true (with or without changes to the frame) if
       * this frame should be kept or false if it should be discarded.
       *
       * See the id3v2.4.0-changes.txt document for further information.
       */
      virtual bool updateFrame(Frame::Header *header) const;

    private:
      FrameFactory(const FrameFactory &);
      FrameFactory &operator=(const FrameFactory &);

      /*!
       * This method is used internally to convert a frame from ID \a from to ID
       * \a to.  If the frame matches the \a from pattern and converts the frame
       * ID in the \a header or simply does nothing if the frame ID does not match.
       */
      void convertFrame(const char *from, const char *to,
                        Frame::Header *header) const;

      void updateGenre(TextIdentificationFrame *frame) const;

      static FrameFactory *factory;

      class FrameFactoryPrivate;
      FrameFactoryPrivate *d;
    };

  }
}

#endif
