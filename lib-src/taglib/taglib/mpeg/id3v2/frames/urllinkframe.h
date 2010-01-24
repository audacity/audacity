/***************************************************************************
    copyright            : (C) 2002 - 2008 by Scott Wheeler
    email                : wheeler@kde.org
    copyright            : (C) 2006 by Urs Fleisch
    email                : ufleisch@users.sourceforge.net
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

#ifndef TAGLIB_URLLINKFRAME_H
#define TAGLIB_URLLINKFRAME_H

#include <id3v2frame.h>

namespace TagLib {

  namespace ID3v2 {

    //! ID3v2 URL frame
    /*!
     * An implementation of ID3v2 URL link frames.
     */
    class TAGLIB_EXPORT UrlLinkFrame : public Frame
    {
      friend class FrameFactory;

    public:
      /*!
       * This is a dual purpose constructor.  \a data can either be binary data
       * that should be parsed or (at a minimum) the frame ID.
       */
      explicit UrlLinkFrame(const ByteVector &data);

      /*!
       * Destroys this UrlLinkFrame instance.
       */
      virtual ~UrlLinkFrame();

      /*!
       * Returns the URL.
       */
      virtual String url() const;

      /*!
       * Sets the URL to \a s.
       */
      virtual void setUrl(const String &s);

      // Reimplementations.

      virtual void setText(const String &s);
      virtual String toString() const;

    protected:
      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

      /*!
       * The constructor used by the FrameFactory.
       */
      UrlLinkFrame(const ByteVector &data, Header *h);

    private:
      UrlLinkFrame(const UrlLinkFrame &);
      UrlLinkFrame &operator=(const UrlLinkFrame &);

      class UrlLinkFramePrivate;
      UrlLinkFramePrivate *d;
    };

    //! ID3v2 User defined URL frame

    /*!
     * This is a specialization of URL link frames that allows for
     * user defined entries.  Each entry has a description in addition to the
     * normal list of fields that a URL link frame has.
     *
     * This description identifies the frame and must be unique.
     */
    class TAGLIB_EXPORT UserUrlLinkFrame : public UrlLinkFrame
    {
      friend class FrameFactory;

    public:
      /*!
       * Constructs an empty user defined URL link frame.  For this to be
       * a useful frame both a description and text must be set.
       */
      explicit UserUrlLinkFrame(String::Type encoding = String::Latin1);

      /*!
       * This is a dual purpose constructor.  \a data can either be binary data
       * that should be parsed or (at a minimum) the frame ID.
       */
      explicit UserUrlLinkFrame(const ByteVector &data);

      /*!
       * Destroys this UserUrlLinkFrame instance.
       */
      virtual ~UserUrlLinkFrame();

      // Reimplementations.

      virtual String toString() const;

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

      /*!
       * Returns the description for this frame.
       */
      String description() const;

      /*!
       * Sets the description of the frame to \a s.  \a s must be unique.
       */
      void setDescription(const String &s);

    protected:
      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

      /*!
       * The constructor used by the FrameFactory.
       */
      UserUrlLinkFrame(const ByteVector &data, Header *h);

    private:
      UserUrlLinkFrame(const UserUrlLinkFrame &);
      UserUrlLinkFrame &operator=(const UserUrlLinkFrame &);

      class UserUrlLinkFramePrivate;
      UserUrlLinkFramePrivate *d;
    };

  }
}
#endif
