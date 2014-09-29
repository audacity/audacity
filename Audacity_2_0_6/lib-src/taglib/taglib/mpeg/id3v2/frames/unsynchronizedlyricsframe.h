/***************************************************************************
    copyright            : (C) 2002 - 2008 by Scott Wheeler
    email                : wheeler@kde.org
    copyright            : (C) 2006 by Urs Fleisch
    email                : ufleisch@users.sourceforge.net
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it  under the terms of the GNU Lesser General Public License version  *
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

#ifndef TAGLIB_UNSYNCHRONIZEDLYRICSFRAME_H
#define TAGLIB_UNSYNCHRONIZEDLYRICSFRAME_H

#include "id3v2frame.h"

namespace TagLib {

  namespace ID3v2 {

    //! ID3v2 unsynchronized lyrics frame
    /*!
     * An implementation of ID3v2 unsynchronized lyrics.
     */
    class TAGLIB_EXPORT UnsynchronizedLyricsFrame : public Frame
    {
      friend class FrameFactory;

    public:
      /*!
       * Construct an empty unsynchronized lyrics frame that will use the text encoding
       * \a encoding.
       */
      explicit UnsynchronizedLyricsFrame(String::Type encoding = String::Latin1);

      /*!
       * Construct a unsynchronized lyrics frame based on the data in \a data.
       */
      explicit UnsynchronizedLyricsFrame(const ByteVector &data);

      /*!
       * Destroys this UnsynchronizedLyricsFrame instance.
       */
      virtual ~UnsynchronizedLyricsFrame();

      /*!
       * Returns the text of this unsynchronized lyrics frame.
       *
       * \see text()
       */
      virtual String toString() const;

      /*!
       * Returns the language encoding as a 3 byte encoding as specified by
       * <a href="http://en.wikipedia.org/wiki/ISO_639">ISO-639-2</a>.
       *
       * \note Most taggers simply ignore this value.
       *
       * \see setLanguage()
       */
      ByteVector language() const;

      /*!
       * Returns the description of this unsynchronized lyrics frame.
       *
       * \note Most taggers simply ignore this value.
       *
       * \see setDescription()
       */
      String description() const;

      /*!
       * Returns the text of this unsynchronized lyrics frame.
       *
       * \see setText()
       */
      String text() const;

      /*!
       * Set the language using the 3 byte language code from
       * <a href="http://en.wikipedia.org/wiki/ISO_639">ISO-639-2</a> to
       * \a languageCode.
       *
       * \see language()
       */
      void setLanguage(const ByteVector &languageCode);

      /*!
       * Sets the description of the unsynchronized lyrics frame to \a s.
       *
       * \see decription()
       */
      void setDescription(const String &s);

      /*!
       * Sets the text portion of the unsynchronized lyrics frame to \a s.
       *
       * \see text()
       */
      virtual void setText(const String &s);

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


      /*! Parses this frame as PropertyMap with a single key.
       * - if description() is empty or "LYRICS", the key will be "LYRICS"
       * - if description() is not a valid PropertyMap key, the frame will be
       *   marked unsupported by an entry "USLT/<description>" in the unsupportedData()
       *   attribute of the returned map.
       * - otherwise, the key will be "LYRICS:<description>"
       * - The single value will be the frame's text().
       * Note that currently the language() field is not supported by the PropertyMap
       * interface.
       */
      PropertyMap asProperties() const;

      /*!
       * LyricsFrames each have a unique description.  This searches for a lyrics
       * frame with the decription \a d and returns a pointer to it.  If no
       * frame is found that matches the given description null is returned.
       *
       * \see description()
       */
      static UnsynchronizedLyricsFrame *findByDescription(const Tag *tag, const String &d);

    protected:
      // Reimplementations.

      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

    private:
      /*!
       * The constructor used by the FrameFactory.
       */
      UnsynchronizedLyricsFrame(const ByteVector &data, Header *h);
      UnsynchronizedLyricsFrame(const UnsynchronizedLyricsFrame &);
      UnsynchronizedLyricsFrame &operator=(const UnsynchronizedLyricsFrame &);

      class UnsynchronizedLyricsFramePrivate;
      UnsynchronizedLyricsFramePrivate *d;
    };

  }
}
#endif
