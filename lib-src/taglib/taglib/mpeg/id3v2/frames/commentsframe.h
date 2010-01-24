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

#ifndef TAGLIB_COMMENTSFRAME_H
#define TAGLIB_COMMENTSFRAME_H

#include <id3v2frame.h>
#include "taglib_export.h"

namespace TagLib {

  namespace ID3v2 {

    //! An implementation of ID3v2 comments

    /*!
     * This implements the ID3v2 comment format.  An ID3v2 comment concists of
     * a language encoding, a description and a single text field.
     */

    class TAGLIB_EXPORT CommentsFrame : public Frame
    {
      friend class FrameFactory;

    public:
      /*!
       * Construct an empty comment frame that will use the text encoding
       * \a encoding.
       */
      explicit CommentsFrame(String::Type encoding = String::Latin1);

      /*!
       * Construct a comment based on the data in \a data.
       */
      explicit CommentsFrame(const ByteVector &data);

      /*!
       * Destroys this CommentFrame instance.
       */
      virtual ~CommentsFrame();

      /*!
       * Returns the text of this comment.
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
       * Returns the description of this comment.
       *
       * \note Most taggers simply ignore this value.
       *
       * \see setDescription()
       */
      String description() const;

      /*!
       * Returns the text of this comment.
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
       * Sets the description of the comment to \a s.
       *
       * \see decription()
       */
      void setDescription(const String &s);

      /*!
       * Sets the text portion of the comment to \a s.
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

      /*!
       * Comments each have a unique description.  This searches for a comment
       * frame with the decription \a d and returns a pointer to it.  If no
       * frame is found that matches the given description null is returned.
       *
       * \see description()
       */
      static CommentsFrame *findByDescription(const Tag *tag, const String &d);

    protected:
      // Reimplementations.

      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

    private:
      /*!
       * The constructor used by the FrameFactory.
       */
      CommentsFrame(const ByteVector &data, Header *h);
      CommentsFrame(const CommentsFrame &);
      CommentsFrame &operator=(const CommentsFrame &);

      class CommentsFramePrivate;
      CommentsFramePrivate *d;
    };

  }
}
#endif
