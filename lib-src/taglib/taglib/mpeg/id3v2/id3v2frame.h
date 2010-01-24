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

#ifndef TAGLIB_ID3V2FRAME_H
#define TAGLIB_ID3V2FRAME_H

#include "tstring.h"
#include "tbytevector.h"
#include "taglib_export.h"

namespace TagLib {

  class StringList;

  namespace ID3v2 {

    class Tag;
    class FrameFactory;

    //! ID3v2 frame implementation

    /*!
     * This class is the main ID3v2 frame implementation.  In ID3v2, a tag is
     * split between a collection of frames (which are in turn split into fields
     * (Structure, <a href="id3v2-structure.html#4">4</a>)
     * (<a href="id3v2-frames.html">Frames</a>).  This class provides an API for
     * gathering information about and modifying ID3v2 frames.  Funtionallity
     * specific to a given frame type is handed in one of the many subclasses.
     */

    class TAGLIB_EXPORT Frame
    {
      friend class Tag;
      friend class FrameFactory;

    public:
      /*!
       * Destroys this Frame instance.
       */
      virtual ~Frame();

      /*!
       * Returns the Frame ID (Structure, <a href="id3v2-structure.html#4">4</a>)
       * (Frames, <a href="id3v2-frames.html#4">4</a>)
       */
      ByteVector frameID() const;

      /*!
       * Returns the size of the frame.
       */
      uint size() const;

      /*!
       * Returns the size of the frame header
       *
       * \deprecated This is only accurate for ID3v2.3 or ID3v2.4.  Please use
       * the call below which accepts an ID3v2 version number.  In the next
       * non-binary compatible release this will be made into a non-static
       * member that checks the internal ID3v2 version.
       */
      static uint headerSize(); // BIC: remove and make non-static

      /*!
       * Returns the size of the frame header for the given ID3v2 version.
       *
       * \deprecated Please see the explanation above.
       */
      static uint headerSize(uint version); // BIC: remove and make non-static

      /*!
       * Sets the data that will be used as the frame.  Since the length is not
       * known before the frame has been parsed, this should just be a pointer to
       * the first byte of the frame.  It will determine the length internally
       * and make that available through size().
       */
      void setData(const ByteVector &data);

      /*!
       * Set the text of frame in the sanest way possible.  This should only be
       * reimplemented in frames where there is some logical mapping to text.
       *
       * \note If the frame type supports multiple text encodings, this will not
       * change the text encoding of the frame; the string will be converted to
       * that frame's encoding.  Please use the specific APIs of the frame types
       * to set the encoding if that is desired.
       */
      virtual void setText(const String &text);

      /*!
       * This returns the textual representation of the data in the frame.
       * Subclasses must reimplement this method to provide a string
       * representation of the frame's data.
       */
      virtual String toString() const = 0;

      /*!
       * Render the frame back to its binary format in a ByteVector.
       */
      ByteVector render() const;

      /*!
       * Returns the text delimiter that is used between fields for the string
       * type \a t.
       */
      static ByteVector textDelimiter(String::Type t);

    protected:
      class Header;

      /*!
       * Constructs an ID3v2 frame using \a data to read the header information.
       * All other processing of \a data should be handled in a subclass.
       *
       * \note This need not contain anything more than a frame ID, but
       * \e must constain at least that.
       */
      explicit Frame(const ByteVector &data);

      /*!
       * This creates an Frame using the header \a h.
       *
       * The ownership of this header will be assigned to the frame and the
       * header will be deleted when the frame is destroyed.
       */
      Frame(Header *h);

      /*!
       * Returns a pointer to the frame header.
       */
      Header *header() const;

      /*!
       * Sets the header to \a h.  If \a deleteCurrent is true, this will free
       * the memory of the current header.
       *
       * The ownership of this header will be assigned to the frame and the
       * header will be deleted when the frame is destroyed.
       */
      void setHeader(Header *h, bool deleteCurrent = true);

      /*!
       * Called by setData() to parse the frame data.  It makes this information
       * available through the public API.
       */
      void parse(const ByteVector &data);

      /*!
       * Called by parse() to parse the field data.  It makes this information
       * available through the public API.  This must be overridden by the
       * subclasses.
       */
      virtual void parseFields(const ByteVector &data) = 0;

      /*!
       * Render the field data back to a binary format in a ByteVector.  This
       * must be overridden by subclasses.
       */
      virtual ByteVector renderFields() const = 0;

      /*!
       * Returns a ByteVector containing the field data given the frame data.
       * This correctly adjusts for the header size plus any additional frame
       * data that's specified in the frame header flags.
       */
      ByteVector fieldData(const ByteVector &frameData) const;

      /*!
       * Reads a String of type \a encodiong from the ByteVector \a data.  If \a
       * position is passed in it is used both as the starting point and is
       * updated to replect the position just after the string that has been read.
       * This is useful for reading strings sequentially.
       */
      String readStringField(const ByteVector &data, String::Type encoding,
                             int *positon = 0);

      /*!
       * Checks a the list of string values to see if they can be used with the
       * specified encoding and returns the recommended encoding.
       */
      static String::Type checkEncoding(const StringList &fields,
                                        String::Type encoding);

    private:
      Frame(const Frame &);
      Frame &operator=(const Frame &);

      class FramePrivate;
      friend class FramePrivate;
      FramePrivate *d;
    };

    //! ID3v2 frame header implementation

    /*!
     * The ID3v2 Frame Header (Structure, <a href="id3v2-structure.html#4">4</a>)
     *
     * Every ID3v2::Frame has an associated header that gives some general
     * properties of the frame and also makes it possible to identify the frame
     * type.
     *
     * As such when reading an ID3v2 tag ID3v2::FrameFactory first creates the
     * frame headers and then creates the appropriate Frame subclass based on
     * the type and attaches the header.
     */

    class TAGLIB_EXPORT Frame::Header
    {
    public:
      /*!
       * Construct a Frame Header based on \a data.  \a data must at least
       * contain a 4 byte frame ID, and optionally can contain flag data and the
       * frame size.  i.e. Just the frame id -- "TALB" -- is a valid value.
       *
       * \deprecated Please use the constructor below that accepts a version
       * number.
       */
      Header(const ByteVector &data, bool synchSafeInts);

      /*!
       * Construct a Frame Header based on \a data.  \a data must at least
       * contain a 4 byte frame ID, and optionally can contain flag data and the
       * frame size.  i.e. Just the frame id -- "TALB" -- is a valid value.
       *
       * \a version should be the ID3v2 version of the tag.
       */
      explicit Header(const ByteVector &data, uint version = 4);

      /*!
       * Destroys this Header instance.
       */
      virtual ~Header();

      /*!
       * Sets the data for the Header.
       *
       * \deprecated Please use the version below that accepts an ID3v2 version
       * number.
       */
      void setData(const ByteVector &data, bool synchSafeInts);

      /*!
       * Sets the data for the Header.  \a version should indicate the ID3v2
       * version number of the tag that this frame is contained in.
       */
      void setData(const ByteVector &data, uint version = 4);

      /*!
       * Returns the Frame ID (Structure, <a href="id3v2-structure.html#4">4</a>)
       * (Frames, <a href="id3v2-frames.html#4">4</a>)
       */
      ByteVector frameID() const;

      /*!
       * Sets the frame's ID to \a id.  Only the first four bytes of \a id will
       * be used.
       *
       * \warning This method should in general be avoided.  It exists simply to
       * provide a mechanism for transforming frames from a deprecated frame type
       * to a newer one -- i.e. TYER to TDRC from ID3v2.3 to ID3v2.4.
       */
      void setFrameID(const ByteVector &id);

      /*!
       * Returns the size of the frame data portion, as set when setData() was
       * called or set explicitly via setFrameSize().
       */
      uint frameSize() const;

      /*!
       * Sets the size of the frame data portion.
       */
      void setFrameSize(uint size);

      /*!
       * Returns the ID3v2 version of the header (as passed in from the
       * construction of the header).
       */
      uint version() const;

      /*!
       * Returns the size of the frame header in bytes.
       *
       * \deprecated Please use the version of this method that accepts a
       * version.  This is only accurate for ID3v2.3 and ID3v2.4.  This will be
       * removed in the next binary incompatible release (2.0) and will be
       * replaced with a non-static method that checks the frame version.
       */
      static uint size();

      /*!
       * Returns the size of the frame header in bytes for the ID3v2 version
       * that's given.
       *
       * \deprecated Please see the explanation in the version above.
       */
      static uint size(uint version);

      /*!
       * Returns true if the flag for tag alter preservation is set.
       *
       * The semantics are a little backwards from what would seem natural
       * (setting the preservation flag to throw away the frame), but this
       * follows the ID3v2 standard.
       *
       * \see setTagAlterPreservation()
       */
      bool tagAlterPreservation() const;

      /*!
       * Sets the flag for preservation of this frame if the tag is set.  If
       * this is set to true the frame will not be written when the tag is
       * saved.
       *
       * The semantics are a little backwards from what would seem natural
       * (setting the preservation flag to throw away the frame), but this
       * follows the ID3v2 standard.
       *
       * \see tagAlterPreservation()
       */
      void setTagAlterPreservation(bool discard);

      /*!
       * Returns true if the flag for file alter preservation is set.
       *
       * \note This flag is currently ignored internally in TagLib.
       */
      bool fileAlterPreservation() const;

      /*!
       * Returns true if the frame is meant to be read only.
       *
       * \note This flag is currently ignored internally in TagLib.
       */
      bool readOnly() const;

      /*!
       * Returns true if the flag for the grouping identifity is set.
       *
       * \note This flag is currently ignored internally in TagLib.
       */
      bool groupingIdentity() const;

      /*!
       * Returns true if compression is enabled for this frame.
       *
       * \note This flag is currently ignored internally in TagLib.
       */
      bool compression() const;

      /*!
       * Returns true if encryption is enabled for this frame.
       *
       * \note This flag is currently ignored internally in TagLib.
       */
      bool encryption() const;

#ifndef DO_NOT_DOCUMENT
      bool unsycronisation() const;
#endif

      /*!
       * Returns true if unsynchronisation is enabled for this frame.
       */
      bool unsynchronisation() const;

      /*!
       * Returns true if the flag for a data length indicator is set.
       */
      bool dataLengthIndicator() const;

      /*!
       * Render the Header back to binary format in a ByteVector.
       */
      ByteVector render() const;

      /*!
       * \deprecated
       */
      bool frameAlterPreservation() const;

    private:
      Header(const Header &);
      Header &operator=(const Header &);

      class HeaderPrivate;
      HeaderPrivate *d;
    };

  }
}

#endif
