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

#ifndef TAGLIB_TEXTIDENTIFICATIONFRAME_H
#define TAGLIB_TEXTIDENTIFICATIONFRAME_H

#include "tstringlist.h"
#include "tmap.h"
#include "taglib_export.h"

#include "id3v2frame.h"

namespace TagLib {

  namespace ID3v2 {

    class Tag;
    typedef Map<String, String> KeyConversionMap;

    //! An ID3v2 text identification frame implementation

    /*!
     * This is an implementation of the most common type of ID3v2 frame -- text
     * identification frames.  There are a number of variations on this.  Those
     * enumerated in the ID3v2.4 standard are:
     *
     * <ul>
     *   <li><b>TALB</b> Album/Movie/Show title</li>
     *   <li><b>TBPM</b> BPM (beats per minute)</li>
     *   <li><b>TCOM</b> Composer</li>
     *   <li><b>TCON</b> Content type</li>
     *   <li><b>TCOP</b> Copyright message</li>
     *   <li><b>TDEN</b> Encoding time</li>
     *   <li><b>TDLY</b> Playlist delay</li>
     *   <li><b>TDOR</b> Original release time</li>
     *   <li><b>TDRC</b> Recording time</li>
     *   <li><b>TDRL</b> Release time</li>
     *   <li><b>TDTG</b> Tagging time</li>
     *   <li><b>TENC</b> Encoded by</li>
     *   <li><b>TEXT</b> Lyricist/Text writer</li>
     *   <li><b>TFLT</b> File type</li>
     *   <li><b>TIPL</b> Involved people list</li>
     *   <li><b>TIT1</b> Content group description</li>
     *   <li><b>TIT2</b> Title/songname/content description</li>
     *   <li><b>TIT3</b> Subtitle/Description refinement</li>
     *   <li><b>TKEY</b> Initial key</li>
     *   <li><b>TLAN</b> Language(s)</li>
     *   <li><b>TLEN</b> Length</li>
     *   <li><b>TMCL</b> Musician credits list</li>
     *   <li><b>TMED</b> Media type</li>
     *   <li><b>TMOO</b> Mood</li>
     *   <li><b>TOAL</b> Original album/movie/show title</li>
     *   <li><b>TOFN</b> Original filename</li>
     *   <li><b>TOLY</b> Original lyricist(s)/text writer(s)</li>
     *   <li><b>TOPE</b> Original artist(s)/performer(s)</li>
     *   <li><b>TOWN</b> File owner/licensee</li>
     *   <li><b>TPE1</b> Lead performer(s)/Soloist(s)</li>
     *   <li><b>TPE2</b> Band/orchestra/accompaniment</li>
     *   <li><b>TPE3</b> Conductor/performer refinement</li>
     *   <li><b>TPE4</b> Interpreted, remixed, or otherwise modified by</li>
     *   <li><b>TPOS</b> Part of a set</li>
     *   <li><b>TPRO</b> Produced notice</li>
     *   <li><b>TPUB</b> Publisher</li>
     *   <li><b>TRCK</b> Track number/Position in set</li>
     *   <li><b>TRSN</b> Internet radio station name</li>
     *   <li><b>TRSO</b> Internet radio station owner</li>
     *   <li><b>TSOA</b> Album sort order</li>
     *   <li><b>TSOP</b> Performer sort order</li>
     *   <li><b>TSOT</b> Title sort order</li>
     *   <li><b>TSRC</b> ISRC (international standard recording code)</li>
     *   <li><b>TSSE</b> Software/Hardware and settings used for encoding</li>
     *   <li><b>TSST</b> Set subtitle</li>
     * </ul>
     *
     * The ID3v2 Frames document gives a description of each of these formats
     * and the expected order of strings in each.  ID3v2::Header::frameID() can
     * be used to determine the frame type.
     *
     * \note If non-Latin1 compatible strings are used with this class, even if
     * the text encoding is set to Latin1, the frame will be written using UTF8
     * (with the encoding flag appropriately set in the output).
     */

    class TAGLIB_EXPORT TextIdentificationFrame : public Frame
    {
      friend class FrameFactory;

    public:
      /*!
       * Construct an empty frame of type \a type.  Uses \a encoding as the
       * default text encoding.
       *
       * \note In this case you must specify the text encoding as it
       * resolves the ambiguity between constructors.
       *
       * \note Please see the note in the class description regarding Latin1.
       */
      TextIdentificationFrame(const ByteVector &type, String::Type encoding);

      /*!
       * This is a dual purpose constructor.  \a data can either be binary data
       * that should be parsed or (at a minimum) the frame ID.
       */
      explicit TextIdentificationFrame(const ByteVector &data);

      /*!
       * This is a special factory method to create a TIPL (involved people list)
       * frame from the given \a properties. Will parse key=[list of values] data
       * into the TIPL format as specified in the ID3 standard.
       */
      static TextIdentificationFrame *createTIPLFrame(const PropertyMap &properties);

      /*!
       * This is a special factory method to create a TMCL (musician credits list)
       * frame from the given \a properties. Will parse key=[list of values] data
       * into the TMCL format as specified in the ID3 standard, where key should be
       * of the form instrumentPrefix:instrument.
       */
      static TextIdentificationFrame *createTMCLFrame(const PropertyMap &properties);
      /*!
       * Destroys this TextIdentificationFrame instance.
       */
      virtual ~TextIdentificationFrame();

      /*!
       * Text identification frames are a list of string fields.
       *
       * This function will accept either a StringList or a String (using the
       * StringList constructor that accepts a single String).
       *
       * \note This will not change the text encoding of the frame even if the
       * strings passed in are not of the same encoding.  Please use
       * setEncoding(s.type()) if you wish to change the encoding of the frame.
       */
      void setText(const StringList &l);

      // Reimplementations.

      virtual void setText(const String &s);
      virtual String toString() const;

      /*!
       * Returns the text encoding that will be used in rendering this frame.
       * This defaults to the type that was either specified in the constructor
       * or read from the frame when parsed.
       *
       * \note Please see the note in the class description regarding Latin1.
       *
       * \see setTextEncoding()
       * \see render()
       */
      String::Type textEncoding() const;

      /*!
       * Sets the text encoding to be used when rendering this frame to
       * \a encoding.
       *
       * \note Please see the note in the class description regarding Latin1.
       *
       * \see textEncoding()
       * \see render()
       */
      void setTextEncoding(String::Type encoding);

      /*!
       * Returns a list of the strings in this frame.
       */
      StringList fieldList() const;

      /*!
       * Returns a KeyConversionMap mapping a role as it would be  used in a PropertyMap
       * to the corresponding key used in a TIPL ID3 frame to describe that role.
       */
      static const KeyConversionMap &involvedPeopleMap();

      PropertyMap asProperties() const;

    protected:
      // Reimplementations.

      virtual void parseFields(const ByteVector &data);
      virtual ByteVector renderFields() const;

      /*!
       * The constructor used by the FrameFactory.
       */
      TextIdentificationFrame(const ByteVector &data, Header *h);

    private:
      TextIdentificationFrame(const TextIdentificationFrame &);
      TextIdentificationFrame &operator=(const TextIdentificationFrame &);

      /*!
       * Parses the special structure of a TIPL frame
       * Only the whitelisted roles "ARRANGER", "ENGINEER", "PRODUCER",
       * "DJMIXER" (ID3: "DJ-MIX") and "MIXER" (ID3: "MIX") are allowed.
       */
      PropertyMap makeTIPLProperties() const;
      /*!
       * Parses the special structure of a TMCL frame.
       */
      PropertyMap makeTMCLProperties() const;
      class TextIdentificationFramePrivate;
      TextIdentificationFramePrivate *d;
    };

    /*!
     * This is a specialization of text identification frames that allows for
     * user defined entries.  Each entry has a description in addition to the
     * normal list of fields that a text identification frame has.
     *
     * This description identifies the frame and must be unique.
     */

    //! An ID3v2 custom text identification frame implementationx

    class TAGLIB_EXPORT UserTextIdentificationFrame : public TextIdentificationFrame
    {
      friend class FrameFactory;

    public:
      /*!
       * Constructs an empty user defined text identification frame.  For this to be
       * a useful frame both a description and text must be set.
       */
      explicit UserTextIdentificationFrame(String::Type encoding = String::Latin1);

      /*!
       * Creates a frame based on \a data.
       */
      explicit UserTextIdentificationFrame(const ByteVector &data);

      /*!
       * Creates a user defined text identification frame with the given \a description
       * and \a values.
       */
      UserTextIdentificationFrame(const String &description, const StringList &values, String::Type encoding = String::UTF8);

      virtual String toString() const;

      /*!
       * Returns the description for this frame.
       */
      String description() const;

      /*!
       * Sets the description of the frame to \a s.  \a s must be unique.  You can
       * check for the presence of another user defined text frame of the same type
       * using find() and testing for null.
       */
      void setDescription(const String &s);

      StringList fieldList() const;
      void setText(const String &text);
      void setText(const StringList &fields);

      /*!
       * A UserTextIdentificationFrame is parsed into a PropertyMap as follows:
       * - the key is the frame's description, uppercased
       * - if the description contains '::', only the substring after that
       *   separator is considered as key (compatibility with exfalso)
       * - if the above rules don't yield a valid key (e.g. containing non-ASCII
       *   characters), the returned map will contain an entry "TXXX/<description>"
       *   in its unsupportedData() list.
       * - The values will be copies of the fieldList().
       * - If the description() appears as value in fieldList(), it will be omitted
       *   in the value list, in order to be compatible with TagLib which copies
       *   the description() into the fieldList().
       */
      PropertyMap asProperties() const;

      /*!
       * Searches for the user defined text frame with the description \a description
       * in \a tag.  This returns null if no matching frames were found.
       */
      static UserTextIdentificationFrame *find(Tag *tag, const String &description);

    private:
      UserTextIdentificationFrame(const ByteVector &data, Header *h);
      UserTextIdentificationFrame(const TextIdentificationFrame &);
      UserTextIdentificationFrame &operator=(const UserTextIdentificationFrame &);

      void checkFields();

      class UserTextIdentificationFramePrivate;
      UserTextIdentificationFramePrivate *d;
    };

  }
}
#endif
