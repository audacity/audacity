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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if HAVE_ZLIB
#include <zlib.h>
#endif

#include <bitset>

#include <tdebug.h>
#include <tstringlist.h>

#include "id3v2tag.h"
#include "id3v2frame.h"
#include "id3v2synchdata.h"
#include "tpropertymap.h"
#include "frames/textidentificationframe.h"
#include "frames/urllinkframe.h"
#include "frames/unsynchronizedlyricsframe.h"
#include "frames/commentsframe.h"
#include "frames/uniquefileidentifierframe.h"
#include "frames/unknownframe.h"

using namespace TagLib;
using namespace ID3v2;

class Frame::FramePrivate
{
public:
  FramePrivate() :
    header(0)
    {}

  ~FramePrivate()
  {
    delete header;
  }

  Frame::Header *header;
};

namespace
{
  bool isValidFrameID(const ByteVector &frameID)
  {
    if(frameID.size() != 4)
      return false;

    for(ByteVector::ConstIterator it = frameID.begin(); it != frameID.end(); it++) {
      if( (*it < 'A' || *it > 'Z') && (*it < '0' || *it > '9') ) {
        return false;
      }
    }
    return true;
  }
}

////////////////////////////////////////////////////////////////////////////////
// static methods
////////////////////////////////////////////////////////////////////////////////

TagLib::uint Frame::headerSize()
{
  return Header::size();
}

TagLib::uint Frame::headerSize(uint version)
{
  return Header::size(version);
}

ByteVector Frame::textDelimiter(String::Type t)
{
  ByteVector d = char(0);
  if(t == String::UTF16 || t == String::UTF16BE || t == String::UTF16LE)
    d.append(char(0));
  return d;
}

const String Frame::instrumentPrefix("PERFORMER:");
const String Frame::commentPrefix("COMMENT:");
const String Frame::lyricsPrefix("LYRICS:");
const String Frame::urlPrefix("URL:");

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

Frame *Frame::createTextualFrame(const String &key, const StringList &values) //static
{
  // check if the key is contained in the key<=>frameID mapping
  ByteVector frameID = keyToFrameID(key);
  if(!frameID.isNull()) {
    if(frameID[0] == 'T'){ // text frame
      TextIdentificationFrame *frame = new TextIdentificationFrame(frameID, String::UTF8);
      frame->setText(values);
      return frame;
    } else if((frameID[0] == 'W') && (values.size() == 1)){  // URL frame (not WXXX); support only one value
        UrlLinkFrame* frame = new UrlLinkFrame(frameID);
        frame->setUrl(values.front());
        return frame;
    }
  }
  if(key == "MUSICBRAINZ_TRACKID" && values.size() == 1) {
    UniqueFileIdentifierFrame *frame = new UniqueFileIdentifierFrame("http://musicbrainz.org", values.front().data(String::UTF8));
    return frame;
  }
  // now we check if it's one of the "special" cases:
  // -LYRICS: depending on the number of values, use USLT or TXXX (with description=LYRICS)
  if((key == "LYRICS" || key.startsWith(lyricsPrefix)) && values.size() == 1){
    UnsynchronizedLyricsFrame *frame = new UnsynchronizedLyricsFrame(String::UTF8);
    frame->setDescription(key == "LYRICS" ? key : key.substr(lyricsPrefix.size()));
    frame->setText(values.front());
    return frame;
  }
  // -URL: depending on the number of values, use WXXX or TXXX (with description=URL)
  if((key == "URL" || key.startsWith(urlPrefix)) && values.size() == 1){
    UserUrlLinkFrame *frame = new UserUrlLinkFrame(String::UTF8);
    frame->setDescription(key == "URL" ? key : key.substr(urlPrefix.size()));
    frame->setUrl(values.front());
    return frame;
  }
  // -COMMENT: depending on the number of values, use COMM or TXXX (with description=COMMENT)
  if((key == "COMMENT" || key.startsWith(commentPrefix)) && values.size() == 1){
    CommentsFrame *frame = new CommentsFrame(String::UTF8);
    if (key != "COMMENT"){
      frame->setDescription(key.substr(commentPrefix.size()));
    }
    frame->setText(values.front());
    return frame;
  }
  // if non of the above cases apply, we use a TXXX frame with the key as description
  return new UserTextIdentificationFrame(keyToTXXX(key), values, String::UTF8);
}

Frame::~Frame()
{
  delete d;
}

ByteVector Frame::frameID() const
{
  if(d->header)
    return d->header->frameID();
  else
    return ByteVector::null;
}

TagLib::uint Frame::size() const
{
  if(d->header)
    return d->header->frameSize();
  else
    return 0;
}

void Frame::setData(const ByteVector &data)
{
  parse(data);
}

void Frame::setText(const String &)
{

}

ByteVector Frame::render() const
{
  ByteVector fieldData = renderFields();
  d->header->setFrameSize(fieldData.size());
  ByteVector headerData = d->header->render();

  return headerData + fieldData;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

Frame::Frame(const ByteVector &data)
{
  d = new FramePrivate;
  d->header = new Header(data);
}

Frame::Frame(Header *h)
{
  d = new FramePrivate;
  d->header = h;
}

Frame::Header *Frame::header() const
{
  return d->header;
}

void Frame::setHeader(Header *h, bool deleteCurrent)
{
  if(deleteCurrent)
    delete d->header;

  d->header = h;
}

void Frame::parse(const ByteVector &data)
{
  if(d->header)
    d->header->setData(data);
  else
    d->header = new Header(data);

  parseFields(fieldData(data));
}

ByteVector Frame::fieldData(const ByteVector &frameData) const
{
  uint headerSize = Header::size(d->header->version());

  uint frameDataOffset = headerSize;
  uint frameDataLength = size();

  if(d->header->compression() || d->header->dataLengthIndicator()) {
    frameDataLength = SynchData::toUInt(frameData.mid(headerSize, 4));
    frameDataOffset += 4;
  }

#if HAVE_ZLIB
  if(d->header->compression() &&
     !d->header->encryption())
  {
    ByteVector data(frameDataLength);
    uLongf uLongTmp = frameDataLength;
    ::uncompress((Bytef *) data.data(),
                 (uLongf *) &uLongTmp,
                 (Bytef *) frameData.data() + frameDataOffset,
                 size());
    return data;
  }
  else
#endif
    return frameData.mid(frameDataOffset, frameDataLength);
}

String Frame::readStringField(const ByteVector &data, String::Type encoding, int *position)
{
  int start = 0;

  if(!position)
    position = &start;

  ByteVector delimiter = textDelimiter(encoding);

  int end = data.find(delimiter, *position, delimiter.size());

  if(end < *position)
    return String::null;

  String str;
  if(encoding == String::Latin1)
    str = Tag::latin1StringHandler()->parse(data.mid(*position, end - *position));
  else
    str = String(data.mid(*position, end - *position), encoding);

  *position = end + delimiter.size();

  return str;
}

String::Type Frame::checkEncoding(const StringList &fields, String::Type encoding) // static
{
  return checkEncoding(fields, encoding, 4);
}

String::Type Frame::checkEncoding(const StringList &fields, String::Type encoding, uint version) // static
{
  if((encoding == String::UTF8 || encoding == String::UTF16BE) && version != 4)
    return String::UTF16;

  if(encoding != String::Latin1)
    return encoding;

  for(StringList::ConstIterator it = fields.begin(); it != fields.end(); ++it) {
    if(!(*it).isLatin1()) {
      if(version == 4) {
        debug("Frame::checkEncoding() -- Rendering using UTF8.");
        return String::UTF8;
      }
      else {
        debug("Frame::checkEncoding() -- Rendering using UTF16.");
        return String::UTF16;
      }
    }
  }

  return String::Latin1;
}

String::Type Frame::checkTextEncoding(const StringList &fields, String::Type encoding) const
{
  return checkEncoding(fields, encoding, header()->version());
}

static const TagLib::uint frameTranslationSize = 51;
static const char *frameTranslation[][2] = {
  // Text information frames
  { "TALB", "ALBUM"},
  { "TBPM", "BPM" },
  { "TCOM", "COMPOSER" },
  { "TCON", "GENRE" },
  { "TCOP", "COPYRIGHT" },
  { "TDEN", "ENCODINGTIME" },
  { "TDLY", "PLAYLISTDELAY" },
  { "TDOR", "ORIGINALDATE" },
  { "TDRC", "DATE" },
  // { "TRDA", "DATE" }, // id3 v2.3, replaced by TDRC in v2.4
  // { "TDAT", "DATE" }, // id3 v2.3, replaced by TDRC in v2.4
  // { "TYER", "DATE" }, // id3 v2.3, replaced by TDRC in v2.4
  // { "TIME", "DATE" }, // id3 v2.3, replaced by TDRC in v2.4
  { "TDRL", "RELEASEDATE" },
  { "TDTG", "TAGGINGDATE" },
  { "TENC", "ENCODEDBY" },
  { "TEXT", "LYRICIST" },
  { "TFLT", "FILETYPE" },
  //{ "TIPL", "INVOLVEDPEOPLE" }, handled separately
  { "TIT1", "CONTENTGROUP" },
  { "TIT2", "TITLE"},
  { "TIT3", "SUBTITLE" },
  { "TKEY", "INITIALKEY" },
  { "TLAN", "LANGUAGE" },
  { "TLEN", "LENGTH" },
  //{ "TMCL", "MUSICIANCREDITS" }, handled separately
  { "TMED", "MEDIA" },
  { "TMOO", "MOOD" },
  { "TOAL", "ORIGINALALBUM" },
  { "TOFN", "ORIGINALFILENAME" },
  { "TOLY", "ORIGINALLYRICIST" },
  { "TOPE", "ORIGINALARTIST" },
  { "TOWN", "OWNER" },
  { "TPE1", "ARTIST"},
  { "TPE2", "ALBUMARTIST" }, // id3's spec says 'PERFORMER', but most programs use 'ALBUMARTIST'
  { "TPE3", "CONDUCTOR" },
  { "TPE4", "REMIXER" }, // could also be ARRANGER
  { "TPOS", "DISCNUMBER" },
  { "TPRO", "PRODUCEDNOTICE" },
  { "TPUB", "LABEL" },
  { "TRCK", "TRACKNUMBER" },
  { "TRSN", "RADIOSTATION" },
  { "TRSO", "RADIOSTATIONOWNER" },
  { "TSOA", "ALBUMSORT" },
  { "TSOP", "ARTISTSORT" },
  { "TSOT", "TITLESORT" },
  { "TSO2", "ALBUMARTISTSORT" }, // non-standard, used by iTunes
  { "TSRC", "ISRC" },
  { "TSSE", "ENCODING" },
  // URL frames
  { "WCOP", "COPYRIGHTURL" },
  { "WOAF", "FILEWEBPAGE" },
  { "WOAR", "ARTISTWEBPAGE" },
  { "WOAS", "AUDIOSOURCEWEBPAGE" },
  { "WORS", "RADIOSTATIONWEBPAGE" },
  { "WPAY", "PAYMENTWEBPAGE" },
  { "WPUB", "PUBLISHERWEBPAGE" },
  //{ "WXXX", "URL"}, handled specially
  // Other frames
  { "COMM", "COMMENT" },
  //{ "USLT", "LYRICS" }, handled specially
};

static const TagLib::uint txxxFrameTranslationSize = 8;
static const char *txxxFrameTranslation[][2] = {
  { "MusicBrainz Album Id", "MUSICBRAINZ_ALBUMID" },
  { "MusicBrainz Artist Id", "MUSICBRAINZ_ARTISTID" },
  { "MusicBrainz Album Artist Id", "MUSICBRAINZ_ALBUMARTISTID" },
  { "MusicBrainz Release Group Id", "MUSICBRAINZ_RELEASEGROUPID" },
  { "MusicBrainz Work Id", "MUSICBRAINZ_WORKID" },
  { "Acoustid Id", "ACOUSTID_ID" },
  { "Acoustid Fingerprint", "ACOUSTID_FINGERPRINT" },
  { "MusicIP PUID", "MUSICIP_PUID" },
};

Map<ByteVector, String> &idMap()
{
  static Map<ByteVector, String> m;
  if(m.isEmpty())
    for(size_t i = 0; i < frameTranslationSize; ++i)
      m[frameTranslation[i][0]] = frameTranslation[i][1];
  return m;
}

Map<String, String> &txxxMap()
{
  static Map<String, String> m;
  if(m.isEmpty()) {
    for(size_t i = 0; i < txxxFrameTranslationSize; ++i) {
      String key = String(txxxFrameTranslation[i][0]).upper();
      m[key] = txxxFrameTranslation[i][1];
    }
  }
  return m;
}

// list of deprecated frames and their successors
static const TagLib::uint deprecatedFramesSize = 4;
static const char *deprecatedFrames[][2] = {
  {"TRDA", "TDRC"}, // 2.3 -> 2.4 (http://en.wikipedia.org/wiki/ID3)
  {"TDAT", "TDRC"}, // 2.3 -> 2.4
  {"TYER", "TDRC"}, // 2.3 -> 2.4
  {"TIME", "TDRC"}, // 2.3 -> 2.4
};

Map<ByteVector,ByteVector> &deprecationMap()
{
  static Map<ByteVector,ByteVector> depMap;
  if(depMap.isEmpty())
    for(TagLib::uint i = 0; i < deprecatedFramesSize; ++i)
      depMap[deprecatedFrames[i][0]] = deprecatedFrames[i][1];
  return depMap;
}

String Frame::frameIDToKey(const ByteVector &id)
{
  Map<ByteVector, String> &m = idMap();
  if(m.contains(id))
    return m[id];
  if(deprecationMap().contains(id))
    return m[deprecationMap()[id]];
  return String::null;
}

ByteVector Frame::keyToFrameID(const String &s)
{
  static Map<String, ByteVector> m;
  if(m.isEmpty())
    for(size_t i = 0; i < frameTranslationSize; ++i)
      m[frameTranslation[i][1]] = frameTranslation[i][0];
  if(m.contains(s.upper()))
    return m[s];
  return ByteVector::null;
}

String Frame::txxxToKey(const String &description)
{
  Map<String, String> &m = txxxMap();
  String d = description.upper();
  if(m.contains(d))
    return m[d];
  return d;
}

String Frame::keyToTXXX(const String &s)
{
  static Map<String, String> m;
  if(m.isEmpty())
    for(size_t i = 0; i < txxxFrameTranslationSize; ++i)
      m[txxxFrameTranslation[i][1]] = txxxFrameTranslation[i][0];
  if(m.contains(s.upper()))
    return m[s];
  return s;
}

PropertyMap Frame::asProperties() const
{
  if(dynamic_cast< const UnknownFrame *>(this)) {
    PropertyMap m;
    m.unsupportedData().append("UNKNOWN/" + frameID());
    return m;
  }
  const ByteVector &id = frameID();
  // workaround until this function is virtual
  if(id == "TXXX")
    return dynamic_cast< const UserTextIdentificationFrame* >(this)->asProperties();
  else if(id[0] == 'T')
    return dynamic_cast< const TextIdentificationFrame* >(this)->asProperties();
  else if(id == "WXXX")
    return dynamic_cast< const UserUrlLinkFrame* >(this)->asProperties();
  else if(id[0] == 'W')
    return dynamic_cast< const UrlLinkFrame* >(this)->asProperties();
  else if(id == "COMM")
    return dynamic_cast< const CommentsFrame* >(this)->asProperties();
  else if(id == "USLT")
    return dynamic_cast< const UnsynchronizedLyricsFrame* >(this)->asProperties();
  else if(id == "UFID")
    return dynamic_cast< const UniqueFileIdentifierFrame* >(this)->asProperties();
  PropertyMap m;
  m.unsupportedData().append(id);
  return m;
}

void Frame::splitProperties(const PropertyMap &original, PropertyMap &singleFrameProperties,
          PropertyMap &tiplProperties, PropertyMap &tmclProperties)
{

  singleFrameProperties.clear();
  tiplProperties.clear();
  tmclProperties.clear();
  for(PropertyMap::ConstIterator it = original.begin(); it != original.end(); ++it) {
    if(TextIdentificationFrame::involvedPeopleMap().contains(it->first))
      tiplProperties.insert(it->first, it->second);
    else if(it->first.startsWith(TextIdentificationFrame::instrumentPrefix))
      tmclProperties.insert(it->first, it->second);
    else
      singleFrameProperties.insert(it->first, it->second);
  }
}

////////////////////////////////////////////////////////////////////////////////
// Frame::Header class
////////////////////////////////////////////////////////////////////////////////

class Frame::Header::HeaderPrivate
{
public:
  HeaderPrivate() :
    frameSize(0),
    version(4),
    tagAlterPreservation(false),
    fileAlterPreservation(false),
    readOnly(false),
    groupingIdentity(false),
    compression(false),
    encryption(false),
    unsynchronisation(false),
    dataLengthIndicator(false)
    {}

  ByteVector frameID;
  uint frameSize;
  uint version;

  // flags

  bool tagAlterPreservation;
  bool fileAlterPreservation;
  bool readOnly;
  bool groupingIdentity;
  bool compression;
  bool encryption;
  bool unsynchronisation;
  bool dataLengthIndicator;
};

////////////////////////////////////////////////////////////////////////////////
// static members (Frame::Header)
////////////////////////////////////////////////////////////////////////////////

TagLib::uint Frame::Header::size()
{
  return size(4);
}

TagLib::uint Frame::Header::size(uint version)
{
  switch(version) {
  case 0:
  case 1:
  case 2:
    return 6;
  case 3:
  case 4:
  default:
    return 10;
  }
}

////////////////////////////////////////////////////////////////////////////////
// public members (Frame::Header)
////////////////////////////////////////////////////////////////////////////////

Frame::Header::Header(const ByteVector &data, bool synchSafeInts)
{
  d = new HeaderPrivate;
  setData(data, synchSafeInts);
}

Frame::Header::Header(const ByteVector &data, uint version)
{
  d = new HeaderPrivate;
  setData(data, version);
}

Frame::Header::~Header()
{
  delete d;
}

void Frame::Header::setData(const ByteVector &data, bool synchSafeInts)
{
  setData(data, uint(synchSafeInts ? 4 : 3));
}

void Frame::Header::setData(const ByteVector &data, uint version)
{
  d->version = version;

  switch(version) {
  case 0:
  case 1:
  case 2:
  {
    // ID3v2.2

    if(data.size() < 3) {
      debug("You must at least specify a frame ID.");
      return;
    }

    // Set the frame ID -- the first three bytes

    d->frameID = data.mid(0, 3);

    // If the full header information was not passed in, do not continue to the
    // steps to parse the frame size and flags.

    if(data.size() < 6) {
      d->frameSize = 0;
      return;
    }

    d->frameSize = data.toUInt(3, 3, true);

    break;
  }
  case 3:
  {
    // ID3v2.3

    if(data.size() < 4) {
      debug("You must at least specify a frame ID.");
      return;
    }

    // Set the frame ID -- the first four bytes

    d->frameID = data.mid(0, 4);

    // If the full header information was not passed in, do not continue to the
    // steps to parse the frame size and flags.

    if(data.size() < 10) {
      d->frameSize = 0;
      return;
    }

    // Set the size -- the frame size is the four bytes starting at byte four in
    // the frame header (structure 4)

    d->frameSize = data.toUInt(4U);

    { // read the first byte of flags
      std::bitset<8> flags(data[8]);
      d->tagAlterPreservation  = flags[7]; // (structure 3.3.1.a)
      d->fileAlterPreservation = flags[6]; // (structure 3.3.1.b)
      d->readOnly              = flags[5]; // (structure 3.3.1.c)
    }

    { // read the second byte of flags
      std::bitset<8> flags(data[9]);
      d->compression         = flags[7]; // (structure 3.3.1.i)
      d->encryption          = flags[6]; // (structure 3.3.1.j)
      d->groupingIdentity    = flags[5]; // (structure 3.3.1.k)
    }
    break;
  }
  case 4:
  default:
  {
    // ID3v2.4

    if(data.size() < 4) {
      debug("You must at least specify a frame ID.");
      return;
    }

    // Set the frame ID -- the first four bytes

    d->frameID = data.mid(0, 4);

    // If the full header information was not passed in, do not continue to the
    // steps to parse the frame size and flags.

    if(data.size() < 10) {
      d->frameSize = 0;
      return;
    }

    // Set the size -- the frame size is the four bytes starting at byte four in
    // the frame header (structure 4)

    d->frameSize = SynchData::toUInt(data.mid(4, 4));
#ifndef NO_ITUNES_HACKS
    // iTunes writes v2.4 tags with v2.3-like frame sizes
    if(d->frameSize > 127) {
      if(!isValidFrameID(data.mid(d->frameSize + 10, 4))) {
        unsigned int uintSize = data.toUInt(4U);
        if(isValidFrameID(data.mid(uintSize + 10, 4))) {
          d->frameSize = uintSize;
        }
      }
    }
#endif

    { // read the first byte of flags
      std::bitset<8> flags(data[8]);
      d->tagAlterPreservation  = flags[6]; // (structure 4.1.1.a)
      d->fileAlterPreservation = flags[5]; // (structure 4.1.1.b)
      d->readOnly              = flags[4]; // (structure 4.1.1.c)
    }

    { // read the second byte of flags
      std::bitset<8> flags(data[9]);
      d->groupingIdentity    = flags[6]; // (structure 4.1.2.h)
      d->compression         = flags[3]; // (structure 4.1.2.k)
      d->encryption          = flags[2]; // (structure 4.1.2.m)
      d->unsynchronisation   = flags[1]; // (structure 4.1.2.n)
      d->dataLengthIndicator = flags[0]; // (structure 4.1.2.p)
    }
    break;
  }
  }
}

ByteVector Frame::Header::frameID() const
{
  return d->frameID;
}

void Frame::Header::setFrameID(const ByteVector &id)
{
  d->frameID = id.mid(0, 4);
}

TagLib::uint Frame::Header::frameSize() const
{
  return d->frameSize;
}

void Frame::Header::setFrameSize(uint size)
{
  d->frameSize = size;
}

TagLib::uint Frame::Header::version() const
{
  return d->version;
}

void Frame::Header::setVersion(TagLib::uint version)
{
  d->version = version;
}

bool Frame::Header::tagAlterPreservation() const
{
  return d->tagAlterPreservation;
}

void Frame::Header::setTagAlterPreservation(bool preserve)
{
  d->tagAlterPreservation = preserve;
}

bool Frame::Header::fileAlterPreservation() const
{
  return d->fileAlterPreservation;
}

bool Frame::Header::readOnly() const
{
  return d->readOnly;
}

bool Frame::Header::groupingIdentity() const
{
  return d->groupingIdentity;
}

bool Frame::Header::compression() const
{
  return d->compression;
}

bool Frame::Header::encryption() const
{
  return d->encryption;
}

bool Frame::Header::unsycronisation() const
{
  return unsynchronisation();
}

bool Frame::Header::unsynchronisation() const
{
  return d->unsynchronisation;
}

bool Frame::Header::dataLengthIndicator() const
{
  return d->dataLengthIndicator;
}

ByteVector Frame::Header::render() const
{
  ByteVector flags(2, char(0)); // just blank for the moment

  ByteVector v = d->frameID +
    (d->version == 3
      ? ByteVector::fromUInt(d->frameSize)
      : SynchData::fromUInt(d->frameSize)) +
    flags;

  return v;
}

bool Frame::Header::frameAlterPreservation() const
{
  return fileAlterPreservation();
}
