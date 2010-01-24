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

#ifndef HAVE_ZLIB
#include <config.h>
#endif

#include <tdebug.h>

#include "id3v2framefactory.h"
#include "id3v2synchdata.h"

#include "frames/attachedpictureframe.h"
#include "frames/commentsframe.h"
#include "frames/relativevolumeframe.h"
#include "frames/textidentificationframe.h"
#include "frames/uniquefileidentifierframe.h"
#include "frames/unknownframe.h"
#include "frames/generalencapsulatedobjectframe.h"
#include "frames/urllinkframe.h"
#include "frames/unsynchronizedlyricsframe.h"
#include "frames/popularimeterframe.h"
#include "frames/privateframe.h"

using namespace TagLib;
using namespace ID3v2;

class FrameFactory::FrameFactoryPrivate
{
public:
  FrameFactoryPrivate() :
    defaultEncoding(String::Latin1),
    useDefaultEncoding(false) {}

  String::Type defaultEncoding;
  bool useDefaultEncoding;

  template <class T> void setTextEncoding(T *frame)
  {
    if(useDefaultEncoding)
      frame->setTextEncoding(defaultEncoding);
  }
};

FrameFactory *FrameFactory::factory = 0;

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

FrameFactory *FrameFactory::instance()
{
  if(!factory)
    factory = new FrameFactory;
  return factory;
}

Frame *FrameFactory::createFrame(const ByteVector &data, bool synchSafeInts) const
{
  return createFrame(data, uint(synchSafeInts ? 4 : 3));
}

Frame *FrameFactory::createFrame(const ByteVector &data, uint version) const
{
  Header tagHeader;
  tagHeader.setMajorVersion(version);
  return createFrame(data, &tagHeader);
}

Frame *FrameFactory::createFrame(const ByteVector &origData, Header *tagHeader) const
{
  ByteVector data = origData;
  uint version = tagHeader->majorVersion();
  Frame::Header *header = new Frame::Header(data, version);
  ByteVector frameID = header->frameID();

  // A quick sanity check -- make sure that the frameID is 4 uppercase Latin1
  // characters.  Also make sure that there is data in the frame.

  if(!frameID.size() == (version < 3 ? 3 : 4) ||
     header->frameSize() <= uint(header->dataLengthIndicator() ? 4 : 0) ||
     header->frameSize() > data.size())
  {
    delete header;
    return 0;
  }

  for(ByteVector::ConstIterator it = frameID.begin(); it != frameID.end(); it++) {
    if( (*it < 'A' || *it > 'Z') && (*it < '1' || *it > '9') ) {
      delete header;
      return 0;
    }
  }

  if(version > 3 && (tagHeader->unsynchronisation() || header->unsynchronisation())) {
    // Data lengths are not part of the encoded data, but since they are synch-safe
    // integers they will be never actually encoded.
    ByteVector frameData = data.mid(Frame::Header::size(version), header->frameSize());
    frameData = SynchData::decode(frameData);
    data = data.mid(0, Frame::Header::size(version)) + frameData;
  }

  // TagLib doesn't mess with encrypted frames, so just treat them
  // as unknown frames.

#if HAVE_ZLIB == 0
  if(header->compression()) {
    debug("Compressed frames are currently not supported.");
    return new UnknownFrame(data, header);
  }
#endif
  if(header->encryption()) {
    debug("Encrypted frames are currently not supported.");
    return new UnknownFrame(data, header);
  }

  if(!updateFrame(header)) {
    header->setTagAlterPreservation(true);
    return new UnknownFrame(data, header);
  }

  // updateFrame() might have updated the frame ID.

  frameID = header->frameID();

  // This is where things get necissarily nasty.  Here we determine which
  // Frame subclass (or if none is found simply an Frame) based
  // on the frame ID.  Since there are a lot of possibilities, that means
  // a lot of if blocks.

  // Text Identification (frames 4.2)

  if(frameID.startsWith("T")) {

    TextIdentificationFrame *f = frameID != "TXXX"
      ? new TextIdentificationFrame(data, header)
      : new UserTextIdentificationFrame(data, header);

    d->setTextEncoding(f);

    if(frameID == "TCON")
      updateGenre(f);

    return f;
  }

  // Comments (frames 4.10)

  if(frameID == "COMM") {
    CommentsFrame *f = new CommentsFrame(data, header);
    d->setTextEncoding(f);
    return f;
  }

  // Attached Picture (frames 4.14)

  if(frameID == "APIC") {
    AttachedPictureFrame *f = new AttachedPictureFrame(data, header);
    d->setTextEncoding(f);
    return f;
  }

  // Relative Volume Adjustment (frames 4.11)

  if(frameID == "RVA2")
    return new RelativeVolumeFrame(data, header);

  // Unique File Identifier (frames 4.1)

  if(frameID == "UFID")
    return new UniqueFileIdentifierFrame(data, header);

  // General Encapsulated Object (frames 4.15)

  if(frameID == "GEOB") {
    GeneralEncapsulatedObjectFrame *f = new GeneralEncapsulatedObjectFrame(data, header);
    d->setTextEncoding(f);
    return f;
  }

  // URL link (frames 4.3)

  if(frameID.startsWith("W")) {
    if(frameID != "WXXX") {
      return new UrlLinkFrame(data, header);
    }
    else {
      UserUrlLinkFrame *f = new UserUrlLinkFrame(data, header);
      d->setTextEncoding(f);
      return f;
    }
  }

  // Unsynchronized lyric/text transcription (frames 4.8)

  if(frameID == "USLT") {
    UnsynchronizedLyricsFrame *f = new UnsynchronizedLyricsFrame(data, header);
    if(d->useDefaultEncoding)
      f->setTextEncoding(d->defaultEncoding);
    return f;
  }

  // Popularimeter (frames 4.17)

  if(frameID == "POPM")
    return new PopularimeterFrame(data, header);

  // Private (frames 4.27)

  if(frameID == "PRIV")
    return new PrivateFrame(data, header);

  return new UnknownFrame(data, header);
}

String::Type FrameFactory::defaultTextEncoding() const
{
  return d->defaultEncoding;
}

void FrameFactory::setDefaultTextEncoding(String::Type encoding)
{
  d->useDefaultEncoding = true;
  d->defaultEncoding = encoding;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

FrameFactory::FrameFactory()
{
  d = new FrameFactoryPrivate;
}

FrameFactory::~FrameFactory()
{
  delete d;
}

bool FrameFactory::updateFrame(Frame::Header *header) const
{
  TagLib::ByteVector frameID = header->frameID();

  switch(header->version()) {

  case 2: // ID3v2.2
  {
    if(frameID == "CRM" ||
       frameID == "EQU" ||
       frameID == "LNK" ||
       frameID == "RVA" ||
       frameID == "TIM" ||
       frameID == "TSI")
    {
      debug("ID3v2.4 no longer supports the frame type " + String(frameID) +
            ".  It will be discarded from the tag.");
      return false;
    }

    // ID3v2.2 only used 3 bytes for the frame ID, so we need to convert all of
    // the frames to their 4 byte ID3v2.4 equivalent.

    convertFrame("BUF", "RBUF", header);
    convertFrame("CNT", "PCNT", header);
    convertFrame("COM", "COMM", header);
    convertFrame("CRA", "AENC", header);
    convertFrame("ETC", "ETCO", header);
    convertFrame("GEO", "GEOB", header);
    convertFrame("IPL", "TIPL", header);
    convertFrame("MCI", "MCDI", header);
    convertFrame("MLL", "MLLT", header);
    convertFrame("PIC", "APIC", header);
    convertFrame("POP", "POPM", header);
    convertFrame("REV", "RVRB", header);
    convertFrame("SLT", "SYLT", header);
    convertFrame("STC", "SYTC", header);
    convertFrame("TAL", "TALB", header);
    convertFrame("TBP", "TBPM", header);
    convertFrame("TCM", "TCOM", header);
    convertFrame("TCO", "TCON", header);
    convertFrame("TCR", "TCOP", header);
    convertFrame("TDA", "TDRC", header);
    convertFrame("TDY", "TDLY", header);
    convertFrame("TEN", "TENC", header);
    convertFrame("TFT", "TFLT", header);
    convertFrame("TKE", "TKEY", header);
    convertFrame("TLA", "TLAN", header);
    convertFrame("TLE", "TLEN", header);
    convertFrame("TMT", "TMED", header);
    convertFrame("TOA", "TOAL", header);
    convertFrame("TOF", "TOFN", header);
    convertFrame("TOL", "TOLY", header);
    convertFrame("TOR", "TDOR", header);
    convertFrame("TOT", "TOAL", header);
    convertFrame("TP1", "TPE1", header);
    convertFrame("TP2", "TPE2", header);
    convertFrame("TP3", "TPE3", header);
    convertFrame("TP4", "TPE4", header);
    convertFrame("TPA", "TPOS", header);
    convertFrame("TPB", "TPUB", header);
    convertFrame("TRC", "TSRC", header);
    convertFrame("TRD", "TDRC", header);
    convertFrame("TRK", "TRCK", header);
    convertFrame("TSS", "TSSE", header);
    convertFrame("TT1", "TIT1", header);
    convertFrame("TT2", "TIT2", header);
    convertFrame("TT3", "TIT3", header);
    convertFrame("TXT", "TOLY", header);
    convertFrame("TXX", "TXXX", header);
    convertFrame("TYE", "TDRC", header);
    convertFrame("UFI", "UFID", header);
    convertFrame("ULT", "USLT", header);
    convertFrame("WAF", "WOAF", header);
    convertFrame("WAR", "WOAR", header);
    convertFrame("WAS", "WOAS", header);
    convertFrame("WCM", "WCOM", header);
    convertFrame("WCP", "WCOP", header);
    convertFrame("WPB", "WPUB", header);
    convertFrame("WXX", "WXXX", header);

    break;
  }

  case 3: // ID3v2.3
  {
    if(frameID == "EQUA" ||
       frameID == "RVAD" ||
       frameID == "TIME" ||
       frameID == "TRDA" ||
       frameID == "TSIZ" ||
       frameID == "TDAT")
    {
      debug("ID3v2.4 no longer supports the frame type " + String(frameID) +
            ".  It will be discarded from the tag.");
      return false;
    }

    convertFrame("TORY", "TDOR", header);
    convertFrame("TYER", "TDRC", header);

    break;
  }

  default:

    // This should catch a typo that existed in TagLib up to and including
    // version 1.1 where TRDC was used for the year rather than TDRC.

    convertFrame("TRDC", "TDRC", header);
    break;
  }

  return true;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void FrameFactory::convertFrame(const char *from, const char *to,
                                Frame::Header *header) const
{
  if(header->frameID() != from)
    return;

  // debug("ID3v2.4 no longer supports the frame type " + String(from) + "  It has" +
  //       "been converted to the type " + String(to) + ".");

  header->setFrameID(to);
}

void FrameFactory::updateGenre(TextIdentificationFrame *frame) const
{
  StringList fields;
  String s = frame->toString();

  while(s.startsWith("(")) {

    int closing = s.find(")");

    if(closing < 0)
      break;

    fields.append(s.substr(1, closing - 1));

    s = s.substr(closing + 1);
  }

  if(!s.isEmpty())
    fields.append(s);

  if(fields.isEmpty())
    fields.append(String::null);

  frame->setText(fields);
}
