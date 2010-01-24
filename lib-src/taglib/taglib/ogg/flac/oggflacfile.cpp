/***************************************************************************
    copyright            : (C) 2004-2005 by Allan Sandfeld Jensen
    email                : kde@carewolf.org
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

#include <tbytevector.h>
#include <tstring.h>
#include <tdebug.h>

#include <xiphcomment.h>
#include "oggflacfile.h"

using namespace TagLib;
using TagLib::FLAC::Properties;

class Ogg::FLAC::File::FilePrivate
{
public:
  FilePrivate() :
    comment(0),
    properties(0),
    streamStart(0),
    streamLength(0),
    scanned(false),
    hasXiphComment(false),
    commentPacket(0) {}

  ~FilePrivate()
  {
    delete comment;
    delete properties;
  }

  Ogg::XiphComment *comment;

  Properties *properties;
  ByteVector streamInfoData;
  ByteVector xiphCommentData;
  long streamStart;
  long streamLength;
  bool scanned;

  bool hasXiphComment;
  int commentPacket;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

Ogg::FLAC::File::File(FileName file, bool readProperties,
                      Properties::ReadStyle propertiesStyle) : Ogg::File(file)
{
  d = new FilePrivate;
  read(readProperties, propertiesStyle);
}

Ogg::FLAC::File::~File()
{
  delete d;
}

Ogg::XiphComment *Ogg::FLAC::File::tag() const
{
  return d->comment;
}

Properties *Ogg::FLAC::File::audioProperties() const
{
  return d->properties;
}


bool Ogg::FLAC::File::save()
{
  d->xiphCommentData = d->comment->render();

  // Create FLAC metadata-block:

  // Put the size in the first 32 bit (I assume no more than 24 bit are used)

  ByteVector v = ByteVector::fromUInt(d->xiphCommentData.size());

  // Set the type of the metadata-block to be a Xiph / Vorbis comment

  v[0] = 4;

  // Append the comment-data after the 32 bit header

  v.append(d->xiphCommentData);

  // Save the packet at the old spot
  // FIXME: Use padding if size is increasing

  setPacket(d->commentPacket, v);

  return Ogg::File::save();
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void Ogg::FLAC::File::read(bool readProperties, Properties::ReadStyle propertiesStyle)
{
  // Sanity: Check if we really have an Ogg/FLAC file

/*
  ByteVector oggHeader = packet(0);

  if (oggHeader.mid(28,4) != "fLaC") {
    debug("Ogg::FLAC::File::read() -- Not an Ogg/FLAC file");
    setValid(false);
    return;
  }*/

  // Look for FLAC metadata, including vorbis comments

  scan();

  if (!d->scanned) {
    setValid(false);
    return;
  }


  if(d->hasXiphComment)
    d->comment = new Ogg::XiphComment(xiphCommentData());
  else
    d->comment = new Ogg::XiphComment;


  if(readProperties)
    d->properties = new Properties(streamInfoData(), streamLength(), propertiesStyle);
}

ByteVector Ogg::FLAC::File::streamInfoData()
{
  scan();
  return d->streamInfoData;
}

ByteVector Ogg::FLAC::File::xiphCommentData()
{
  scan();
  return d->xiphCommentData;
}

long Ogg::FLAC::File::streamLength()
{
  scan();
  return d->streamLength;
}

void Ogg::FLAC::File::scan()
{
  // Scan the metadata pages

  if(d->scanned)
    return;

  if(!isValid())
    return;

  int ipacket = 0;
  long overhead = 0;

  ByteVector metadataHeader = packet(ipacket);
  if(metadataHeader.isNull())
    return;

  ByteVector header;

  if (!metadataHeader.startsWith("fLaC"))  {
    // FLAC 1.1.2+
    if (metadataHeader.mid(1,4) != "FLAC") return;

    if (metadataHeader[5] != 1) return; // not version 1

    metadataHeader = metadataHeader.mid(13);
  }
  else {
    // FLAC 1.1.0 & 1.1.1
    metadataHeader = packet(++ipacket);

    if(metadataHeader.isNull())
      return;

  }

  header = metadataHeader.mid(0,4);
  // Header format (from spec):
  // <1> Last-metadata-block flag
  // <7> BLOCK_TYPE
  //    0 : STREAMINFO
  //    1 : PADDING
  //    ..
  //    4 : VORBIS_COMMENT
  //    ..
  // <24> Length of metadata to follow

  char blockType = header[0] & 0x7f;
  bool lastBlock = (header[0] & 0x80) != 0;
  uint length = header.mid(1, 3).toUInt();
  overhead += length;

  // Sanity: First block should be the stream_info metadata

  if(blockType != 0) {
    debug("Ogg::FLAC::File::scan() -- Invalid Ogg/FLAC stream");
    return;
  }

  d->streamInfoData = metadataHeader.mid(4,length);

  // Search through the remaining metadata

  while(!lastBlock) {
    metadataHeader = packet(++ipacket);

    if(metadataHeader.isNull())
      return;

    header = metadataHeader.mid(0, 4);
    blockType = header[0] & 0x7f;
    lastBlock = (header[0] & 0x80) != 0;
    length = header.mid(1, 3).toUInt();
    overhead += length;

    if(blockType == 1) {
      // debug("Ogg::FLAC::File::scan() -- Padding found");
    }
    else if(blockType == 4) {
      // debug("Ogg::FLAC::File::scan() -- Vorbis-comments found");
      d->xiphCommentData = metadataHeader.mid(4, length);
      d->hasXiphComment = true;
      d->commentPacket = ipacket;
    }
    else if(blockType > 5)
      debug("Ogg::FLAC::File::scan() -- Unknown metadata block");

  }

  // End of metadata, now comes the datastream
  d->streamStart = overhead;
  d->streamLength = File::length() - d->streamStart;

  d->scanned = true;
}
