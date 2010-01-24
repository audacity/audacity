/***************************************************************************
    copyright            : (C) 2006 by Lukáš Lalinský
    email                : lalinsky@gmail.com

    copyright            : (C) 2004 by Allan Sandfeld Jensen
    email                : kde@carewolf.org
                           (original MPC implementation)
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
#include <tagunion.h>

#include "trueaudiofile.h"
#include "id3v1tag.h"
#include "id3v2tag.h"
#include "id3v2header.h"

using namespace TagLib;

namespace
{
  enum { ID3v2Index = 0, ID3v1Index = 1 };
}

class TrueAudio::File::FilePrivate
{
public:
  FilePrivate(const ID3v2::FrameFactory *frameFactory = ID3v2::FrameFactory::instance()) :
    ID3v2FrameFactory(frameFactory),
    ID3v2Location(-1),
    ID3v2OriginalSize(0),
    ID3v1Location(-1),
    properties(0),
    scanned(false),
    hasID3v1(false),
    hasID3v2(false) {}

  ~FilePrivate()
  {
    delete properties;
  }

  const ID3v2::FrameFactory *ID3v2FrameFactory;
  long ID3v2Location;
  uint ID3v2OriginalSize;

  long ID3v1Location;

  TagUnion tag;

  Properties *properties;
  bool scanned;

  // These indicate whether the file *on disk* has these tags, not if
  // this data structure does.  This is used in computing offsets.

  bool hasID3v1;
  bool hasID3v2;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

TrueAudio::File::File(FileName file, bool readProperties,
                 Properties::ReadStyle propertiesStyle) : TagLib::File(file)
{
  d = new FilePrivate;
  if(isOpen())
    read(readProperties, propertiesStyle);
}

TrueAudio::File::File(FileName file, ID3v2::FrameFactory *frameFactory,
                 bool readProperties, Properties::ReadStyle propertiesStyle) :
  TagLib::File(file)
{
  d = new FilePrivate(frameFactory);
  if(isOpen())
    read(readProperties, propertiesStyle);
}

TrueAudio::File::~File()
{
  delete d;
}

TagLib::Tag *TrueAudio::File::tag() const
{
  return &d->tag;
}

TrueAudio::Properties *TrueAudio::File::audioProperties() const
{
  return d->properties;
}

void TrueAudio::File::setID3v2FrameFactory(const ID3v2::FrameFactory *factory)
{
  d->ID3v2FrameFactory = factory;
}

bool TrueAudio::File::save()
{
  if(readOnly()) {
    debug("TrueAudio::File::save() -- File is read only.");
    return false;
  }

  // Update ID3v2 tag

  if(ID3v2Tag() && !ID3v2Tag()->isEmpty()) {
    if(!d->hasID3v2) {
      d->ID3v2Location = 0;
      d->ID3v2OriginalSize = 0;
    }
    ByteVector data = ID3v2Tag()->render();
    insert(data, d->ID3v2Location, d->ID3v2OriginalSize);
    d->ID3v1Location -= d->ID3v2OriginalSize - data.size();
    d->ID3v2OriginalSize = data.size();
    d->hasID3v2 = true;
  }
  else if(d->hasID3v2) {
    removeBlock(d->ID3v2Location, d->ID3v2OriginalSize);
    d->ID3v1Location -= d->ID3v2OriginalSize;
    d->ID3v2Location = -1;
    d->ID3v2OriginalSize = 0;
    d->hasID3v2 = false;
  }

  // Update ID3v1 tag

  if(ID3v1Tag() && !ID3v1Tag()->isEmpty()) {
    if(!d->hasID3v1) {
      seek(0, End);
      d->ID3v1Location = tell();
    }
    else
      seek(d->ID3v1Location);
    writeBlock(ID3v1Tag()->render());
    d->hasID3v1 = true;
  }
  else if(d->hasID3v1) {
    removeBlock(d->ID3v1Location, 128);
    d->ID3v1Location = -1;
    d->hasID3v1 = false;
  }

  return true;
}

ID3v1::Tag *TrueAudio::File::ID3v1Tag(bool create)
{
  return d->tag.access<ID3v1::Tag>(ID3v1Index, create);
}

ID3v2::Tag *TrueAudio::File::ID3v2Tag(bool create)
{
  return d->tag.access<ID3v2::Tag>(ID3v2Index, create);
}

void TrueAudio::File::strip(int tags)
{
  if(tags & ID3v1) {
    d->tag.set(ID3v1Index, 0);
    ID3v2Tag(true);
  }

  if(tags & ID3v2) {
    d->tag.set(ID3v2Index, 0);

    if(!ID3v1Tag())
      ID3v2Tag(true);
  }
}


////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void TrueAudio::File::read(bool readProperties, Properties::ReadStyle /* propertiesStyle */)
{
  // Look for an ID3v2 tag

  d->ID3v2Location = findID3v2();

  if(d->ID3v2Location >= 0) {

    d->tag.set(ID3v2Index, new ID3v2::Tag(this, d->ID3v2Location, d->ID3v2FrameFactory));

    d->ID3v2OriginalSize = ID3v2Tag()->header()->completeTagSize();

    if(ID3v2Tag()->header()->tagSize() <= 0)
      d->tag.set(ID3v2Index, 0);
    else
      d->hasID3v2 = true;
  }

  // Look for an ID3v1 tag

  d->ID3v1Location = findID3v1();

  if(d->ID3v1Location >= 0) {
    d->tag.set(ID3v1Index, new ID3v1::Tag(this, d->ID3v1Location));
    d->hasID3v1 = true;
  }

  if(!d->hasID3v1)
    ID3v2Tag(true);

  // Look for TrueAudio metadata

  if(readProperties) {
    if(d->ID3v2Location >= 0) {
      seek(d->ID3v2Location + d->ID3v2OriginalSize);
      d->properties = new Properties(readBlock(TrueAudio::HeaderSize),
                                     length() - d->ID3v2OriginalSize);
    }
    else {
      seek(0);
      d->properties = new Properties(readBlock(TrueAudio::HeaderSize),
                                     length());
    }
  }
}

long TrueAudio::File::findID3v1()
{
  if(!isValid())
    return -1;

  seek(-128, End);
  long p = tell();

  if(readBlock(3) == ID3v1::Tag::fileIdentifier())
    return p;

  return -1;
}

long TrueAudio::File::findID3v2()
{
  if(!isValid())
    return -1;

  seek(0);

  if(readBlock(3) == ID3v2::Header::fileIdentifier())
    return 0;

  return -1;
}
