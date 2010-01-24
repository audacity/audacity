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

#include "wavpackfile.h"
#include "id3v1tag.h"
#include "id3v2header.h"
#include "apetag.h"
#include "apefooter.h"

using namespace TagLib;

namespace
{
  enum { APEIndex, ID3v1Index };
}

class WavPack::File::FilePrivate
{
public:
  FilePrivate() :
    APELocation(-1),
    APESize(0),
    ID3v1Location(-1),
    properties(0),
    scanned(false),
    hasAPE(false),
    hasID3v1(false) {}

  ~FilePrivate()
  {
    delete properties;
  }

  long APELocation;
  uint APESize;

  long ID3v1Location;

  TagUnion tag;

  Properties *properties;
  bool scanned;

  // These indicate whether the file *on disk* has these tags, not if
  // this data structure does.  This is used in computing offsets.

  bool hasAPE;
  bool hasID3v1;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

WavPack::File::File(FileName file, bool readProperties,
                Properties::ReadStyle propertiesStyle) : TagLib::File(file)
{
  d = new FilePrivate;
  read(readProperties, propertiesStyle);
}

WavPack::File::~File()
{
  delete d;
}

TagLib::Tag *WavPack::File::tag() const
{
  return &d->tag;
}

WavPack::Properties *WavPack::File::audioProperties() const
{
  return d->properties;
}

bool WavPack::File::save()
{
  if(readOnly()) {
    debug("WavPack::File::save() -- File is read only.");
    return false;
  }

  // Update ID3v1 tag

  if(ID3v1Tag()) {
    if(d->hasID3v1) {
      seek(d->ID3v1Location);
      writeBlock(ID3v1Tag()->render());
    }
    else {
      seek(0, End);
      d->ID3v1Location = tell();
      writeBlock(ID3v1Tag()->render());
      d->hasID3v1 = true;
    }
  }
  else {
    if(d->hasID3v1) {
      removeBlock(d->ID3v1Location, 128);
      d->hasID3v1 = false;
      if(d->hasAPE) {
        if(d->APELocation > d->ID3v1Location)
          d->APELocation -= 128;
      }
    }
  }

  // Update APE tag

  if(APETag()) {
    if(d->hasAPE)
      insert(APETag()->render(), d->APELocation, d->APESize);
    else {
      if(d->hasID3v1)  {
        insert(APETag()->render(), d->ID3v1Location, 0);
        d->APESize = APETag()->footer()->completeTagSize();
        d->hasAPE = true;
        d->APELocation = d->ID3v1Location;
        d->ID3v1Location += d->APESize;
      }
      else {
        seek(0, End);
        d->APELocation = tell();
        writeBlock(APETag()->render());
        d->APESize = APETag()->footer()->completeTagSize();
        d->hasAPE = true;
      }
    }
  }
  else {
    if(d->hasAPE) {
      removeBlock(d->APELocation, d->APESize);
      d->hasAPE = false;
      if(d->hasID3v1) {
        if(d->ID3v1Location > d->APELocation) {
          d->ID3v1Location -= d->APESize;
        }
      }
    }
  }

   return true;
}

ID3v1::Tag *WavPack::File::ID3v1Tag(bool create)
{
  return d->tag.access<ID3v1::Tag>(ID3v1Index, create);
}

APE::Tag *WavPack::File::APETag(bool create)
{
  return d->tag.access<APE::Tag>(APEIndex, create);
}

void WavPack::File::strip(int tags)
{
  if(tags & ID3v1) {
    d->tag.set(ID3v1Index, 0);
    APETag(true);
  }

  if(tags & APE) {
    d->tag.set(APEIndex, 0);

    if(!ID3v1Tag())
      APETag(true);
  }
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void WavPack::File::read(bool readProperties, Properties::ReadStyle /* propertiesStyle */)
{
  // Look for an ID3v1 tag

  d->ID3v1Location = findID3v1();

  if(d->ID3v1Location >= 0) {
    d->tag.set(ID3v1Index, new ID3v1::Tag(this, d->ID3v1Location));
    d->hasID3v1 = true;
  }

  // Look for an APE tag

  d->APELocation = findAPE();

  if(d->APELocation >= 0) {
    d->tag.set(APEIndex, new APE::Tag(this, d->APELocation));
    d->APESize = APETag()->footer()->completeTagSize();
    d->APELocation = d->APELocation + APETag()->footer()->size() - d->APESize;
    d->hasAPE = true;
  }

  if(!d->hasID3v1)
    APETag(true);

  // Look for WavPack audio properties

  if(readProperties) {
    seek(0);
    d->properties = new Properties(readBlock(WavPack::HeaderSize),
                                   length() - d->APESize);
  }
}

long WavPack::File::findAPE()
{
  if(!isValid())
    return -1;

  if(d->hasID3v1)
    seek(-160, End);
  else
    seek(-32, End);

  long p = tell();

  if(readBlock(8) == APE::Tag::fileIdentifier())
    return p;

  return -1;
}

long WavPack::File::findID3v1()
{
  if(!isValid())
    return -1;

  seek(-128, End);
  long p = tell();

  if(readBlock(3) == ID3v1::Tag::fileIdentifier())
    return p;

  return -1;
}
