/**************************************************************************
    copyright            : (C) 2010 by Lukáš Lalinský
    email                : lalinsky@gmail.com
 **************************************************************************/

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

#include <taglib.h>
#include <tdebug.h>
#include "flacpicture.h"

using namespace TagLib;

class FLAC::Picture::PicturePrivate
{
public:
  PicturePrivate() :
    type(FLAC::Picture::Other),
    width(0),
    height(0),
    colorDepth(0),
    numColors(0)
    {}

  Type type;
  String mimeType;
  String description;
  int width;
  int height;
  int colorDepth;
  int numColors;
  ByteVector data;
};

FLAC::Picture::Picture()
{
  d = new PicturePrivate;
}

FLAC::Picture::Picture(const ByteVector &data)
{
  d = new PicturePrivate;
  parse(data);
}

FLAC::Picture::~Picture()
{
  delete d;
}

int FLAC::Picture::code() const
{
  return FLAC::MetadataBlock::Picture;
}

bool FLAC::Picture::parse(const ByteVector &data)
{
  if(data.size() < 32) {
    debug("A picture block must contain at least 5 bytes.");
    return false;
  }

  uint pos = 0;
  d->type = FLAC::Picture::Type(data.toUInt(pos));
  pos += 4;
  uint mimeTypeLength = data.toUInt(pos);
  pos += 4;
  if(pos + mimeTypeLength + 24 > data.size()) {
    debug("Invalid picture block.");
    return false;
  }
  d->mimeType = String(data.mid(pos, mimeTypeLength), String::UTF8);
  pos += mimeTypeLength;
  uint descriptionLength = data.toUInt(pos);
  pos += 4;
  if(pos + descriptionLength + 20 > data.size()) {
    debug("Invalid picture block.");
    return false;
  }
  d->description = String(data.mid(pos, descriptionLength), String::UTF8);
  pos += descriptionLength;
  d->width = data.toUInt(pos);
  pos += 4;
  d->height = data.toUInt(pos);
  pos += 4;
  d->colorDepth = data.toUInt(pos);
  pos += 4;
  d->numColors = data.toUInt(pos);
  pos += 4;
  uint dataLength = data.toUInt(pos);
  pos += 4;
  if(pos + dataLength > data.size()) {
    debug("Invalid picture block.");
    return false;
  }
  d->data = data.mid(pos, dataLength);

  return true;
}

ByteVector FLAC::Picture::render() const
{
  ByteVector result;
  result.append(ByteVector::fromUInt(d->type));
  ByteVector mimeTypeData = d->mimeType.data(String::UTF8);
  result.append(ByteVector::fromUInt(mimeTypeData.size()));
  result.append(mimeTypeData);
  ByteVector descriptionData = d->description.data(String::UTF8);
  result.append(ByteVector::fromUInt(descriptionData.size()));
  result.append(descriptionData);
  result.append(ByteVector::fromUInt(d->width));
  result.append(ByteVector::fromUInt(d->height));
  result.append(ByteVector::fromUInt(d->colorDepth));
  result.append(ByteVector::fromUInt(d->numColors));
  result.append(ByteVector::fromUInt(d->data.size()));
  result.append(d->data);
  return result;
}

FLAC::Picture::Type FLAC::Picture::type() const
{
  return d->type;
}

void FLAC::Picture::setType(FLAC::Picture::Type type)
{
  d->type = type;
}

String FLAC::Picture::mimeType() const
{
  return d->mimeType;
}

void FLAC::Picture::setMimeType(const String &mimeType)
{
  d->mimeType = mimeType;
}

String FLAC::Picture::description() const
{
  return d->description;
}

void FLAC::Picture::setDescription(const String &description)
{
  d->description = description;
}

int FLAC::Picture::width() const
{
  return d->width;
}

void FLAC::Picture::setWidth(int width)
{
  d->width = width;
}

int FLAC::Picture::height() const
{
  return d->height;
}

void FLAC::Picture::setHeight(int height)
{
  d->height = height;
}

int FLAC::Picture::colorDepth() const
{
  return d->colorDepth;
}

void FLAC::Picture::setColorDepth(int colorDepth)
{
  d->colorDepth = colorDepth;
}

int FLAC::Picture::numColors() const
{
  return d->numColors;
}

void FLAC::Picture::setNumColors(int numColors)
{
  d->numColors = numColors;
}

ByteVector FLAC::Picture::data() const
{
  return d->data;
}

void FLAC::Picture::setData(const ByteVector &data)
{
  d->data = data;
}

