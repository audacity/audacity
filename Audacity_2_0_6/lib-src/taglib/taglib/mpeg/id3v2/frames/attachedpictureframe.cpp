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

#include "attachedpictureframe.h"

#include <tstringlist.h>
#include <tdebug.h>

using namespace TagLib;
using namespace ID3v2;

class AttachedPictureFrame::AttachedPictureFramePrivate
{
public:
  AttachedPictureFramePrivate() : textEncoding(String::Latin1),
                                  type(AttachedPictureFrame::Other) {}

  String::Type textEncoding;
  String mimeType;
  AttachedPictureFrame::Type type;
  String description;
  ByteVector data;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

AttachedPictureFrame::AttachedPictureFrame() : Frame("APIC")
{
  d = new AttachedPictureFramePrivate;
}

AttachedPictureFrame::AttachedPictureFrame(const ByteVector &data) : Frame(data)
{
  d = new AttachedPictureFramePrivate;
  setData(data);
}

AttachedPictureFrame::~AttachedPictureFrame()
{
  delete d;
}

String AttachedPictureFrame::toString() const
{
  String s = "[" + d->mimeType + "]";
  return d->description.isEmpty() ? s : d->description + " " + s;
}

String::Type AttachedPictureFrame::textEncoding() const
{
  return d->textEncoding;
}

void AttachedPictureFrame::setTextEncoding(String::Type t)
{
  d->textEncoding = t;
}

String AttachedPictureFrame::mimeType() const
{
  return d->mimeType;
}

void AttachedPictureFrame::setMimeType(const String &m)
{
  d->mimeType = m;
}

AttachedPictureFrame::Type AttachedPictureFrame::type() const
{
  return d->type;
}

void AttachedPictureFrame::setType(Type t)
{
  d->type = t;
}

String AttachedPictureFrame::description() const
{
  return d->description;
}

void AttachedPictureFrame::setDescription(const String &desc)
{
  d->description = desc;
}

ByteVector AttachedPictureFrame::picture() const
{
  return d->data;
}

void AttachedPictureFrame::setPicture(const ByteVector &p)
{
  d->data = p;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

void AttachedPictureFrame::parseFields(const ByteVector &data)
{
  if(data.size() < 5) {
    debug("A picture frame must contain at least 5 bytes.");
    return;
  }

  d->textEncoding = String::Type(data[0]);

  int pos = 1;

  d->mimeType = readStringField(data, String::Latin1, &pos);
  /* Now we need at least two more bytes available */
  if (uint(pos) + 1 >= data.size()) {
    debug("Truncated picture frame.");
    return;
  }

  d->type = (TagLib::ID3v2::AttachedPictureFrame::Type)data[pos++];
  d->description = readStringField(data, d->textEncoding, &pos);

  d->data = data.mid(pos);
}

ByteVector AttachedPictureFrame::renderFields() const
{
  ByteVector data;

  String::Type encoding = checkTextEncoding(d->description, d->textEncoding);

  data.append(char(encoding));
  data.append(d->mimeType.data(String::Latin1));
  data.append(textDelimiter(String::Latin1));
  data.append(char(d->type));
  data.append(d->description.data(encoding));
  data.append(textDelimiter(encoding));
  data.append(d->data);

  return data;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

AttachedPictureFrame::AttachedPictureFrame(const ByteVector &data, Header *h) : Frame(h)
{
  d = new AttachedPictureFramePrivate;
  parseFields(fieldData(data));
}

////////////////////////////////////////////////////////////////////////////////
// support for ID3v2.2 PIC frames
////////////////////////////////////////////////////////////////////////////////

void AttachedPictureFrameV22::parseFields(const ByteVector &data)
{
  if(data.size() < 5) {
    debug("A picture frame must contain at least 5 bytes.");
    return;
  }

  d->textEncoding = String::Type(data[0]);

  int pos = 1;

  String fixedString = String(data.mid(pos, 3), String::Latin1);
  pos += 3;
  // convert fixed string image type to mime string
  if (fixedString.upper() == "JPG") {
    d->mimeType = "image/jpeg";
  } else if (fixedString.upper() == "PNG") {
    d->mimeType = "image/png";
  } else {
    debug("probably unsupported image type");
    d->mimeType = "image/" + fixedString;
  }

  d->type = (TagLib::ID3v2::AttachedPictureFrame::Type)data[pos++];
  d->description = readStringField(data, d->textEncoding, &pos);

  d->data = data.mid(pos);
}

AttachedPictureFrameV22::AttachedPictureFrameV22(const ByteVector &data, Header *h)
{
  // set v2.2 header to make fieldData work correctly
  setHeader(h, true);

  parseFields(fieldData(data));

  // now set the v2.4 header
  Frame::Header *newHeader = new Frame::Header("APIC");
  newHeader->setFrameSize(h->frameSize());
  setHeader(newHeader, true);
}
