/***************************************************************************
    copyright            : (C) 2002 - 2008 by Scott Wheeler
    email                : wheeler@kde.org
    copyright            : (C) 2006 by Urs Fleisch
    email                : ufleisch@users.sourceforge.net
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

#include "urllinkframe.h"
#include <tdebug.h>
#include <tstringlist.h>

using namespace TagLib;
using namespace ID3v2;

class UrlLinkFrame::UrlLinkFramePrivate
{
public:
  String url;
};

class UserUrlLinkFrame::UserUrlLinkFramePrivate
{
public:
  UserUrlLinkFramePrivate() : textEncoding(String::Latin1) {}
  String::Type textEncoding;
  String description;
};

UrlLinkFrame::UrlLinkFrame(const ByteVector &data) :
  Frame(data)
{
  d = new UrlLinkFramePrivate;
  setData(data);
}

UrlLinkFrame::~UrlLinkFrame()
{
  delete d;
}

void UrlLinkFrame::setUrl(const String &s)
{
  d->url = s;
}

String UrlLinkFrame::url() const
{
  return d->url;
}

void UrlLinkFrame::setText(const String &s)
{
  setUrl(s);
}

String UrlLinkFrame::toString() const
{
  return url();
}

void UrlLinkFrame::parseFields(const ByteVector &data)
{
  d->url = String(data);
}

ByteVector UrlLinkFrame::renderFields() const
{
  return d->url.data(String::Latin1);
}

UrlLinkFrame::UrlLinkFrame(const ByteVector &data, Header *h) : Frame(h)
{
  d = new UrlLinkFramePrivate;
  parseFields(fieldData(data));
}


UserUrlLinkFrame::UserUrlLinkFrame(String::Type encoding) :
  UrlLinkFrame("WXXX")
{
  d = new UserUrlLinkFramePrivate;
  d->textEncoding = encoding;
}

UserUrlLinkFrame::UserUrlLinkFrame(const ByteVector &data) :
  UrlLinkFrame(data)
{
  d = new UserUrlLinkFramePrivate;
  setData(data);
}

UserUrlLinkFrame::~UserUrlLinkFrame()
{
  delete d;
}

String UserUrlLinkFrame::toString() const
{
  return "[" + description() + "] " + url();
}

String::Type UserUrlLinkFrame::textEncoding() const
{
  return d->textEncoding;
}

void UserUrlLinkFrame::setTextEncoding(String::Type encoding)
{
  d->textEncoding = encoding;
}

String UserUrlLinkFrame::description() const
{
  return d->description;
}

void UserUrlLinkFrame::setDescription(const String &s)
{
  d->description = s;
}

void UserUrlLinkFrame::parseFields(const ByteVector &data)
{
  if(data.size() < 2) {
    debug("A user URL link frame must contain at least 2 bytes.");
    return;
  }

  int pos = 0;

  d->textEncoding = String::Type(data[0]);
  pos += 1;

  if(d->textEncoding == String::Latin1 || d->textEncoding == String::UTF8) {
    int offset = data.find(textDelimiter(d->textEncoding), pos);
    if(offset < pos)
      return;

    d->description = String(data.mid(pos, offset - pos), d->textEncoding);
    pos = offset + 1;
  }
  else {
    int len = data.mid(pos).find(textDelimiter(d->textEncoding), 0, 2);
    if(len < 0)
      return;

    d->description = String(data.mid(pos, len), d->textEncoding);
    pos += len + 2;
  }

  setUrl(String(data.mid(pos)));
}

ByteVector UserUrlLinkFrame::renderFields() const
{
  ByteVector v;

  String::Type encoding = checkEncoding(d->description, d->textEncoding);

  v.append(char(encoding));
  v.append(d->description.data(encoding));
  v.append(textDelimiter(encoding));
  v.append(url().data(String::Latin1));

  return v;
}

UserUrlLinkFrame::UserUrlLinkFrame(const ByteVector &data, Header *h) : UrlLinkFrame(data, h)
{
  d = new UserUrlLinkFramePrivate;
  parseFields(fieldData(data));
}
