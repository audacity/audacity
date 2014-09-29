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

#include <tbytevectorlist.h>
#include <id3v2tag.h>
#include <tdebug.h>
#include <tstringlist.h>

#include "commentsframe.h"
#include "tpropertymap.h"

using namespace TagLib;
using namespace ID3v2;

class CommentsFrame::CommentsFramePrivate
{
public:
  CommentsFramePrivate() : textEncoding(String::Latin1) {}
  String::Type textEncoding;
  ByteVector language;
  String description;
  String text;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

CommentsFrame::CommentsFrame(String::Type encoding) : Frame("COMM")
{
  d = new CommentsFramePrivate;
  d->textEncoding = encoding;
}

CommentsFrame::CommentsFrame(const ByteVector &data) : Frame(data)
{
  d = new CommentsFramePrivate;
  setData(data);
}

CommentsFrame::~CommentsFrame()
{
  delete d;
}

String CommentsFrame::toString() const
{
  return d->text;
}

ByteVector CommentsFrame::language() const
{
  return d->language;
}

String CommentsFrame::description() const
{
  return d->description;
}

String CommentsFrame::text() const
{
  return d->text;
}

void CommentsFrame::setLanguage(const ByteVector &languageEncoding)
{
  d->language = languageEncoding.mid(0, 3);
}

void CommentsFrame::setDescription(const String &s)
{
  d->description = s;
}

void CommentsFrame::setText(const String &s)
{
  d->text = s;
}

String::Type CommentsFrame::textEncoding() const
{
  return d->textEncoding;
}

void CommentsFrame::setTextEncoding(String::Type encoding)
{
  d->textEncoding = encoding;
}

PropertyMap CommentsFrame::asProperties() const
{
  String key = description().upper();
  PropertyMap map;
  if(key.isEmpty() || key == "COMMENT")
    map.insert("COMMENT", text());
  else if(key.isNull())
    map.unsupportedData().append(L"COMM/" + description());
  else
    map.insert("COMMENT:" + key, text());
  return map;
}

CommentsFrame *CommentsFrame::findByDescription(const ID3v2::Tag *tag, const String &d) // static
{
  ID3v2::FrameList comments = tag->frameList("COMM");

  for(ID3v2::FrameList::ConstIterator it = comments.begin();
      it != comments.end();
      ++it)
  {
    CommentsFrame *frame = dynamic_cast<CommentsFrame *>(*it);
    if(frame && frame->description() == d)
      return frame;
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

void CommentsFrame::parseFields(const ByteVector &data)
{
  if(data.size() < 5) {
    debug("A comment frame must contain at least 5 bytes.");
    return;
  }

  d->textEncoding = String::Type(data[0]);
  d->language = data.mid(1, 3);

  int byteAlign = d->textEncoding == String::Latin1 || d->textEncoding == String::UTF8 ? 1 : 2;

  ByteVectorList l = ByteVectorList::split(data.mid(4), textDelimiter(d->textEncoding), byteAlign, 2);

  if(l.size() == 2) {
    if(d->textEncoding == String::Latin1) {
      d->description = Tag::latin1StringHandler()->parse(l.front());
      d->text = Tag::latin1StringHandler()->parse(l.back());
    } else {
      d->description = String(l.front(), d->textEncoding);
      d->text = String(l.back(), d->textEncoding);
    }  
  }
}

ByteVector CommentsFrame::renderFields() const
{
  ByteVector v;

  String::Type encoding = d->textEncoding;

  encoding = checkTextEncoding(d->description, encoding);
  encoding = checkTextEncoding(d->text, encoding);

  v.append(char(encoding));
  v.append(d->language.size() == 3 ? d->language : "XXX");
  v.append(d->description.data(encoding));
  v.append(textDelimiter(encoding));
  v.append(d->text.data(encoding));

  return v;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

CommentsFrame::CommentsFrame(const ByteVector &data, Header *h) : Frame(h)
{
  d = new CommentsFramePrivate();
  parseFields(fieldData(data));
}
