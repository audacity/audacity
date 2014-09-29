/***************************************************************************
    copyright            : (C) 2012 by Lukáš Lalinský
    email                : lalinsky@gmail.com

    copyright            : (C) 2002 - 2008 by Scott Wheeler
    email                : wheeler@kde.org
                           (original Vorbis implementation)
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

#include <bitset>

#include <tstring.h>
#include <tdebug.h>
#include <tpropertymap.h>

#include "opusfile.h"

using namespace TagLib;
using namespace TagLib::Ogg;

class Opus::File::FilePrivate
{
public:
  FilePrivate() :
    comment(0),
    properties(0) {}

  ~FilePrivate()
  {
    delete comment;
    delete properties;
  }

  Ogg::XiphComment *comment;
  Properties *properties;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

Opus::File::File(FileName file, bool readProperties, Properties::ReadStyle propertiesStyle) :
  Ogg::File(file),
  d(new FilePrivate())
{
  if(isOpen())
    read(readProperties, propertiesStyle);
}

Opus::File::File(IOStream *stream, bool readProperties, Properties::ReadStyle propertiesStyle) :
  Ogg::File(stream),
  d(new FilePrivate())
{
  if(isOpen())
    read(readProperties, propertiesStyle);
}

Opus::File::~File()
{
  delete d;
}

Ogg::XiphComment *Opus::File::tag() const
{
  return d->comment;
}

PropertyMap Opus::File::properties() const
{
  return d->comment->properties();
}

PropertyMap Opus::File::setProperties(const PropertyMap &properties)
{
  return d->comment->setProperties(properties);
}

Opus::Properties *Opus::File::audioProperties() const
{
  return d->properties;
}

bool Opus::File::save()
{
  if(!d->comment)
    d->comment = new Ogg::XiphComment;

  setPacket(1, ByteVector("OpusTags", 8) + d->comment->render(false));

  return Ogg::File::save();
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void Opus::File::read(bool readProperties, Properties::ReadStyle propertiesStyle)
{
  ByteVector opusHeaderData = packet(0);

  if(!opusHeaderData.startsWith("OpusHead")) {
    setValid(false);
    debug("Opus::File::read() -- invalid Opus identification header");
    return;
  }

  ByteVector commentHeaderData = packet(1);

  if(!commentHeaderData.startsWith("OpusTags")) {
    setValid(false);
    debug("Opus::File::read() -- invalid Opus tags header");
    return;
  }

  d->comment = new Ogg::XiphComment(commentHeaderData.mid(8));

  if(readProperties)
    d->properties = new Properties(this, propertiesStyle);
}
