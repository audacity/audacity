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

#include <bitset>

#include <tstring.h>
#include <tdebug.h>

#include "vorbisfile.h"

using namespace TagLib;

class Vorbis::File::FilePrivate
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

namespace TagLib {
  /*!
   * Vorbis headers can be found with one type ID byte and the string "vorbis" in
   * an Ogg stream.  0x03 indicates the comment header.
   */
  static const char vorbisCommentHeaderID[] = { 0x03, 'v', 'o', 'r', 'b', 'i', 's', 0 };
}

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

Vorbis::File::File(FileName file, bool readProperties,
                   Properties::ReadStyle propertiesStyle) : Ogg::File(file)
{
  d = new FilePrivate;
  read(readProperties, propertiesStyle);
}

Vorbis::File::~File()
{
  delete d;
}

Ogg::XiphComment *Vorbis::File::tag() const
{
  return d->comment;
}

Vorbis::Properties *Vorbis::File::audioProperties() const
{
  return d->properties;
}

bool Vorbis::File::save()
{
  ByteVector v(vorbisCommentHeaderID);

  if(!d->comment)
    d->comment = new Ogg::XiphComment;
  v.append(d->comment->render());

  setPacket(1, v);

  return Ogg::File::save();
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void Vorbis::File::read(bool readProperties, Properties::ReadStyle propertiesStyle)
{
  ByteVector commentHeaderData = packet(1);

  if(commentHeaderData.mid(0, 7) != vorbisCommentHeaderID) {
    debug("Vorbis::File::read() - Could not find the Vorbis comment header.");
    setValid(false);
    return;
  }

  d->comment = new Ogg::XiphComment(commentHeaderData.mid(7));

  if(readProperties)
    d->properties = new Properties(this, propertiesStyle);
}
