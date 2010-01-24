/***************************************************************************
    copyright            : (C) 2003 by Ismael Orenstein
    email                : orenstein@kde.org
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

#include "xingheader.h"

using namespace TagLib;

class MPEG::XingHeader::XingHeaderPrivate
{
public:
  XingHeaderPrivate() :
    frames(0),
    size(0),
    valid(false)
    {}

  uint frames;
  uint size;
  bool valid;
};

MPEG::XingHeader::XingHeader(const ByteVector &data)
{
  d = new XingHeaderPrivate;
  parse(data);
}

MPEG::XingHeader::~XingHeader()
{
  delete d;
}

bool MPEG::XingHeader::isValid() const
{
  return d->valid;
}

TagLib::uint MPEG::XingHeader::totalFrames() const
{
  return d->frames;
}

TagLib::uint MPEG::XingHeader::totalSize() const
{
  return d->size;
}

int MPEG::XingHeader::xingHeaderOffset(TagLib::MPEG::Header::Version v,
                                       TagLib::MPEG::Header::ChannelMode c)
{
  if(v == MPEG::Header::Version1) {
    if(c == MPEG::Header::SingleChannel)
      return 0x15;
    else
      return 0x24;
  }
  else {
    if(c == MPEG::Header::SingleChannel)
      return 0x0D;
    else
      return 0x15;
  }
}

void MPEG::XingHeader::parse(const ByteVector &data)
{
  // Check to see if a valid Xing header is available.

  if(!data.startsWith("Xing") && !data.startsWith("Info"))
    return;

  // If the XingHeader doesn't contain the number of frames and the total stream
  // info it's invalid.

  if(!(data[7] & 0x01)) {
    debug("MPEG::XingHeader::parse() -- Xing header doesn't contain the total number of frames.");
    return;
  }

  if(!(data[7] & 0x02)) {
    debug("MPEG::XingHeader::parse() -- Xing header doesn't contain the total stream size.");
    return;
  }

  d->frames = data.mid(8, 4).toUInt();
  d->size = data.mid(12, 4).toUInt();

  d->valid = true;
}
