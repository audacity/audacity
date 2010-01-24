/***************************************************************************
    copyright            : (C) 2008 by Scott Wheeler
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

#include <tstring.h>
#include <tdebug.h>
#include <cmath>
// ldexp is a c99 function, which might not be defined in <cmath>
// so we pull in math.h too and hope it does the right (wrong) thing
// wrt. c99 functions in C++
#include <math.h>

#include "aiffproperties.h"

////////////////////////////////////////////////////////////////////////////////
// nasty 80-bit float helpers
////////////////////////////////////////////////////////////////////////////////

#define UnsignedToFloat(u) (((double)((long)(u - 2147483647L - 1))) + 2147483648.0)

static double ConvertFromIeeeExtended(unsigned char *bytes)
{
  double f;
  int expon;
  unsigned long hiMant, loMant;
    
  expon  = ((bytes[0] & 0x7F) << 8) | (bytes[1] & 0xFF);

  hiMant = ((unsigned long)(bytes[2] & 0xFF) << 24) |
           ((unsigned long)(bytes[3] & 0xFF) << 16) |
           ((unsigned long)(bytes[4] & 0xFF) << 8)  |
           ((unsigned long)(bytes[5] & 0xFF));

  loMant = ((unsigned long)(bytes[6] & 0xFF) << 24) |
           ((unsigned long)(bytes[7] & 0xFF) << 16) |
           ((unsigned long)(bytes[8] & 0xFF) << 8)  |
           ((unsigned long)(bytes[9] & 0xFF));

  if (expon == 0 && hiMant == 0 && loMant == 0)
    f = 0;
  else {
    if(expon == 0x7FFF) /* Infinity or NaN */
      f = HUGE_VAL;
    else {
      expon -= 16383;
      f  = ldexp(UnsignedToFloat(hiMant), expon -= 31);
      f += ldexp(UnsignedToFloat(loMant), expon -= 32);
    }
  }

  if(bytes[0] & 0x80)
    return -f;
  else
    return f;
}

using namespace TagLib;

class RIFF::AIFF::Properties::PropertiesPrivate
{
public:
  PropertiesPrivate() :
    length(0),
    bitrate(0),
    sampleRate(0),
    channels(0)
  {

  }

  int length;
  int bitrate;
  int sampleRate;
  int channels;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

RIFF::AIFF::Properties::Properties(const ByteVector &data, ReadStyle style) : AudioProperties(style)
{
  d = new PropertiesPrivate;
  read(data);
}

RIFF::AIFF::Properties::~Properties()
{
  delete d;
}

int RIFF::AIFF::Properties::length() const
{
  return d->length;
}

int RIFF::AIFF::Properties::bitrate() const
{
  return d->bitrate;
}

int RIFF::AIFF::Properties::sampleRate() const
{
  return d->sampleRate;
}

int RIFF::AIFF::Properties::channels() const
{
  return d->channels;
}

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void RIFF::AIFF::Properties::read(const ByteVector &data)
{
  d->channels       = data.mid(0, 2).toShort();
  uint sampleFrames = data.mid(2, 4).toUInt();
  short sampleSize  = data.mid(6, 2).toShort();
  double sampleRate = ConvertFromIeeeExtended(reinterpret_cast<unsigned char *>(data.mid(8, 10).data()));
  d->sampleRate     = sampleRate;
  d->bitrate        = (sampleRate * sampleSize * d->channels) / 1024.0;
  d->length         = sampleFrames / d->sampleRate;
}
