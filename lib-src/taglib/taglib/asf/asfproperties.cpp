/**************************************************************************
    copyright            : (C) 2005-2007 by Lukáš Lalinský
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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef WITH_ASF

#include <tdebug.h>
#include <tstring.h>
#include "asfproperties.h"

using namespace TagLib;

class ASF::Properties::PropertiesPrivate
{
public:
  PropertiesPrivate(): length(0), bitrate(0), sampleRate(0), channels(0) {}
  int length;
  int bitrate;
  int sampleRate;
  int channels;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

ASF::Properties::Properties() : AudioProperties(AudioProperties::Average)
{
  d = new PropertiesPrivate;
}

ASF::Properties::~Properties()
{
  if(d)
    delete d;  
}

int ASF::Properties::length() const
{
  return d->length;
}

int ASF::Properties::bitrate() const
{
  return d->bitrate;
}

int ASF::Properties::sampleRate() const
{
  return d->sampleRate;
}

int ASF::Properties::channels() const
{
  return d->channels;
} 

////////////////////////////////////////////////////////////////////////////////
// private members
////////////////////////////////////////////////////////////////////////////////

void ASF::Properties::setLength(int length)
{
  d->length = length;
}

void ASF::Properties::setBitrate(int length)
{
  d->bitrate = length;
}

void ASF::Properties::setSampleRate(int length)
{
  d->sampleRate = length;
}

void ASF::Properties::setChannels(int length)
{
  d->channels = length;
}

#endif
