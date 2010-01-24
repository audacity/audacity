/**************************************************************************
    copyright            : (C) 2007 by Lukáš Lalinský
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

#ifdef WITH_MP4

#include <tdebug.h>
#include <tstring.h>
#include "mp4atom.h"
#include "mp4tag.h"
#include "mp4file.h"

using namespace TagLib;

class MP4::File::FilePrivate
{
public:
  FilePrivate() : tag(0), atoms(0)
  {
  }

  ~FilePrivate()
  {
    if(atoms) {
        delete atoms;
        atoms = 0;
    }
    if(tag) {
        delete tag;
        tag = 0;
    }
    if(properties) {
        delete properties;
        properties = 0;
    }
  }

  MP4::Tag *tag;
  MP4::Atoms *atoms;
  MP4::Properties *properties;
};

MP4::File::File(FileName file, bool readProperties, AudioProperties::ReadStyle audioPropertiesStyle)
    : TagLib::File(file)
{
  d = new FilePrivate;
  read(readProperties, audioPropertiesStyle);
}

MP4::File::~File()
{
  delete d;
}

MP4::Tag *
MP4::File::tag() const
{
  return d->tag;
}

MP4::Properties *
MP4::File::audioProperties() const
{
  return d->properties;
}

void
MP4::File::read(bool readProperties, Properties::ReadStyle audioPropertiesStyle)
{
  if(!isValid())
    return;

  d->atoms = new Atoms(this);
  d->tag = new Tag(this, d->atoms);
  if(readProperties) {
    d->properties = new Properties(this, d->atoms, audioPropertiesStyle);
  }
}

bool
MP4::File::save()
{
  return d->tag->save();
}

#endif
