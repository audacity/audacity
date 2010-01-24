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

using namespace TagLib;

const char *MP4::Atom::containers[10] = {
    "moov", "udta", "mdia", "meta", "ilst",
    "stbl", "minf", "moof", "traf", "trak",
};

MP4::Atom::Atom(File *file)
{
  offset = file->tell();
  ByteVector header = file->readBlock(8);
  length = header.mid(0, 4).toUInt();

  if (length == 1) {
    debug("MP4: 64-bit atoms are not supported");
    length = 0;
    file->seek(0, File::End);
    return;
  }
  if (length < 8) {
    debug("MP4: Invalid atom size");
    length = 0;
    file->seek(0, File::End);
    return;
  }

  name = header.mid(4, 4);

  for(int i = 0; i < numContainers; i++) {
    if(name == containers[i]) {
      if(name == "meta") {
        file->seek(4, File::Current);
      }
      while(file->tell() < offset + length) {
        children.append(new MP4::Atom(file));
      }
      return;
    }
  }

  file->seek(offset + length);
}

MP4::Atom::~Atom()
{
  for(unsigned int i = 0; i < children.size(); i++) {
    delete children[i];
  }
  children.clear();
}

MP4::Atom *
MP4::Atom::find(const char *name1, const char *name2, const char *name3, const char *name4)
{
  if(name1 == 0) {
    return this;
  }
  for(unsigned int i = 0; i < children.size(); i++) {
    if(children[i]->name == name1) {
      return children[i]->find(name2, name3, name4);
    }
  }
  return 0;
}

MP4::AtomList
MP4::Atom::findall(const char *name, bool recursive)
{
  MP4::AtomList result;
  for(unsigned int i = 0; i < children.size(); i++) {
    if(children[i]->name == name) {
      result.append(children[i]);
    }
    if(recursive) {
      result.append(children[i]->findall(name, recursive));
    }
  }
  return result;
}

bool
MP4::Atom::path(MP4::AtomList &path, const char *name1, const char *name2, const char *name3)
{
  path.append(this);
  if(name1 == 0) {
    return true;
  }
  for(unsigned int i = 0; i < children.size(); i++) {
    if(children[i]->name == name1) {
      return children[i]->path(path, name2, name3);
    }
  }
  return false;
}

MP4::Atoms::Atoms(File *file)
{
  file->seek(0, File::End);
  long end = file->tell();
  file->seek(0);
  while(file->tell() + 8 <= end) {
    atoms.append(new MP4::Atom(file));
  }
}

MP4::Atoms::~Atoms()
{
  for(unsigned int i = 0; i < atoms.size(); i++) {
    delete atoms[i];
  }
  atoms.clear();
}

MP4::Atom *
MP4::Atoms::find(const char *name1, const char *name2, const char *name3, const char *name4)
{
  for(unsigned int i = 0; i < atoms.size(); i++) {
    if(atoms[i]->name == name1) {
      return atoms[i]->find(name2, name3, name4);
    }
  }
  return 0;
}

MP4::AtomList
MP4::Atoms::path(const char *name1, const char *name2, const char *name3, const char *name4)
{
  MP4::AtomList path;
  for(unsigned int i = 0; i < atoms.size(); i++) {
    if(atoms[i]->name == name1) {
      if(!atoms[i]->path(path, name2, name3, name4)) {
        path.clear();
      }
      return path;
    }
  }
  return path;
}

#endif
