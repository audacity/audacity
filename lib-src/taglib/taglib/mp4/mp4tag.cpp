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

using namespace TagLib;

class MP4::Tag::TagPrivate
{
public:
  TagPrivate() : file(0), atoms(0) {}
  ~TagPrivate() {}
  File *file;
  Atoms *atoms;
  ItemListMap items;
};

MP4::Tag::Tag(File *file, MP4::Atoms *atoms)
{
  d = new TagPrivate;
  d->file = file;
  d->atoms = atoms;

  MP4::Atom *ilst = atoms->find("moov", "udta", "meta", "ilst");
  if(!ilst) {
    //debug("Atom moov.udta.meta.ilst not found.");
    return;
  }

  for(unsigned int i = 0; i < ilst->children.size(); i++) {
    MP4::Atom *atom = ilst->children[i];
    file->seek(atom->offset + 8);
    if(atom->name == "----") {
      parseFreeForm(atom, file);
    }
    else if(atom->name == "trkn" || atom->name == "disk") {
      parseIntPair(atom, file);
    }
    else if(atom->name == "cpil" || atom->name == "pgap" || atom->name == "pcst") {
      parseBool(atom, file);
    }
    else if(atom->name == "tmpo") {
      parseInt(atom, file);
    }
    else {
      parseText(atom, file);
    }
  }
}

ByteVectorList
MP4::Tag::parseData(MP4::Atom *atom, TagLib::File *file, int expectedFlags, bool freeForm)
{
  ByteVectorList result;
  ByteVector data = file->readBlock(atom->length - 8);
  int i = 0;
  unsigned int pos = 0;
  while(pos < data.size()) {
    int length = data.mid(pos, 4).toUInt();
    ByteVector name = data.mid(pos + 4, 4);
    int flags = data.mid(pos + 8, 4).toUInt();
    if(freeForm && i < 2) {
      if(i == 0 && name != "mean") {
        debug("MP4: Unexpected atom \"" + name + "\", expecting \"mean\"");
        return result;
      }
      else if(i == 1 && name != "name") {
        debug("MP4: Unexpected atom \"" + name + "\", expecting \"name\"");
        return result;
      }
      result.append(data.mid(pos + 12, length - 12));
    }
    else {
      if(name != "data") {
        debug("MP4: Unexpected atom \"" + name + "\", expecting \"data\"");
        return result;
      }
      if(expectedFlags == -1 || flags == expectedFlags) {
        result.append(data.mid(pos + 16, length - 16));
      }
    }
    pos += length;
    i++;
  }
  return result;
}

void
MP4::Tag::parseInt(MP4::Atom *atom, TagLib::File *file)
{
  ByteVectorList data = parseData(atom, file);
  if(data.size()) {
    d->items.insert(atom->name, (int)data[0].toShort());
  }
}

void
MP4::Tag::parseIntPair(MP4::Atom *atom, TagLib::File *file)
{
  ByteVectorList data = parseData(atom, file);
  if(data.size()) {
    int a = data[0].mid(2, 2).toShort();
    int b = data[0].mid(4, 2).toShort();
    d->items.insert(atom->name, MP4::Item(a, b));
  }
}

void
MP4::Tag::parseBool(MP4::Atom *atom, TagLib::File *file)
{
  ByteVectorList data = parseData(atom, file);
  if(data.size()) {
    d->items.insert(atom->name, data[0][0] != '\0');
  }
}

void
MP4::Tag::parseText(MP4::Atom *atom, TagLib::File *file, int expectedFlags)
{
  ByteVectorList data = parseData(atom, file, expectedFlags);
  if(data.size()) {
    StringList value;
    for(unsigned int i = 0; i < data.size(); i++) {
      value.append(String(data[i], String::UTF8));
    }
    d->items.insert(atom->name, value);
  }
}

void
MP4::Tag::parseFreeForm(MP4::Atom *atom, TagLib::File *file)
{
  ByteVectorList data = parseData(atom, file, 1, true);
  if(data.size() > 2) {
    StringList value;
    for(unsigned int i = 2; i < data.size(); i++) {
      value.append(String(data[i], String::UTF8));
    }
    String name = "----:" + data[0] + ":" + data[1];
    d->items.insert(name, value);
  }
}

ByteVector
MP4::Tag::padIlst(const ByteVector &data, int length)
{
  if (length == -1) {
    length = ((data.size() + 1023) & ~1023) - data.size();
  }
  return renderAtom("free", ByteVector(length, '\1'));
}

ByteVector
MP4::Tag::renderAtom(const ByteVector &name, const ByteVector &data)
{
  return ByteVector::fromUInt(data.size() + 8) + name + data;
}

ByteVector
MP4::Tag::renderData(const ByteVector &name, int flags, const ByteVectorList &data)
{
  ByteVector result;
  for(unsigned int i = 0; i < data.size(); i++) {
    result.append(renderAtom("data", ByteVector::fromUInt(flags) + ByteVector(4, '\0') + data[i]));
  }
  return renderAtom(name, result);
}

ByteVector
MP4::Tag::renderBool(const ByteVector &name, MP4::Item &item)
{
  ByteVectorList data;
  data.append(ByteVector(1, item.toBool() ? '\1' : '\0'));
  return renderData(name, 0x15, data);
}

ByteVector
MP4::Tag::renderInt(const ByteVector &name, MP4::Item &item)
{
  ByteVectorList data;
  data.append(ByteVector::fromShort(item.toInt()));
  return renderData(name, 0x15, data);
}

ByteVector
MP4::Tag::renderIntPair(const ByteVector &name, MP4::Item &item)
{
  ByteVectorList data;
  data.append(ByteVector(2, '\0') +
              ByteVector::fromShort(item.toIntPair().first) +
              ByteVector::fromShort(item.toIntPair().second) +
              ByteVector(2, '\0'));
  return renderData(name, 0x15, data);
}

ByteVector
MP4::Tag::renderIntPairNoTrailing(const ByteVector &name, MP4::Item &item)
{
  ByteVectorList data;
  data.append(ByteVector(2, '\0') +
              ByteVector::fromShort(item.toIntPair().first) +
              ByteVector::fromShort(item.toIntPair().second));
  return renderData(name, 0x15, data);
}

ByteVector
MP4::Tag::renderText(const ByteVector &name, MP4::Item &item, int flags)
{
  ByteVectorList data;
  StringList value = item.toStringList();
  for(unsigned int i = 0; i < value.size(); i++) {
    data.append(value[i].data(String::UTF8));
  }
  return renderData(name, flags, data);
}

ByteVector
MP4::Tag::renderFreeForm(const String &name, MP4::Item &item)
{
  StringList header = StringList::split(name, ":");
  if (header.size() != 3) {
    debug("MP4: Invalid free-form item name \"" + name + "\"");
    return ByteVector::null;
  }
  ByteVector data;
  data.append(renderAtom("mean", ByteVector::fromUInt(0) + header[1].data(String::UTF8)));
  data.append(renderAtom("name", ByteVector::fromUInt(0) + header[2].data(String::UTF8)));
  StringList value = item.toStringList();
  for(unsigned int i = 0; i < value.size(); i++) {
    data.append(renderAtom("data", ByteVector::fromUInt(1) + ByteVector(4, '\0') + value[i].data(String::UTF8)));
  }
  return renderAtom("----", data);
}

bool
MP4::Tag::save()
{
  ByteVector data;
  for(MP4::ItemListMap::Iterator i = d->items.begin(); i != d->items.end(); i++) {
    const String name = i->first;
    if(name.startsWith("----")) {
      data.append(renderFreeForm(name, i->second));
    }
    else if(name == "trkn") {
      data.append(renderIntPair(name.data(String::Latin1), i->second));
    }
    else if(name == "disk") {
      data.append(renderIntPairNoTrailing(name.data(String::Latin1), i->second));
    }
    else if(name == "cpil" || name == "pgap" || name == "pcst") {
      data.append(renderBool(name.data(String::Latin1), i->second));
    }
    else if(name == "tmpo") {
      data.append(renderInt(name.data(String::Latin1), i->second));
    }
    else if(name.size() == 4){
      data.append(renderText(name.data(String::Latin1), i->second));
    }
    else {
      debug("MP4: Unknown item name \"" + name + "\"");
    }
  }
  data = renderAtom("ilst", data);

  AtomList path = d->atoms->path("moov", "udta", "meta", "ilst");
  if(path.size() == 4) {
    saveExisting(data, path);
  }
  else {
    saveNew(data);
  }

  return true;
}

void
MP4::Tag::updateParents(AtomList &path, long delta, int ignore)
{
  for(unsigned int i = 0; i < path.size() - ignore; i++) {
    d->file->seek(path[i]->offset);
    long size = d->file->readBlock(4).toUInt() + delta;
    d->file->seek(path[i]->offset);
    d->file->writeBlock(ByteVector::fromUInt(size));
  }
}

void
MP4::Tag::updateOffsets(long delta, long offset)
{
  MP4::Atom *moov = d->atoms->find("moov");
  if(moov) {
    MP4::AtomList stco = moov->findall("stco", true);
    for(unsigned int i = 0; i < stco.size(); i++) {
      MP4::Atom *atom = stco[i];
      if(atom->offset > offset) {
        atom->offset += delta;
      }
      d->file->seek(atom->offset + 12);
      ByteVector data = d->file->readBlock(atom->length - 12);
      unsigned int count = data.mid(0, 4).toUInt();
      d->file->seek(atom->offset + 16);
      int pos = 4;
      while(count--) {
        long o = data.mid(pos, 4).toUInt();
        if(o > offset) {
          o += delta;
        }
        d->file->writeBlock(ByteVector::fromUInt(o));
        pos += 4;
      }
    }

    MP4::AtomList co64 = moov->findall("co64", true);
    for(unsigned int i = 0; i < co64.size(); i++) {
      MP4::Atom *atom = co64[i];
      if(atom->offset > offset) {
        atom->offset += delta;
      }
      d->file->seek(atom->offset + 12);
      ByteVector data = d->file->readBlock(atom->length - 12);
      unsigned int count = data.mid(0, 4).toUInt();
      d->file->seek(atom->offset + 16);
      int pos = 4;
      while(count--) {
        long long o = data.mid(pos, 8).toLongLong();
        if(o > offset) {
          o += delta;
        }
        d->file->writeBlock(ByteVector::fromLongLong(o));
        pos += 8;
      }
    }
  }

  MP4::Atom *moof = d->atoms->find("moof");
  if(moof) {
    MP4::AtomList tfhd = moof->findall("tfhd", true);
    for(unsigned int i = 0; i < tfhd.size(); i++) {
      MP4::Atom *atom = tfhd[i];
      if(atom->offset > offset) {
        atom->offset += delta;
      }
      d->file->seek(atom->offset + 9);
      ByteVector data = d->file->readBlock(atom->offset - 9);
      unsigned int flags = (ByteVector(1, '\0') + data.mid(0, 3)).toUInt();
      if(flags & 1) {
        long long o = data.mid(7, 8).toLongLong();
        if(o > offset) {
          o += delta;
        }
        d->file->seek(atom->offset + 16);
        d->file->writeBlock(ByteVector::fromLongLong(o));
      }
    }
  }
}

void
MP4::Tag::saveNew(ByteVector &data)
{
  data = renderAtom("meta", TagLib::ByteVector(4, '\0') +
                    renderAtom("hdlr", TagLib::ByteVector(8, '\0') + TagLib::ByteVector("mdirappl") + TagLib::ByteVector(9, '\0')) +
                    data + padIlst(data));

  AtomList path = d->atoms->path("moov", "udta");
  if(path.size() != 2) {
    path = d->atoms->path("moov");
    data = renderAtom("udta", data);
  }

  long offset = path[path.size() - 1]->offset + 8;
  d->file->insert(data, offset, 0);

  updateParents(path, data.size());
  updateOffsets(data.size(), offset);
}

void
MP4::Tag::saveExisting(ByteVector &data, AtomList &path)
{
  MP4::Atom *ilst = path[path.size() - 1];
  long offset = ilst->offset;
  long length = ilst->length;

  MP4::Atom *meta = path[path.size() - 2];
  AtomList::Iterator index = meta->children.find(ilst);
  if(index != meta->children.begin()) {
    AtomList::Iterator prevIndex = index;
    prevIndex--;
    MP4::Atom *prev = *prevIndex;
    if(prev->name == "free") {
      offset = prev->offset;
      length += prev->length;
    }
  }
  if(index != meta->children.end()) {
    AtomList::Iterator nextIndex = index;
    nextIndex++;
    MP4::Atom *next = *nextIndex;
    if(next->name == "free") {
      length += next->length;
    }
  }

  long delta = data.size() - length;
  if(delta > 0 || (delta < 0 && delta > -8)) {
    data.append(padIlst(data));
    delta = data.size() - length;
  }
  else if(delta < 0) {
    data.append(padIlst(data, -delta - 8));
    delta = 0;
  }

  d->file->insert(data, offset, length);

  if(delta) {
    updateParents(path, delta, 1);
    updateOffsets(delta, offset);
  }
}

String
MP4::Tag::title() const
{
  if(d->items.contains("\251nam"))
    return d->items["\251nam"].toStringList().toString(", ");
  return String::null;
}

String
MP4::Tag::artist() const
{
  if(d->items.contains("\251ART"))
    return d->items["\251ART"].toStringList().toString(", ");
  return String::null;
}

String
MP4::Tag::album() const
{
  if(d->items.contains("\251alb"))
    return d->items["\251alb"].toStringList().toString(", ");
  return String::null;
}

String
MP4::Tag::comment() const
{
  if(d->items.contains("\251cmt"))
    return d->items["\251cmt"].toStringList().toString(", ");
  return String::null;
}

String
MP4::Tag::genre() const
{
  if(d->items.contains("\251gen"))
    return d->items["\251gen"].toStringList().toString(", ");
  return String::null;
}

unsigned int
MP4::Tag::year() const
{
  if(d->items.contains("\251day"))
    return d->items["\251day"].toStringList().toString().toInt();
  return 0;
}

unsigned int
MP4::Tag::track() const
{
  if(d->items.contains("trkn"))
    return d->items["trkn"].toIntPair().first;
  return 0;
}

void
MP4::Tag::setTitle(const String &value)
{
  d->items["\251nam"] = StringList(value);
}

void
MP4::Tag::setArtist(const String &value)
{
  d->items["\251ART"] = StringList(value);
}

void
MP4::Tag::setAlbum(const String &value)
{
  d->items["\251alb"] = StringList(value);
}

void
MP4::Tag::setComment(const String &value)
{
  d->items["\251cmt"] = StringList(value);
}

void
MP4::Tag::setGenre(const String &value)
{
  d->items["\251gen"] = StringList(value);
}

void
MP4::Tag::setYear(uint value)
{
  d->items["\251day"] = StringList(String::number(value));
}

void
MP4::Tag::setTrack(uint value)
{
  d->items["trkn"] = MP4::Item(value, 0);
}

MP4::ItemListMap &
MP4::Tag::itemListMap()
{
  return d->items;
}

#endif
