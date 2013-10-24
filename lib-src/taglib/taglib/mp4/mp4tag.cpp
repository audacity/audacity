/**************************************************************************
    copyright            : (C) 2007,2011 by Lukáš Lalinský
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
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *
 *   02110-1301  USA                                                       *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#include <tdebug.h>
#include <tstring.h>
#include <tpropertymap.h>
#include "mp4atom.h"
#include "mp4tag.h"
#include "id3v1genres.h"

using namespace TagLib;

class MP4::Tag::TagPrivate
{
public:
  TagPrivate() : file(0), atoms(0) {}
  ~TagPrivate() {}
  TagLib::File *file;
  Atoms *atoms;
  ItemListMap items;
};

MP4::Tag::Tag()
{
  d = new TagPrivate;
}

MP4::Tag::Tag(TagLib::File *file, MP4::Atoms *atoms)
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
    else if(atom->name == "cpil" || atom->name == "pgap" || atom->name == "pcst" ||
            atom->name == "hdvd") {
      parseBool(atom, file);
    }
    else if(atom->name == "tmpo") {
      parseInt(atom, file);
    }
    else if(atom->name == "tvsn" || atom->name == "tves" || atom->name == "cnID" ||
            atom->name == "sfID" || atom->name == "atID" || atom->name == "geID") {
      parseUInt(atom, file);
    }
    else if(atom->name == "plID") {
      parseLongLong(atom, file);
    }
    else if(atom->name == "stik" || atom->name == "rtng" || atom->name == "akID") {
      parseByte(atom, file);
    }
    else if(atom->name == "gnre") {
      parseGnre(atom, file);
    }
    else if(atom->name == "covr") {
      parseCovr(atom, file);
    }
    else {
      parseText(atom, file);
    }
  }
}

MP4::Tag::~Tag()
{
  delete d;
}

MP4::AtomDataList
MP4::Tag::parseData2(MP4::Atom *atom, TagLib::File *file, int expectedFlags, bool freeForm)
{
  AtomDataList result;
  ByteVector data = file->readBlock(atom->length - 8);
  int i = 0;
  unsigned int pos = 0;
  while(pos < data.size()) {
    const int length = static_cast<int>(data.toUInt(pos));
    ByteVector name = data.mid(pos + 4, 4);
    const int flags = static_cast<int>(data.toUInt(pos + 8));
    if(freeForm && i < 2) {
      if(i == 0 && name != "mean") {
        debug("MP4: Unexpected atom \"" + name + "\", expecting \"mean\"");
        return result;
      }
      else if(i == 1 && name != "name") {
        debug("MP4: Unexpected atom \"" + name + "\", expecting \"name\"");
        return result;
      }
      result.append(AtomData(AtomDataType(flags), data.mid(pos + 12, length - 12)));
    }
    else {
      if(name != "data") {
        debug("MP4: Unexpected atom \"" + name + "\", expecting \"data\"");
        return result;
      }
      if(expectedFlags == -1 || flags == expectedFlags) {
        result.append(AtomData(AtomDataType(flags), data.mid(pos + 16, length - 16)));
      }
    }
    pos += length;
    i++;
  }
  return result;
}

ByteVectorList
MP4::Tag::parseData(MP4::Atom *atom, TagLib::File *file, int expectedFlags, bool freeForm)
{
  AtomDataList data = parseData2(atom, file, expectedFlags, freeForm);
  ByteVectorList result;
  for(uint i = 0; i < data.size(); i++) {
    result.append(data[i].data);
  }
  return result;
}

void
MP4::Tag::parseInt(MP4::Atom *atom, TagLib::File *file)
{
  ByteVectorList data = parseData(atom, file);
  if(data.size()) {
    addItem(atom->name, (int)data[0].toShort());
  }
}

void
MP4::Tag::parseUInt(MP4::Atom *atom, TagLib::File *file)
{
  ByteVectorList data = parseData(atom, file);
  if(data.size()) {
    addItem(atom->name, data[0].toUInt());
  }
}

void
MP4::Tag::parseLongLong(MP4::Atom *atom, TagLib::File *file)
{
  ByteVectorList data = parseData(atom, file);
  if(data.size()) {
    addItem(atom->name, data[0].toLongLong());
  }
}

void
MP4::Tag::parseByte(MP4::Atom *atom, TagLib::File *file)
{
  ByteVectorList data = parseData(atom, file);
  if(data.size()) {
    addItem(atom->name, (uchar)data[0].at(0));
  }
}

void
MP4::Tag::parseGnre(MP4::Atom *atom, TagLib::File *file)
{
  ByteVectorList data = parseData(atom, file);
  if(data.size()) {
    int idx = (int)data[0].toShort();
    if(idx > 0) {
      addItem("\251gen", StringList(ID3v1::genre(idx - 1)));
    }
  }
}

void
MP4::Tag::parseIntPair(MP4::Atom *atom, TagLib::File *file)
{
  ByteVectorList data = parseData(atom, file);
  if(data.size()) {
    const int a = data[0].toShort(2U);
    const int b = data[0].toShort(4U);
    addItem(atom->name, MP4::Item(a, b));
  }
}

void
MP4::Tag::parseBool(MP4::Atom *atom, TagLib::File *file)
{
  ByteVectorList data = parseData(atom, file);
  if(data.size()) {
    bool value = data[0].size() ? data[0][0] != '\0' : false;
    addItem(atom->name, value);
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
    addItem(atom->name, value);
  }
}

void
MP4::Tag::parseFreeForm(MP4::Atom *atom, TagLib::File *file)
{
  AtomDataList data = parseData2(atom, file, -1, true);
  if(data.size() > 2) {
    String name = "----:" + String(data[0].data, String::UTF8) + ':' + String(data[1].data, String::UTF8);
    AtomDataType type = data[2].type;
    for(uint i = 2; i < data.size(); i++) {
      if(data[i].type != type) {
        debug("MP4: We currently don't support values with multiple types");
        break;
      }
    }
    if(type == TypeUTF8) {
      StringList value;
      for(uint i = 2; i < data.size(); i++) {
        value.append(String(data[i].data, String::UTF8));
      }
      Item item(value);
      item.setAtomDataType(type);
      addItem(name, item);
    }
    else {
      ByteVectorList value;
      for(uint i = 2; i < data.size(); i++) {
        value.append(data[i].data);
      }
      Item item(value);
      item.setAtomDataType(type);
      addItem(name, item);
    }
  }
}

void
MP4::Tag::parseCovr(MP4::Atom *atom, TagLib::File *file)
{
  MP4::CoverArtList value;
  ByteVector data = file->readBlock(atom->length - 8);
  unsigned int pos = 0;
  while(pos < data.size()) {
    const int length = static_cast<int>(data.toUInt(pos));
    ByteVector name = data.mid(pos + 4, 4);
    const int flags = static_cast<int>(data.toUInt(pos + 8));
    if(name != "data") {
      debug("MP4: Unexpected atom \"" + name + "\", expecting \"data\"");
      break;
    }
    if(flags == TypeJPEG || flags == TypePNG || flags == TypeBMP || flags == TypeGIF || flags == TypeImplicit) {
      value.append(MP4::CoverArt(MP4::CoverArt::Format(flags),
                                 data.mid(pos + 16, length - 16)));
    }
    else {
      debug("MP4: Unknown covr format " + String::number(flags));
    }
    pos += length;
  }
  if(value.size() > 0)
    addItem(atom->name, value);
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
  return renderData(name, TypeInteger, data);
}

ByteVector
MP4::Tag::renderInt(const ByteVector &name, MP4::Item &item)
{
  ByteVectorList data;
  data.append(ByteVector::fromShort(item.toInt()));
  return renderData(name, TypeInteger, data);
}

ByteVector
MP4::Tag::renderUInt(const ByteVector &name, MP4::Item &item)
{
  ByteVectorList data;
  data.append(ByteVector::fromUInt(item.toUInt()));
  return renderData(name, TypeInteger, data);
}

ByteVector
MP4::Tag::renderLongLong(const ByteVector &name, MP4::Item &item)
{
  ByteVectorList data;
  data.append(ByteVector::fromLongLong(item.toLongLong()));
  return renderData(name, TypeInteger, data);
}

ByteVector
MP4::Tag::renderByte(const ByteVector &name, MP4::Item &item)
{
  ByteVectorList data;
  data.append(ByteVector(1, item.toByte()));
  return renderData(name, TypeInteger, data);
}

ByteVector
MP4::Tag::renderIntPair(const ByteVector &name, MP4::Item &item)
{
  ByteVectorList data;
  data.append(ByteVector(2, '\0') +
              ByteVector::fromShort(item.toIntPair().first) +
              ByteVector::fromShort(item.toIntPair().second) +
              ByteVector(2, '\0'));
  return renderData(name, TypeImplicit, data);
}

ByteVector
MP4::Tag::renderIntPairNoTrailing(const ByteVector &name, MP4::Item &item)
{
  ByteVectorList data;
  data.append(ByteVector(2, '\0') +
              ByteVector::fromShort(item.toIntPair().first) +
              ByteVector::fromShort(item.toIntPair().second));
  return renderData(name, TypeImplicit, data);
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
MP4::Tag::renderCovr(const ByteVector &name, MP4::Item &item)
{
  ByteVector data;
  MP4::CoverArtList value = item.toCoverArtList();
  for(unsigned int i = 0; i < value.size(); i++) {
    data.append(renderAtom("data", ByteVector::fromUInt(value[i].format()) +
                                   ByteVector(4, '\0') + value[i].data()));
  }
  return renderAtom(name, data);
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
  AtomDataType type = item.atomDataType();
  if(type == TypeUndefined) {
    if(!item.toStringList().isEmpty()) {
      type = TypeUTF8;
    }
    else {
      type = TypeImplicit;
    }
  }
  if(type == TypeUTF8) {
    StringList value = item.toStringList();
    for(unsigned int i = 0; i < value.size(); i++) {
      data.append(renderAtom("data", ByteVector::fromUInt(type) + ByteVector(4, '\0') + value[i].data(String::UTF8)));
    }
  }
  else {
    ByteVectorList value = item.toByteVectorList();
    for(unsigned int i = 0; i < value.size(); i++) {
      data.append(renderAtom("data", ByteVector::fromUInt(type) + ByteVector(4, '\0') + value[i]));
    }
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
    else if(name == "cpil" || name == "pgap" || name == "pcst" || name == "hdvd") {
      data.append(renderBool(name.data(String::Latin1), i->second));
    }
    else if(name == "tmpo") {
      data.append(renderInt(name.data(String::Latin1), i->second));
    }
    else if(name == "tvsn" || name == "tves" || name == "cnID" ||
            name == "sfID" || name == "atID" || name == "geID") {
      data.append(renderUInt(name.data(String::Latin1), i->second));
    }
    else if(name == "plID") {
      data.append(renderLongLong(name.data(String::Latin1), i->second));
    }
    else if(name == "stik" || name == "rtng" || name == "akID") {
      data.append(renderByte(name.data(String::Latin1), i->second));
    }
    else if(name == "covr") {
      data.append(renderCovr(name.data(String::Latin1), i->second));
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
    long size = d->file->readBlock(4).toUInt();
    // 64-bit
    if (size == 1) {
      d->file->seek(4, File::Current); // Skip name
      long long longSize = d->file->readBlock(8).toLongLong();
      // Seek the offset of the 64-bit size
      d->file->seek(path[i]->offset + 8);
      d->file->writeBlock(ByteVector::fromLongLong(longSize + delta));
    }
    // 32-bit
    else {
      d->file->seek(path[i]->offset);
      d->file->writeBlock(ByteVector::fromUInt(size + delta));
    }
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
      unsigned int count = data.toUInt();
      d->file->seek(atom->offset + 16);
      uint pos = 4;
      while(count--) {
        long o = static_cast<long>(data.toUInt(pos));
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
      unsigned int count = data.toUInt();
      d->file->seek(atom->offset + 16);
      uint pos = 4;
      while(count--) {
        long long o = data.toLongLong(pos);
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
      ByteVector data = d->file->readBlock(atom->length - 9);
      const unsigned int flags = data.toUInt(0, 3, true);
      if(flags & 1) {
        long long o = data.toLongLong(7U);
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

  // check if there is an atom before 'ilst', and possibly use it as padding
  if(index != meta->children.begin()) {
    AtomList::Iterator prevIndex = index;
    prevIndex--;
    MP4::Atom *prev = *prevIndex;
    if(prev->name == "free") {
      offset = prev->offset;
      length += prev->length;
    }
  }
  // check if there is an atom after 'ilst', and possibly use it as padding
  AtomList::Iterator nextIndex = index;
  nextIndex++;
  if(nextIndex != meta->children.end()) {
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

static const char *keyTranslation[][2] = {
  { "\251nam", "TITLE" },
  { "\251ART", "ARTIST" },
  { "\251alb", "ALBUM" },
  { "\251cmt", "COMMENT" },
  { "\251gen", "GENRE" },
  { "\251day", "DATE" },
  { "\251wrt", "COMPOSER" },
  { "\251grp", "GROUPING" },
  { "trkn", "TRACKNUMBER" },
  { "disk", "DISCNUMBER" },
  { "cpil", "COMPILATION" },
  { "tmpo", "BPM" },
  { "cprt", "COPYRIGHT" },
  { "\251lyr", "LYRICS" },
  { "\251too", "ENCODEDBY" },
  { "soal", "ALBUMSORT" },
  { "soaa", "ALBUMARTISTSORT" },
  { "soar", "ARTISTSORT" },
  { "sonm", "TITLESORT" },
  { "soco", "COMPOSERSORT" },
  { "sosn", "SHOWSORT" },
  { "----:com.apple.iTunes:MusicBrainz Track Id", "MUSICBRAINZ_TRACKID" },
  { "----:com.apple.iTunes:MusicBrainz Artist Id", "MUSICBRAINZ_ARTISTID" },
  { "----:com.apple.iTunes:MusicBrainz Album Id", "MUSICBRAINZ_ALBUMID" },
  { "----:com.apple.iTunes:MusicBrainz Album Artist Id", "MUSICBRAINZ_ALBUMARTISTID" },
  { "----:com.apple.iTunes:MusicBrainz Release Group Id", "MUSICBRAINZ_RELEASEGROUPID" },
  { "----:com.apple.iTunes:MusicBrainz Work Id", "MUSICBRAINZ_WORKID" },
  { "----:com.apple.iTunes:ASIN", "ASIN" },
  { "----:com.apple.iTunes:LABEL", "LABEL" },
  { "----:com.apple.iTunes:LYRICIST", "LYRICIST" },
  { "----:com.apple.iTunes:CONDUCTOR", "CONDUCTOR" },
  { "----:com.apple.iTunes:REMIXER", "REMIXER" },
  { "----:com.apple.iTunes:ENGINEER", "ENGINEER" },
  { "----:com.apple.iTunes:PRODUCER", "PRODUCER" },
  { "----:com.apple.iTunes:DJMIXER", "DJMIXER" },
  { "----:com.apple.iTunes:MIXER", "MIXER" },
  { "----:com.apple.iTunes:SUBTITLE", "SUBTITLE" },
  { "----:com.apple.iTunes:DISCSUBTITLE", "DISCSUBTITLE" },
  { "----:com.apple.iTunes:MOOD", "MOOD" },
  { "----:com.apple.iTunes:ISRC", "ISRC" },
  { "----:com.apple.iTunes:CATALOGNUMBER", "CATALOGNUMBER" },
  { "----:com.apple.iTunes:BARCODE", "BARCODE" },
  { "----:com.apple.iTunes:SCRIPT", "SCRIPT" },
  { "----:com.apple.iTunes:LANGUAGE", "LANGUAGE" },
  { "----:com.apple.iTunes:LICENSE", "LICENSE" },
  { "----:com.apple.iTunes:MEDIA", "MEDIA" },
};

PropertyMap MP4::Tag::properties() const
{
  static Map<String, String> keyMap;
  if(keyMap.isEmpty()) {
    int numKeys = sizeof(keyTranslation) / sizeof(keyTranslation[0]);
    for(int i = 0; i < numKeys; i++) {
      keyMap[keyTranslation[i][0]] = keyTranslation[i][1];
    }
  }

  PropertyMap props;
  MP4::ItemListMap::ConstIterator it = d->items.begin();
  for(; it != d->items.end(); ++it) {
    if(keyMap.contains(it->first)) {
      String key = keyMap[it->first];
      if(key == "TRACKNUMBER" || key == "DISCNUMBER") {
        MP4::Item::IntPair ip = it->second.toIntPair();
        String value = String::number(ip.first);
        if(ip.second) {
          value += "/" + String::number(ip.second);
        }
        props[key] = value;
      }
      else if(key == "BPM") {
        props[key] = String::number(it->second.toInt());
      }
      else if(key == "COMPILATION") {
        props[key] = String::number(it->second.toBool());
      }
      else {
        props[key] = it->second.toStringList();
      }
    }
    else {
      props.unsupportedData().append(it->first);
    }
  }
  return props;
}

void MP4::Tag::removeUnsupportedProperties(const StringList &props)
{
  StringList::ConstIterator it = props.begin();
  for(; it != props.end(); ++it)
    d->items.erase(*it);
}

PropertyMap MP4::Tag::setProperties(const PropertyMap &props)
{
  static Map<String, String> reverseKeyMap;
  if(reverseKeyMap.isEmpty()) {
    int numKeys = sizeof(keyTranslation) / sizeof(keyTranslation[0]);
    for(int i = 0; i < numKeys; i++) {
      reverseKeyMap[keyTranslation[i][1]] = keyTranslation[i][0];
    }
  }

  PropertyMap origProps = properties();
  PropertyMap::ConstIterator it = origProps.begin();
  for(; it != origProps.end(); ++it) {
    if(!props.contains(it->first) || props[it->first].isEmpty()) {
      d->items.erase(reverseKeyMap[it->first]);
    }
  }

  PropertyMap ignoredProps;
  it = props.begin();
  for(; it != props.end(); ++it) {
    if(reverseKeyMap.contains(it->first)) {
      String name = reverseKeyMap[it->first];
      if(it->first == "TRACKNUMBER" || it->first == "DISCNUMBER") {
        int first = 0, second = 0;
        StringList parts = StringList::split(it->second.front(), "/");
        if(parts.size() > 0) {
          first = parts[0].toInt();
          if(parts.size() > 1) {
            second = parts[1].toInt();
          }
          d->items[name] = MP4::Item(first, second);
        }
      }
      else if(it->first == "BPM") {
        int value = it->second.front().toInt();
        d->items[name] = MP4::Item(value);
      }
      else if(it->first == "COMPILATION") {
        bool value = (it->second.front().toInt() != 0);
        d->items[name] = MP4::Item(value);
      }
      else {
        d->items[name] = it->second;
      }
    }
    else {
      ignoredProps.insert(it->first, it->second);
    }
  }

  return ignoredProps;
}

void MP4::Tag::addItem(const String &name, const Item &value)
{
  if(!d->items.contains(name)) {
    d->items.insert(name, value);
  }
  else {
    debug("MP4: Ignoring duplicate atom \"" + name + "\"");
  }
}
