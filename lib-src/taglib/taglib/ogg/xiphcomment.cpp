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

#include <tbytevector.h>
#include <tdebug.h>

#include <xiphcomment.h>
#include <tpropertymap.h>

using namespace TagLib;

class Ogg::XiphComment::XiphCommentPrivate
{
public:
  FieldListMap fieldListMap;
  String vendorID;
  String commentField;
};

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

Ogg::XiphComment::XiphComment() : TagLib::Tag()
{
  d = new XiphCommentPrivate;
}

Ogg::XiphComment::XiphComment(const ByteVector &data) : TagLib::Tag()
{
  d = new XiphCommentPrivate;
  parse(data);
}

Ogg::XiphComment::~XiphComment()
{
  delete d;
}

String Ogg::XiphComment::title() const
{
  if(d->fieldListMap["TITLE"].isEmpty())
    return String::null;
  return d->fieldListMap["TITLE"].front();
}

String Ogg::XiphComment::artist() const
{
  if(d->fieldListMap["ARTIST"].isEmpty())
    return String::null;
  return d->fieldListMap["ARTIST"].front();
}

String Ogg::XiphComment::album() const
{
  if(d->fieldListMap["ALBUM"].isEmpty())
    return String::null;
  return d->fieldListMap["ALBUM"].front();
}

String Ogg::XiphComment::comment() const
{
  if(!d->fieldListMap["DESCRIPTION"].isEmpty()) {
    d->commentField = "DESCRIPTION";
    return d->fieldListMap["DESCRIPTION"].front();
  }

  if(!d->fieldListMap["COMMENT"].isEmpty()) {
    d->commentField = "COMMENT";
    return d->fieldListMap["COMMENT"].front();
  }

  return String::null;
}

String Ogg::XiphComment::genre() const
{
  if(d->fieldListMap["GENRE"].isEmpty())
    return String::null;
  return d->fieldListMap["GENRE"].front();
}

TagLib::uint Ogg::XiphComment::year() const
{
  if(!d->fieldListMap["DATE"].isEmpty())
    return d->fieldListMap["DATE"].front().toInt();
  if(!d->fieldListMap["YEAR"].isEmpty())
    return d->fieldListMap["YEAR"].front().toInt();
  return 0;
}

TagLib::uint Ogg::XiphComment::track() const
{
  if(!d->fieldListMap["TRACKNUMBER"].isEmpty())
    return d->fieldListMap["TRACKNUMBER"].front().toInt();
  if(!d->fieldListMap["TRACKNUM"].isEmpty())
    return d->fieldListMap["TRACKNUM"].front().toInt();
  return 0;
}

void Ogg::XiphComment::setTitle(const String &s)
{
  addField("TITLE", s);
}

void Ogg::XiphComment::setArtist(const String &s)
{
  addField("ARTIST", s);
}

void Ogg::XiphComment::setAlbum(const String &s)
{
  addField("ALBUM", s);
}

void Ogg::XiphComment::setComment(const String &s)
{
  addField(d->commentField.isEmpty() ? "DESCRIPTION" : d->commentField, s);
}

void Ogg::XiphComment::setGenre(const String &s)
{
  addField("GENRE", s);
}

void Ogg::XiphComment::setYear(uint i)
{
  removeField("YEAR");
  if(i == 0)
    removeField("DATE");
  else
    addField("DATE", String::number(i));
}

void Ogg::XiphComment::setTrack(uint i)
{
  removeField("TRACKNUM");
  if(i == 0)
    removeField("TRACKNUMBER");
  else
    addField("TRACKNUMBER", String::number(i));
}

bool Ogg::XiphComment::isEmpty() const
{
  FieldListMap::ConstIterator it = d->fieldListMap.begin();
  for(; it != d->fieldListMap.end(); ++it)
    if(!(*it).second.isEmpty())
      return false;

  return true;
}

TagLib::uint Ogg::XiphComment::fieldCount() const
{
  uint count = 0;

  FieldListMap::ConstIterator it = d->fieldListMap.begin();
  for(; it != d->fieldListMap.end(); ++it)
    count += (*it).second.size();

  return count;
}

const Ogg::FieldListMap &Ogg::XiphComment::fieldListMap() const
{
  return d->fieldListMap;
}

PropertyMap Ogg::XiphComment::properties() const
{
  return d->fieldListMap;
}

PropertyMap Ogg::XiphComment::setProperties(const PropertyMap &properties)
{
  // check which keys are to be deleted
  StringList toRemove;
  for(FieldListMap::ConstIterator it = d->fieldListMap.begin(); it != d->fieldListMap.end(); ++it)
    if (!properties.contains(it->first))
      toRemove.append(it->first);

  for(StringList::ConstIterator it = toRemove.begin(); it != toRemove.end(); ++it)
      removeField(*it);

  // now go through keys in \a properties and check that the values match those in the xiph comment
  PropertyMap invalid;
  PropertyMap::ConstIterator it = properties.begin();
  for(; it != properties.end(); ++it)
  {
    if(!checkKey(it->first))
      invalid.insert(it->first, it->second);
    else if(!d->fieldListMap.contains(it->first) || !(it->second == d->fieldListMap[it->first])) {
      const StringList &sl = it->second;
      if(sl.size() == 0)
        // zero size string list -> remove the tag with all values
        removeField(it->first);
      else {
        // replace all strings in the list for the tag
        StringList::ConstIterator valueIterator = sl.begin();
        addField(it->first, *valueIterator, true);
        ++valueIterator;
        for(; valueIterator != sl.end(); ++valueIterator)
          addField(it->first, *valueIterator, false);
      }
    }
  }
  return invalid;
}

bool Ogg::XiphComment::checkKey(const String &key)
{
  if(key.size() < 1)
    return false;
  for(String::ConstIterator it = key.begin(); it != key.end(); it++)
      // forbid non-printable, non-ascii, '=' (#61) and '~' (#126)
      if (*it < 32 || *it >= 128 || *it == 61 || *it == 126)
        return false;
  return true;
}

String Ogg::XiphComment::vendorID() const
{
  return d->vendorID;
}

void Ogg::XiphComment::addField(const String &key, const String &value, bool replace)
{
  if(replace)
    removeField(key.upper());

  if(!key.isEmpty() && !value.isEmpty())
    d->fieldListMap[key.upper()].append(value);
}

void Ogg::XiphComment::removeField(const String &key, const String &value)
{
  if(!value.isNull()) {
    StringList::Iterator it = d->fieldListMap[key].begin();
    while(it != d->fieldListMap[key].end()) {
      if(value == *it)
        it = d->fieldListMap[key].erase(it);
      else
        it++;
    }
  }
  else
    d->fieldListMap.erase(key);
}

bool Ogg::XiphComment::contains(const String &key) const
{
  return d->fieldListMap.contains(key) && !d->fieldListMap[key].isEmpty();
}

ByteVector Ogg::XiphComment::render() const
{
  return render(true);
}

ByteVector Ogg::XiphComment::render(bool addFramingBit) const
{
  ByteVector data;

  // Add the vendor ID length and the vendor ID.  It's important to use the
  // length of the data(String::UTF8) rather than the length of the the string
  // since this is UTF8 text and there may be more characters in the data than
  // in the UTF16 string.

  ByteVector vendorData = d->vendorID.data(String::UTF8);

  data.append(ByteVector::fromUInt(vendorData.size(), false));
  data.append(vendorData);

  // Add the number of fields.

  data.append(ByteVector::fromUInt(fieldCount(), false));

  // Iterate over the the field lists.  Our iterator returns a
  // std::pair<String, StringList> where the first String is the field name and
  // the StringList is the values associated with that field.

  FieldListMap::ConstIterator it = d->fieldListMap.begin();
  for(; it != d->fieldListMap.end(); ++it) {

    // And now iterate over the values of the current list.

    String fieldName = (*it).first;
    StringList values = (*it).second;

    StringList::ConstIterator valuesIt = values.begin();
    for(; valuesIt != values.end(); ++valuesIt) {
      ByteVector fieldData = fieldName.data(String::UTF8);
      fieldData.append('=');
      fieldData.append((*valuesIt).data(String::UTF8));

      data.append(ByteVector::fromUInt(fieldData.size(), false));
      data.append(fieldData);
    }
  }

  // Append the "framing bit".

  if(addFramingBit)
    data.append(char(1));

  return data;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

void Ogg::XiphComment::parse(const ByteVector &data)
{
  // The first thing in the comment data is the vendor ID length, followed by a
  // UTF8 string with the vendor ID.

  uint pos = 0;

  const uint vendorLength = data.toUInt(0, false);
  pos += 4;

  d->vendorID = String(data.mid(pos, vendorLength), String::UTF8);
  pos += vendorLength;

  // Next the number of fields in the comment vector.

  const uint commentFields = data.toUInt(pos, false);
  pos += 4;

  if(commentFields > (data.size() - 8) / 4) {
    return;
  }

  for(uint i = 0; i < commentFields; i++) {

    // Each comment field is in the format "KEY=value" in a UTF8 string and has
    // 4 bytes before the text starts that gives the length.

    const uint commentLength = data.toUInt(pos, false);
    pos += 4;

    String comment = String(data.mid(pos, commentLength), String::UTF8);
    pos += commentLength;
    if(pos > data.size()) {
      break;
    }

    int commentSeparatorPosition = comment.find("=");
    if(commentSeparatorPosition == -1) {
      break;
    }

    String key = comment.substr(0, commentSeparatorPosition);
    String value = comment.substr(commentSeparatorPosition + 1);

    addField(key, value, false);
  }
}
