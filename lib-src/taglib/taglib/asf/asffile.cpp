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
#include <tbytevectorlist.h>
#include <tstring.h>
#include "asffile.h"
#include "asftag.h"
#include "asfproperties.h"

using namespace TagLib;

class ASF::File::FilePrivate
{
public:
  FilePrivate():
    size(0),
    tag(0),
    properties(0),
    contentDescriptionObject(0),
    extendedContentDescriptionObject(0),
    headerExtensionObject(0),
    metadataObject(0),
    metadataLibraryObject(0) {}
  unsigned long long size;
  ASF::Tag *tag;
  ASF::Properties *properties;
  List<ASF::File::BaseObject *> objects;
  ASF::File::ContentDescriptionObject *contentDescriptionObject;
  ASF::File::ExtendedContentDescriptionObject *extendedContentDescriptionObject;
  ASF::File::HeaderExtensionObject *headerExtensionObject;
  ASF::File::MetadataObject *metadataObject;
  ASF::File::MetadataLibraryObject *metadataLibraryObject;
};

static ByteVector headerGuid("\x30\x26\xB2\x75\x8E\x66\xCF\x11\xA6\xD9\x00\xAA\x00\x62\xCE\x6C", 16);
static ByteVector filePropertiesGuid("\xA1\xDC\xAB\x8C\x47\xA9\xCF\x11\x8E\xE4\x00\xC0\x0C\x20\x53\x65", 16);
static ByteVector streamPropertiesGuid("\x91\x07\xDC\xB7\xB7\xA9\xCF\x11\x8E\xE6\x00\xC0\x0C\x20\x53\x65", 16);
static ByteVector contentDescriptionGuid("\x33\x26\xB2\x75\x8E\x66\xCF\x11\xA6\xD9\x00\xAA\x00\x62\xCE\x6C", 16);
static ByteVector extendedContentDescriptionGuid("\x40\xA4\xD0\xD2\x07\xE3\xD2\x11\x97\xF0\x00\xA0\xC9\x5E\xA8\x50", 16);
static ByteVector headerExtensionGuid("\xb5\x03\xbf_.\xa9\xcf\x11\x8e\xe3\x00\xc0\x0c Se", 16);
static ByteVector metadataGuid("\xEA\xCB\xF8\xC5\xAF[wH\204g\xAA\214D\xFAL\xCA", 16);
static ByteVector metadataLibraryGuid("\224\034#D\230\224\321I\241A\x1d\x13NEpT", 16);

class ASF::File::BaseObject
{
public:
  ByteVector data;
  virtual ~BaseObject() {}
  virtual ByteVector guid() = 0;
  virtual void parse(ASF::File *file, unsigned int size);
  virtual ByteVector render(ASF::File *file);
};

class ASF::File::UnknownObject : public ASF::File::BaseObject
{
  ByteVector myGuid;
public:
  UnknownObject(const ByteVector &guid);
  ByteVector guid();
};

class ASF::File::FilePropertiesObject : public ASF::File::BaseObject
{
public:
  ByteVector guid();
  void parse(ASF::File *file, uint size);
};

class ASF::File::StreamPropertiesObject : public ASF::File::BaseObject
{
public:
  ByteVector guid();
  void parse(ASF::File *file, uint size);
};

class ASF::File::ContentDescriptionObject : public ASF::File::BaseObject
{
public:
  ByteVector guid();
  void parse(ASF::File *file, uint size);
  ByteVector render(ASF::File *file);
};

class ASF::File::ExtendedContentDescriptionObject : public ASF::File::BaseObject
{
public:
  ByteVectorList attributeData;
  ByteVector guid();
  void parse(ASF::File *file, uint size);
  ByteVector render(ASF::File *file);
};

class ASF::File::MetadataObject : public ASF::File::BaseObject
{
public:
  ByteVectorList attributeData;
  ByteVector guid();
  void parse(ASF::File *file, uint size);
  ByteVector render(ASF::File *file);
};

class ASF::File::MetadataLibraryObject : public ASF::File::BaseObject
{
public:
  ByteVectorList attributeData;
  ByteVector guid();
  void parse(ASF::File *file, uint size);
  ByteVector render(ASF::File *file);
};

class ASF::File::HeaderExtensionObject : public ASF::File::BaseObject
{
public:
  List<ASF::File::BaseObject *> objects;
  ByteVector guid();
  void parse(ASF::File *file, uint size);
  ByteVector render(ASF::File *file);
};

void
ASF::File::BaseObject::parse(ASF::File *file, unsigned int size)
{
  data = file->readBlock(size - 24);
}

ByteVector
ASF::File::BaseObject::render(ASF::File * /*file*/)
{
  return guid() + ByteVector::fromLongLong(data.size() + 24, false) + data;
}

ASF::File::UnknownObject::UnknownObject(const ByteVector &guid) : myGuid(guid)
{
}

ByteVector
ASF::File::UnknownObject::guid()
{
  return myGuid;
}

ByteVector
ASF::File::FilePropertiesObject::guid()
{
  return filePropertiesGuid;
}

void
ASF::File::FilePropertiesObject::parse(ASF::File *file, uint size)
{
  BaseObject::parse(file, size);
  file->d->properties->setLength((int)(data.mid(40, 8).toLongLong(false) / 10000000L - data.mid(56, 8).toLongLong(false) / 1000L));
}

ByteVector
ASF::File::StreamPropertiesObject::guid()
{
  return streamPropertiesGuid;
}

void
ASF::File::StreamPropertiesObject::parse(ASF::File *file, uint size)
{
  BaseObject::parse(file, size);
  file->d->properties->setChannels(data.mid(56, 2).toShort(false));
  file->d->properties->setSampleRate(data.mid(58, 4).toUInt(false));
  file->d->properties->setBitrate(data.mid(62, 4).toUInt(false) * 8 / 1000);
}

ByteVector
ASF::File::ContentDescriptionObject::guid()
{
  return contentDescriptionGuid;
}

void
ASF::File::ContentDescriptionObject::parse(ASF::File *file, uint /*size*/)
{
  file->d->contentDescriptionObject = this;
  int titleLength = file->readWORD();
  int artistLength = file->readWORD();
  int copyrightLength = file->readWORD();
  int commentLength = file->readWORD();
  int ratingLength = file->readWORD();
  file->d->tag->setTitle(file->readString(titleLength));
  file->d->tag->setArtist(file->readString(artistLength));
  file->d->tag->setCopyright(file->readString(copyrightLength));
  file->d->tag->setComment(file->readString(commentLength));
  file->d->tag->setRating(file->readString(ratingLength));
}

ByteVector
ASF::File::ContentDescriptionObject::render(ASF::File *file)
{
  ByteVector v1 = file->renderString(file->d->tag->title());
  ByteVector v2 = file->renderString(file->d->tag->artist());
  ByteVector v3 = file->renderString(file->d->tag->copyright());
  ByteVector v4 = file->renderString(file->d->tag->comment());
  ByteVector v5 = file->renderString(file->d->tag->rating());
  data.clear();
  data.append(ByteVector::fromShort(v1.size(), false));
  data.append(ByteVector::fromShort(v2.size(), false));
  data.append(ByteVector::fromShort(v3.size(), false));
  data.append(ByteVector::fromShort(v4.size(), false));
  data.append(ByteVector::fromShort(v5.size(), false));
  data.append(v1);
  data.append(v2);
  data.append(v3);
  data.append(v4);
  data.append(v5);
  return BaseObject::render(file);
}

ByteVector
ASF::File::ExtendedContentDescriptionObject::guid()
{
  return extendedContentDescriptionGuid;
}

void
ASF::File::ExtendedContentDescriptionObject::parse(ASF::File *file, uint /*size*/)
{
  file->d->extendedContentDescriptionObject = this;
  int count = file->readWORD();
  while(count--) {
    ASF::Attribute attribute;
    String name = attribute.parse(*file);
    file->d->tag->addAttribute(name, attribute);
  }
}

ByteVector
ASF::File::ExtendedContentDescriptionObject::render(ASF::File *file)
{
  data.clear();
  data.append(ByteVector::fromShort(attributeData.size(), false));
  data.append(attributeData.toByteVector(ByteVector::null));
  return BaseObject::render(file);
}

ByteVector
ASF::File::MetadataObject::guid()
{
  return metadataGuid;
}

void
ASF::File::MetadataObject::parse(ASF::File *file, uint /*size*/)
{
  file->d->metadataObject = this;
  int count = file->readWORD();
  while(count--) {
    ASF::Attribute attribute;
    String name = attribute.parse(*file, 1);
    file->d->tag->addAttribute(name, attribute);
  }
}

ByteVector
ASF::File::MetadataObject::render(ASF::File *file)
{
  data.clear();
  data.append(ByteVector::fromShort(attributeData.size(), false));
  data.append(attributeData.toByteVector(ByteVector::null));
  return BaseObject::render(file);
}

ByteVector
ASF::File::MetadataLibraryObject::guid()
{
  return metadataLibraryGuid;
}

void
ASF::File::MetadataLibraryObject::parse(ASF::File *file, uint /*size*/)
{
  file->d->metadataLibraryObject = this;
  int count = file->readWORD();
  while(count--) {
    ASF::Attribute attribute;
    String name = attribute.parse(*file, 2);
    file->d->tag->addAttribute(name, attribute);
  }
}

ByteVector
ASF::File::MetadataLibraryObject::render(ASF::File *file)
{
  data.clear();
  data.append(ByteVector::fromShort(attributeData.size(), false));
  data.append(attributeData.toByteVector(ByteVector::null));
  return BaseObject::render(file);
}

ByteVector
ASF::File::HeaderExtensionObject::guid()
{
  return headerExtensionGuid;
}

void
ASF::File::HeaderExtensionObject::parse(ASF::File *file, uint /*size*/)
{
  file->d->headerExtensionObject = this;
  file->seek(18, File::Current);
  long long dataSize = file->readDWORD();
  long long dataPos = 0;
  while(dataPos < dataSize) {
    ByteVector guid = file->readBlock(16);
    long long size = file->readQWORD();
    BaseObject *obj;
    if(guid == metadataGuid) {
      obj = new MetadataObject();
    }
    else if(guid == metadataLibraryGuid) {
      obj = new MetadataLibraryObject();
    }
    else {
      obj = new UnknownObject(guid);
    }
    obj->parse(file, size);
    objects.append(obj);
    dataPos += size;
  }
}

ByteVector
ASF::File::HeaderExtensionObject::render(ASF::File *file)
{
  data.clear();
  for(unsigned int i = 0; i < objects.size(); i++) {
    data.append(objects[i]->render(file));
  }
  data = ByteVector("\x11\xD2\xD3\xAB\xBA\xA9\xcf\x11\x8E\xE6\x00\xC0\x0C\x20\x53\x65\x06\x00", 18) + ByteVector::fromUInt(data.size(), false) + data;
  return BaseObject::render(file);
}

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

ASF::File::File(FileName file, bool readProperties, Properties::ReadStyle propertiesStyle) 
  : TagLib::File(file)
{
  d = new FilePrivate;
  read(readProperties, propertiesStyle);
}

ASF::File::~File()
{
  for(unsigned int i = 0; i < d->objects.size(); i++) {
    delete d->objects[i];
  }
  if(d->tag) {
    delete d->tag;
  }
  if(d->properties) {
    delete d->properties;
  }
  delete d;
}

ASF::Tag *ASF::File::tag() const
{
  return d->tag;
}

ASF::Properties *ASF::File::audioProperties() const
{
  return d->properties;
}

void ASF::File::read(bool /*readProperties*/, Properties::ReadStyle /*propertiesStyle*/)
{
  if(!isValid())
    return;

  ByteVector guid = readBlock(16);
  if(guid != headerGuid) {
    debug("ASF: Not an ASF file.");
    return;
  }

  d->tag = new ASF::Tag();
  d->properties = new ASF::Properties();

  d->size = readQWORD();
  int numObjects = readDWORD();
  seek(2, Current);

  for(int i = 0; i < numObjects; i++) {
    ByteVector guid = readBlock(16);
    long size = (long)readQWORD();
    BaseObject *obj;
    if(guid == filePropertiesGuid) {
      obj = new FilePropertiesObject();
    }
    else if(guid == streamPropertiesGuid) {
      obj = new StreamPropertiesObject();
    }
    else if(guid == contentDescriptionGuid) {
      obj = new ContentDescriptionObject();
    }
    else if(guid == extendedContentDescriptionGuid) {
      obj = new ExtendedContentDescriptionObject();
    }
    else if(guid == headerExtensionGuid) {
      obj = new HeaderExtensionObject();
    }
    else {
      obj = new UnknownObject(guid);
    }
    obj->parse(this, size);
    d->objects.append(obj);
  }
}

bool ASF::File::save()
{
  if(readOnly()) {
    debug("ASF: File is read-only.");
    return false;
  }

  if(!d->contentDescriptionObject) {
    d->contentDescriptionObject = new ContentDescriptionObject();
    d->objects.append(d->contentDescriptionObject);
  }
  if(!d->extendedContentDescriptionObject) {
    d->extendedContentDescriptionObject = new ExtendedContentDescriptionObject();
    d->objects.append(d->extendedContentDescriptionObject);
  }
  if(!d->headerExtensionObject) {
    d->headerExtensionObject = new HeaderExtensionObject();
    d->objects.append(d->headerExtensionObject);
  }
  if(!d->metadataObject) {
    d->metadataObject = new MetadataObject();
    d->headerExtensionObject->objects.append(d->metadataObject);
  }
  if(!d->metadataLibraryObject) {
    d->metadataLibraryObject = new MetadataLibraryObject();
    d->headerExtensionObject->objects.append(d->metadataLibraryObject);
  }

  ASF::AttributeListMap::ConstIterator it = d->tag->attributeListMap().begin();
  for(; it != d->tag->attributeListMap().end(); it++) {
    const String &name = it->first;
    const AttributeList &attributes = it->second;
    bool inExtendedContentDescriptionObject = false;
    bool inMetadataObject = false;
    for(unsigned int j = 0; j < attributes.size(); j++) {
      const Attribute &attribute = attributes[j];
      if(!inExtendedContentDescriptionObject && attribute.language() == 0 && attribute.stream() == 0) {
        d->extendedContentDescriptionObject->attributeData.append(attribute.render(name));
        inExtendedContentDescriptionObject = true;
      }
      else if(!inMetadataObject && attribute.language() == 0 && attribute.stream() != 0) {
        d->metadataObject->attributeData.append(attribute.render(name, 1));
        inMetadataObject = true;
      }
      else {
        d->metadataLibraryObject->attributeData.append(attribute.render(name, 2));
      }
    }
  }

  ByteVector data;
  for(unsigned int i = 0; i < d->objects.size(); i++) {
    data.append(d->objects[i]->render(this));
  }
  data = headerGuid + ByteVector::fromLongLong(data.size() + 30, false) + ByteVector::fromUInt(d->objects.size(), false) + ByteVector("\x01\x02", 2) + data;
  insert(data, 0, d->size);

  return true;
}

////////////////////////////////////////////////////////////////////////////////
// protected members
////////////////////////////////////////////////////////////////////////////////

int ASF::File::readBYTE()
{
  ByteVector v = readBlock(1);
  return v[0];
}

int ASF::File::readWORD()
{
  ByteVector v = readBlock(2);
  return v.toShort(false);
}

unsigned int ASF::File::readDWORD()
{
  ByteVector v = readBlock(4);
  return v.toUInt(false);
}

long long ASF::File::readQWORD()
{
  ByteVector v = readBlock(8);
  return v.toLongLong(false);
}

String
ASF::File::readString(int length)
{
  ByteVector data = readBlock(length);
  unsigned int size = data.size();
  while (size >= 2) {
    if(data[size - 1] != '\0' || data[size - 2] != '\0') {
      break;
    }
    size -= 2;
  }
  if(size != data.size()) {
    data.resize(size);
  }
  return String(data, String::UTF16LE);
}

ByteVector
ASF::File::renderString(const String &str, bool includeLength)
{
  ByteVector data = str.data(String::UTF16LE) + ByteVector::fromShort(0, false);
  if(includeLength) {
    data = ByteVector::fromShort(data.size(), false) + data;
  }
  return data;
}

#endif
