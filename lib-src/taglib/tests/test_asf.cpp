#include <string>
#include <stdio.h>
#include <tag.h>
#include <tstringlist.h>
#include <tbytevectorlist.h>
#include <tpropertymap.h>
#include <asffile.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestASF : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestASF);
  CPPUNIT_TEST(testAudioProperties);
  CPPUNIT_TEST(testRead);
  CPPUNIT_TEST(testSaveMultipleValues);
  CPPUNIT_TEST(testSaveStream);
  CPPUNIT_TEST(testSaveLanguage);
  CPPUNIT_TEST(testDWordTrackNumber);
  CPPUNIT_TEST(testSaveLargeValue);
  CPPUNIT_TEST(testSavePicture);
  CPPUNIT_TEST(testSaveMultiplePictures);
  CPPUNIT_TEST(testProperties);
  CPPUNIT_TEST_SUITE_END();

public:

  void testAudioProperties()
  {
    ASF::File f(TEST_FILE_PATH_C("silence-1.wma"));
    CPPUNIT_ASSERT_EQUAL(4, f.audioProperties()->length());
    CPPUNIT_ASSERT_EQUAL(64, f.audioProperties()->bitrate());
    CPPUNIT_ASSERT_EQUAL(2, f.audioProperties()->channels());
    CPPUNIT_ASSERT_EQUAL(48000, f.audioProperties()->sampleRate());
  }

  void testRead()
  {
    ASF::File f(TEST_FILE_PATH_C("silence-1.wma"));
    CPPUNIT_ASSERT_EQUAL(String("test"), f.tag()->title());
  }

  void testSaveMultipleValues()
  {
    ScopedFileCopy copy("silence-1", ".wma");
    string newname = copy.fileName();

    ASF::File *f = new ASF::File(newname.c_str());
    ASF::AttributeList values;
    values.append("Foo");
    values.append("Bar");
    f->tag()->attributeListMap()["WM/AlbumTitle"] = values;
    f->save();
    delete f;

    f = new ASF::File(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(2, (int)f->tag()->attributeListMap()["WM/AlbumTitle"].size());
    delete f;
  }

  void testDWordTrackNumber()
  {
    ScopedFileCopy copy("silence-1", ".wma");
    string newname = copy.fileName();

    ASF::File *f = new ASF::File(newname.c_str());
    CPPUNIT_ASSERT(!f->tag()->attributeListMap().contains("WM/TrackNumber"));
    f->tag()->setAttribute("WM/TrackNumber", (unsigned int)(123));
    f->save();
    delete f;

    f = new ASF::File(newname.c_str());
    CPPUNIT_ASSERT(f->tag()->attributeListMap().contains("WM/TrackNumber"));
    CPPUNIT_ASSERT_EQUAL(ASF::Attribute::DWordType, f->tag()->attributeListMap()["WM/TrackNumber"].front().type());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(123), f->tag()->track());
    f->tag()->setTrack(234);
    f->save();
    delete f;

    f = new ASF::File(newname.c_str());
    CPPUNIT_ASSERT(f->tag()->attributeListMap().contains("WM/TrackNumber"));
    CPPUNIT_ASSERT_EQUAL(ASF::Attribute::UnicodeType, f->tag()->attributeListMap()["WM/TrackNumber"].front().type());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(234), f->tag()->track());
    delete f;
  }

  void testSaveStream()
  {
    ScopedFileCopy copy("silence-1", ".wma");
    string newname = copy.fileName();

    ASF::File *f = new ASF::File(newname.c_str());
    ASF::AttributeList values;
    ASF::Attribute attr("Foo");
    attr.setStream(43);
    values.append(attr);
    f->tag()->attributeListMap()["WM/AlbumTitle"] = values;
    f->save();
    delete f;

    f = new ASF::File(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(43, f->tag()->attributeListMap()["WM/AlbumTitle"][0].stream());
    delete f;
  }

  void testSaveLanguage()
  {
    ScopedFileCopy copy("silence-1", ".wma");
    string newname = copy.fileName();

    ASF::File *f = new ASF::File(newname.c_str());
    ASF::AttributeList values;
    ASF::Attribute attr("Foo");
    attr.setStream(32);
    attr.setLanguage(56);
    values.append(attr);
    f->tag()->attributeListMap()["WM/AlbumTitle"] = values;
    f->save();
    delete f;

    f = new ASF::File(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(32, f->tag()->attributeListMap()["WM/AlbumTitle"][0].stream());
    CPPUNIT_ASSERT_EQUAL(56, f->tag()->attributeListMap()["WM/AlbumTitle"][0].language());
    delete f;
  }

  void testSaveLargeValue()
  {
    ScopedFileCopy copy("silence-1", ".wma");
    string newname = copy.fileName();

    ASF::File *f = new ASF::File(newname.c_str());
    ASF::AttributeList values;
    ASF::Attribute attr(ByteVector(70000, 'x'));
    values.append(attr);
    f->tag()->attributeListMap()["WM/Blob"] = values;
    f->save();
    delete f;

    f = new ASF::File(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(ByteVector(70000, 'x'), f->tag()->attributeListMap()["WM/Blob"][0].toByteVector());
    delete f;
  }

  void testSavePicture()
  {
    ScopedFileCopy copy("silence-1", ".wma");
    string newname = copy.fileName();

    ASF::File *f = new ASF::File(newname.c_str());
    ASF::AttributeList values;
    ASF::Picture picture;
    picture.setMimeType("image/jpeg");
    picture.setType(ASF::Picture::FrontCover);
    picture.setDescription("description");
    picture.setPicture("data");
    ASF::Attribute attr(picture);
    values.append(attr);
    f->tag()->attributeListMap()["WM/Picture"] = values;
    f->save();
    delete f;

    f = new ASF::File(newname.c_str());
    ASF::AttributeList values2 = f->tag()->attributeListMap()["WM/Picture"];
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), values2.size());
    ASF::Attribute attr2 = values2.front();
    ASF::Picture picture2 = attr2.toPicture();
    CPPUNIT_ASSERT(picture2.isValid());
    CPPUNIT_ASSERT_EQUAL(String("image/jpeg"), picture2.mimeType());
    CPPUNIT_ASSERT_EQUAL(ASF::Picture::FrontCover, picture2.type());
    CPPUNIT_ASSERT_EQUAL(String("description"), picture2.description());
    CPPUNIT_ASSERT_EQUAL(ByteVector("data"), picture2.picture());
    delete f;
  }

  void testSaveMultiplePictures()
  {
    ScopedFileCopy copy("silence-1", ".wma");
    string newname = copy.fileName();

    ASF::File *f = new ASF::File(newname.c_str());
    ASF::AttributeList values;
    ASF::Picture picture;
    picture.setMimeType("image/jpeg");
    picture.setType(ASF::Picture::FrontCover);
    picture.setDescription("description");
    picture.setPicture("data");
    values.append(ASF::Attribute(picture));
    ASF::Picture picture2;
    picture2.setMimeType("image/png");
    picture2.setType(ASF::Picture::BackCover);
    picture2.setDescription("back cover");
    picture2.setPicture("PNG data");
    values.append(ASF::Attribute(picture2));
    f->tag()->attributeListMap()["WM/Picture"] = values;
    f->save();
    delete f;

    f = new ASF::File(newname.c_str());
    ASF::AttributeList values2 = f->tag()->attributeListMap()["WM/Picture"];
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2), values2.size());
    ASF::Picture picture3 = values2[1].toPicture();
    CPPUNIT_ASSERT(picture3.isValid());
    CPPUNIT_ASSERT_EQUAL(String("image/jpeg"), picture3.mimeType());
    CPPUNIT_ASSERT_EQUAL(ASF::Picture::FrontCover, picture3.type());
    CPPUNIT_ASSERT_EQUAL(String("description"), picture3.description());
    CPPUNIT_ASSERT_EQUAL(ByteVector("data"), picture3.picture());
    ASF::Picture picture4 = values2[0].toPicture();
    CPPUNIT_ASSERT(picture4.isValid());
    CPPUNIT_ASSERT_EQUAL(String("image/png"), picture4.mimeType());
    CPPUNIT_ASSERT_EQUAL(ASF::Picture::BackCover, picture4.type());
    CPPUNIT_ASSERT_EQUAL(String("back cover"), picture4.description());
    CPPUNIT_ASSERT_EQUAL(ByteVector("PNG data"), picture4.picture());
    delete f;
  }

  void testProperties()
  {
    ASF::File f(TEST_FILE_PATH_C("silence-1.wma"));
    
    PropertyMap tags = f.properties();

    tags["TRACKNUMBER"] = StringList("2");
    tags["DISCNUMBER"] = StringList("3");
    tags["BPM"] = StringList("123");
    tags["ARTIST"] = StringList("Foo Bar");
    f.setProperties(tags);

    tags = f.properties();

    CPPUNIT_ASSERT_EQUAL(String("Foo Bar"), f.tag()->artist());
    CPPUNIT_ASSERT_EQUAL(StringList("Foo Bar"), tags["ARTIST"]);

    CPPUNIT_ASSERT(f.tag()->attributeListMap().contains("WM/BeatsPerMinute"));
    CPPUNIT_ASSERT_EQUAL(1u, f.tag()->attributeListMap()["WM/BeatsPerMinute"].size());
    CPPUNIT_ASSERT_EQUAL(String("123"), f.tag()->attributeListMap()["WM/BeatsPerMinute"].front().toString());
    CPPUNIT_ASSERT_EQUAL(StringList("123"), tags["BPM"]);

    CPPUNIT_ASSERT(f.tag()->attributeListMap().contains("WM/TrackNumber"));
    CPPUNIT_ASSERT_EQUAL(1u, f.tag()->attributeListMap()["WM/TrackNumber"].size());
    CPPUNIT_ASSERT_EQUAL(String("2"), f.tag()->attributeListMap()["WM/TrackNumber"].front().toString());
    CPPUNIT_ASSERT_EQUAL(StringList("2"), tags["TRACKNUMBER"]);

    CPPUNIT_ASSERT(f.tag()->attributeListMap().contains("WM/PartOfSet"));
    CPPUNIT_ASSERT_EQUAL(1u, f.tag()->attributeListMap()["WM/PartOfSet"].size());
    CPPUNIT_ASSERT_EQUAL(String("3"), f.tag()->attributeListMap()["WM/PartOfSet"].front().toString());
    CPPUNIT_ASSERT_EQUAL(StringList("3"), tags["DISCNUMBER"]);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestASF);
