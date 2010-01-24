#include <cppunit/extensions/HelperMacros.h>
#include <string>
#include <stdio.h>
#include <tag.h>
#include <tstringlist.h>
#include <tbytevectorlist.h>
#include <asffile.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestASF : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestASF);
  CPPUNIT_TEST(testProperties);
  CPPUNIT_TEST(testRead);
  CPPUNIT_TEST(testSaveMultipleValues);
  CPPUNIT_TEST(testSaveStream);
  CPPUNIT_TEST(testSaveLanguage);
  CPPUNIT_TEST_SUITE_END();

public:

  void testProperties()
  {
    ASF::File f("data/silence-1.wma");
    CPPUNIT_ASSERT_EQUAL(4, f.audioProperties()->length());
    CPPUNIT_ASSERT_EQUAL(64, f.audioProperties()->bitrate());
    CPPUNIT_ASSERT_EQUAL(2, f.audioProperties()->channels());
    CPPUNIT_ASSERT_EQUAL(48000, f.audioProperties()->sampleRate());
  }

  void testRead()
  {
    ASF::File f("data/silence-1.wma");
    CPPUNIT_ASSERT_EQUAL(String("test"), f.tag()->title());
  }

  void testSaveMultipleValues()
  {
    string newname = copyFile("silence-1", ".wma");

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

    deleteFile(newname);
  }

  void testSaveStream()
  {
    string newname = copyFile("silence-1", ".wma");

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

    deleteFile(newname);
  }

  void testSaveLanguage()
  {
    string newname = copyFile("silence-1", ".wma");

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

    deleteFile(newname);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestASF);
