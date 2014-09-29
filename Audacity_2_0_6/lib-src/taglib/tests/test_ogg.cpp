#include <string>
#include <stdio.h>
#include <tag.h>
#include <tstringlist.h>
#include <tbytevectorlist.h>
#include <tpropertymap.h>
#include <oggfile.h>
#include <vorbisfile.h>
#include <oggpageheader.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestOGG : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestOGG);
  CPPUNIT_TEST(testSimple);
  CPPUNIT_TEST(testSplitPackets);
  CPPUNIT_TEST(testDictInterface1);
  CPPUNIT_TEST(testDictInterface2);
  CPPUNIT_TEST_SUITE_END();

public:

  void testSimple()
  {
    ScopedFileCopy copy("empty", ".ogg");
    string newname = copy.fileName();

    Vorbis::File *f = new Vorbis::File(newname.c_str());
    f->tag()->setArtist("The Artist");
    f->save();
    delete f;

    f = new Vorbis::File(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(String("The Artist"), f->tag()->artist());
    delete f;
  }

  void testSplitPackets()
  {
    ScopedFileCopy copy("empty", ".ogg");
    string newname = copy.fileName();

    Vorbis::File *f = new Vorbis::File(newname.c_str());
    f->tag()->addField("test", ByteVector(128 * 1024, 'x') + ByteVector(1, '\0'));
    f->save();
    delete f;

    f = new Vorbis::File(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(19, f->lastPageHeader()->pageSequenceNumber());
    delete f;
  }

  void testDictInterface1()
  {
    ScopedFileCopy copy("empty", ".ogg");
    string newname = copy.fileName();

    Vorbis::File *f = new Vorbis::File(newname.c_str());

    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0), f->tag()->properties().size());

    PropertyMap newTags;
    StringList values("value 1");
    values.append("value 2");
    newTags["ARTIST"] = values;
    f->tag()->setProperties(newTags);

    PropertyMap map = f->tag()->properties();
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), map.size());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2), map["ARTIST"].size());
    CPPUNIT_ASSERT_EQUAL(String("value 1"), map["ARTIST"][0]);
    delete f;

  }

  void testDictInterface2()
  {
    ScopedFileCopy copy("test", ".ogg");
    string newname = copy.fileName();

    Vorbis::File *f = new Vorbis::File(newname.c_str());
    PropertyMap tags = f->tag()->properties();

    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2), tags["UNUSUALTAG"].size());
    CPPUNIT_ASSERT_EQUAL(String("usual value"), tags["UNUSUALTAG"][0]);
    CPPUNIT_ASSERT_EQUAL(String("another value"), tags["UNUSUALTAG"][1]);
    CPPUNIT_ASSERT_EQUAL(String("öäüoΣø", String::UTF8), tags["UNICODETAG"][0]);

    tags["UNICODETAG"][0] = String("νεω ναλυε", String::UTF8);
    tags.erase("UNUSUALTAG");
    f->tag()->setProperties(tags);
    CPPUNIT_ASSERT_EQUAL(String("νεω ναλυε", String::UTF8), f->tag()->properties()["UNICODETAG"][0]);
    CPPUNIT_ASSERT_EQUAL(false, f->tag()->properties().contains("UNUSUALTAG"));

    delete f;
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestOGG);
