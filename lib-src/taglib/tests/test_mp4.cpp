#include <cppunit/extensions/HelperMacros.h>
#include <string>
#include <stdio.h>
#include <tag.h>
#include <mp4tag.h>
#include <tbytevectorlist.h>
#include <mp4atom.h>
#include <mp4file.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestMP4 : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestMP4);
  CPPUNIT_TEST(testProperties);
  CPPUNIT_TEST(testFreeForm);
  CPPUNIT_TEST(testUpdateStco);
  CPPUNIT_TEST_SUITE_END();

public:

  void testProperties()
  {
    MP4::File f("data/has-tags.m4a");
    CPPUNIT_ASSERT_EQUAL(3, f.audioProperties()->length());
    CPPUNIT_ASSERT_EQUAL(3, f.audioProperties()->bitrate());
    CPPUNIT_ASSERT_EQUAL(2, f.audioProperties()->channels());
    CPPUNIT_ASSERT_EQUAL(44100, f.audioProperties()->sampleRate());
    CPPUNIT_ASSERT_EQUAL(16, ((MP4::Properties *)f.audioProperties())->bitsPerSample());
  }

  void testUpdateStco()
  {
    string filename = copyFile("no-tags", ".3g2");

    MP4::File *f = new MP4::File(filename.c_str());
    f->tag()->setArtist(ByteVector(3000, 'x'));

    ByteVectorList data1;
    {
      MP4::Atoms a(f);
      MP4::Atom *stco = a.find("moov")->findall("stco", true)[0];
      f->seek(stco->offset + 12);
      ByteVector data = f->readBlock(stco->length - 12);
      unsigned int count = data.mid(0, 4).toUInt();
      int pos = 4;
      while (count--) {
        unsigned int offset = data.mid(pos, 4).toUInt();
        f->seek(offset);
        data1.append(f->readBlock(20));
        pos += 4;
      }
    }

    f->save();
    delete f;
    f = new MP4::File(filename.c_str());

    {
      MP4::Atoms a(f);
      MP4::Atom *stco = a.find("moov")->findall("stco", true)[0];
      f->seek(stco->offset + 12);
      ByteVector data = f->readBlock(stco->length - 12);
      unsigned int count = data.mid(0, 4).toUInt();
      int pos = 4, i = 0;
      while (count--) {
        unsigned int offset = data.mid(pos, 4).toUInt();
        f->seek(offset);
        CPPUNIT_ASSERT_EQUAL(data1[i], f->readBlock(20));
        pos += 4;
        i++;
      }
    }

    delete f;

    deleteFile(filename);
  }

  void testFreeForm()
  {
    string filename = copyFile("has-tags", ".m4a");

    MP4::File *f = new MP4::File(filename.c_str());
    CPPUNIT_ASSERT(f->tag()->itemListMap().contains("----:com.apple.iTunes:iTunNORM"));
    f->tag()->itemListMap()["----:org.kde.TagLib:Foo"] = StringList("Bar");
    f->save();
    delete f;

    f = new MP4::File(filename.c_str());
    CPPUNIT_ASSERT(f->tag()->itemListMap().contains("----:org.kde.TagLib:Foo"));
    CPPUNIT_ASSERT_EQUAL(String("Bar"), f->tag()->itemListMap()["----:org.kde.TagLib:Foo"].toStringList()[0]);
    f->save();
    delete f;

    deleteFile(filename);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestMP4);
