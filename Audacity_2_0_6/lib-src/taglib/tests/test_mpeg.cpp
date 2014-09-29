#include <string>
#include <stdio.h>
#include <tstring.h>
#include <mpegfile.h>
#include <id3v2tag.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestMPEG : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestMPEG);
  CPPUNIT_TEST(testVersion2DurationWithXingHeader);
  CPPUNIT_TEST(testSaveID3v24);
  CPPUNIT_TEST(testSaveID3v24WrongParam);
  CPPUNIT_TEST(testSaveID3v23);
  CPPUNIT_TEST_SUITE_END();

public:

  void testVersion2DurationWithXingHeader()
  {
    MPEG::File f(TEST_FILE_PATH_C("mpeg2.mp3"));
    CPPUNIT_ASSERT_EQUAL(5387, f.audioProperties()->length());
  }

  void testSaveID3v24()
  {
    ScopedFileCopy copy("xing", ".mp3");
    string newname = copy.fileName();

    String xxx = ByteVector(254, 'X');
    MPEG::File f(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(false, f.hasID3v2Tag());

    f.tag()->setTitle(xxx);
    f.tag()->setArtist("Artist A");
    f.save(MPEG::File::AllTags, true, 4);
    CPPUNIT_ASSERT_EQUAL(true, f.hasID3v2Tag());

    MPEG::File f2(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4), f2.ID3v2Tag()->header()->majorVersion());
    CPPUNIT_ASSERT_EQUAL(String("Artist A"), f2.tag()->artist());
    CPPUNIT_ASSERT_EQUAL(xxx, f2.tag()->title());
  }

  void testSaveID3v24WrongParam()
  {
    ScopedFileCopy copy("xing", ".mp3");
    string newname = copy.fileName();

    String xxx = ByteVector(254, 'X');
    MPEG::File f(newname.c_str());
    f.tag()->setTitle(xxx);
    f.tag()->setArtist("Artist A");
    f.save(MPEG::File::AllTags, true, 8);

    MPEG::File f2(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4), f2.ID3v2Tag()->header()->majorVersion());
    CPPUNIT_ASSERT_EQUAL(String("Artist A"), f2.tag()->artist());
    CPPUNIT_ASSERT_EQUAL(xxx, f2.tag()->title());
  }

  void testSaveID3v23()
  {
    ScopedFileCopy copy("xing", ".mp3");
    string newname = copy.fileName();

    String xxx = ByteVector(254, 'X');
    MPEG::File f(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(false, f.hasID3v2Tag());

    f.tag()->setTitle(xxx);
    f.tag()->setArtist("Artist A");
    f.save(MPEG::File::AllTags, true, 3);
    CPPUNIT_ASSERT_EQUAL(true, f.hasID3v2Tag());

    MPEG::File f2(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(3), f2.ID3v2Tag()->header()->majorVersion());
    CPPUNIT_ASSERT_EQUAL(String("Artist A"), f2.tag()->artist());
    CPPUNIT_ASSERT_EQUAL(xxx, f2.tag()->title());
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestMPEG);
