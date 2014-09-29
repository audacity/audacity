#include <string>
#include <stdio.h>
#include <tag.h>
#include <fileref.h>
#include <oggflacfile.h>
#include <vorbisfile.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestFileRef : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestFileRef);
  CPPUNIT_TEST(testASF);
  CPPUNIT_TEST(testMusepack);
  CPPUNIT_TEST(testVorbis);
  CPPUNIT_TEST(testSpeex);
  CPPUNIT_TEST(testFLAC);
  CPPUNIT_TEST(testMP3);
  CPPUNIT_TEST(testOGA_FLAC);
  CPPUNIT_TEST(testOGA_Vorbis);
  CPPUNIT_TEST(testMP4_1);
  CPPUNIT_TEST(testMP4_2);
  CPPUNIT_TEST(testMP4_3);
  CPPUNIT_TEST(testTrueAudio);
  CPPUNIT_TEST(testAPE);
  CPPUNIT_TEST(testWav);
  CPPUNIT_TEST(testUnsupported);
  CPPUNIT_TEST_SUITE_END();

public:

  void fileRefSave(const string &filename, const string &ext)
  {
    ScopedFileCopy copy(filename, ext);
    string newname = copy.fileName();

    FileRef *f = new FileRef(newname.c_str());
    CPPUNIT_ASSERT(!f->isNull());
    f->tag()->setArtist("test artist");
    f->tag()->setTitle("test title");
    f->tag()->setGenre("Test!");
    f->tag()->setAlbum("albummmm");
    f->tag()->setTrack(5);
    f->tag()->setYear(2020);
    f->save();
    delete f;

    f = new FileRef(newname.c_str());
    CPPUNIT_ASSERT(!f->isNull());
    CPPUNIT_ASSERT_EQUAL(f->tag()->artist(), String("test artist"));
    CPPUNIT_ASSERT_EQUAL(f->tag()->title(), String("test title"));
    CPPUNIT_ASSERT_EQUAL(f->tag()->genre(), String("Test!"));
    CPPUNIT_ASSERT_EQUAL(f->tag()->album(), String("albummmm"));
    CPPUNIT_ASSERT_EQUAL(f->tag()->track(), TagLib::uint(5));
    CPPUNIT_ASSERT_EQUAL(f->tag()->year(), TagLib::uint(2020));
    f->tag()->setArtist("ttest artist");
    f->tag()->setTitle("ytest title");
    f->tag()->setGenre("uTest!");
    f->tag()->setAlbum("ialbummmm");
    f->tag()->setTrack(7);
    f->tag()->setYear(2080);
    f->save();
    delete f;

    f = new FileRef(newname.c_str());
    CPPUNIT_ASSERT(!f->isNull());
    CPPUNIT_ASSERT_EQUAL(f->tag()->artist(), String("ttest artist"));
    CPPUNIT_ASSERT_EQUAL(f->tag()->title(), String("ytest title"));
    CPPUNIT_ASSERT_EQUAL(f->tag()->genre(), String("uTest!"));
    CPPUNIT_ASSERT_EQUAL(f->tag()->album(), String("ialbummmm"));
    CPPUNIT_ASSERT_EQUAL(f->tag()->track(), TagLib::uint(7));
    CPPUNIT_ASSERT_EQUAL(f->tag()->year(), TagLib::uint(2080));
    delete f;
  }

  void testMusepack()
  {
    fileRefSave("click", ".mpc");
  }

  void testASF()
  {
    fileRefSave("silence-1", ".wma");
  }

  void testVorbis()
  {
    fileRefSave("empty", ".ogg");
  }

  void testSpeex()
  {
    fileRefSave("empty", ".spx");
  }

  void testFLAC()
  {
    fileRefSave("no-tags", ".flac");
  }

  void testMP3()
  {
    fileRefSave("xing", ".mp3");
  }

  void testTrueAudio()
  {
    fileRefSave("empty", ".tta");
  }

  void testMP4_1()
  {
    fileRefSave("has-tags", ".m4a");
  }

  void testMP4_2()
  {
    fileRefSave("no-tags", ".m4a");
  }

  void testMP4_3()
  {
    fileRefSave("no-tags", ".3g2");
  }

  void testWav()
  {
    fileRefSave("empty", ".wav");
  }

  void testOGA_FLAC()
  {
      FileRef *f = new FileRef(TEST_FILE_PATH_C("empty_flac.oga"));
      CPPUNIT_ASSERT(dynamic_cast<Ogg::Vorbis::File *>(f->file()) == NULL);
      CPPUNIT_ASSERT(dynamic_cast<Ogg::FLAC::File *>(f->file()) != NULL);
  }

  void testOGA_Vorbis()
  {
      FileRef *f = new FileRef(TEST_FILE_PATH_C("empty_vorbis.oga"));
      CPPUNIT_ASSERT(dynamic_cast<Ogg::Vorbis::File *>(f->file()) != NULL);
      CPPUNIT_ASSERT(dynamic_cast<Ogg::FLAC::File *>(f->file()) == NULL);
  }

  void testAPE()
  {
    fileRefSave("mac-399", ".ape");
  }

  void testUnsupported()
  {
    FileRef f1(TEST_FILE_PATH_C("no-extension"));
    CPPUNIT_ASSERT(f1.isNull());
    
    FileRef f2(TEST_FILE_PATH_C("unsupported-extension.xxx"));
    CPPUNIT_ASSERT(f2.isNull());
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(TestFileRef);
