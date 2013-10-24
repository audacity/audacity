#include <string>
#include <stdio.h>
#include <tag.h>
#include <tstringlist.h>
#include <tbytevectorlist.h>
#include <tpropertymap.h>
#include <flacfile.h>
#include <xiphcomment.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestFLAC : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestFLAC);
  CPPUNIT_TEST(testSignature);
  CPPUNIT_TEST(testMultipleCommentBlocks);
  CPPUNIT_TEST(testReadPicture);
  CPPUNIT_TEST(testAddPicture);
  CPPUNIT_TEST(testReplacePicture);
  CPPUNIT_TEST(testRemoveAllPictures);
  CPPUNIT_TEST(testRepeatedSave);
  CPPUNIT_TEST(testSaveMultipleValues);
  CPPUNIT_TEST(testDict);
  CPPUNIT_TEST(testInvalid);
  CPPUNIT_TEST_SUITE_END();

public:

  void testSignature()
  {
    FLAC::File f(TEST_FILE_PATH_C("no-tags.flac"));
    CPPUNIT_ASSERT_EQUAL(ByteVector("a1b141f766e9849ac3db1030a20a3c77"), f.audioProperties()->signature().toHex());
  }

  void testMultipleCommentBlocks()
  {
    ScopedFileCopy copy("multiple-vc", ".flac");
    string newname = copy.fileName();

    FLAC::File *f = new FLAC::File(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(String("Artist 1"), f->tag()->artist());
    f->tag()->setArtist("The Artist");
    f->save();
    delete f;

    f = new FLAC::File(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(String("The Artist"), f->tag()->artist());
    delete f;
  }

  void testReadPicture()
  {
    ScopedFileCopy copy("silence-44-s", ".flac");
    string newname = copy.fileName();

    FLAC::File *f = new FLAC::File(newname.c_str());
    List<FLAC::Picture *> lst = f->pictureList();
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), lst.size());

    FLAC::Picture *pic = lst.front();
    CPPUNIT_ASSERT_EQUAL(FLAC::Picture::FrontCover, pic->type());
    CPPUNIT_ASSERT_EQUAL(1, pic->width());
    CPPUNIT_ASSERT_EQUAL(1, pic->height());
    CPPUNIT_ASSERT_EQUAL(24, pic->colorDepth());
    CPPUNIT_ASSERT_EQUAL(0, pic->numColors());
    CPPUNIT_ASSERT_EQUAL(String("image/png"), pic->mimeType());
    CPPUNIT_ASSERT_EQUAL(String("A pixel."), pic->description());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(150), pic->data().size());
  }

  void testAddPicture()
  {
    ScopedFileCopy copy("silence-44-s", ".flac");
    string newname = copy.fileName();

    FLAC::File *f = new FLAC::File(newname.c_str());
    List<FLAC::Picture *> lst = f->pictureList();
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), lst.size());

    FLAC::Picture *newpic = new FLAC::Picture();
    newpic->setType(FLAC::Picture::BackCover);
    newpic->setWidth(5);
    newpic->setHeight(6);
    newpic->setColorDepth(16);
    newpic->setNumColors(7);
    newpic->setMimeType("image/jpeg");
    newpic->setDescription("new image");
    newpic->setData("JPEG data");
    f->addPicture(newpic);
    f->save();

    f = new FLAC::File(newname.c_str());
    lst = f->pictureList();
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2), lst.size());

    FLAC::Picture *pic = lst[0];
    CPPUNIT_ASSERT_EQUAL(FLAC::Picture::FrontCover, pic->type());
    CPPUNIT_ASSERT_EQUAL(1, pic->width());
    CPPUNIT_ASSERT_EQUAL(1, pic->height());
    CPPUNIT_ASSERT_EQUAL(24, pic->colorDepth());
    CPPUNIT_ASSERT_EQUAL(0, pic->numColors());
    CPPUNIT_ASSERT_EQUAL(String("image/png"), pic->mimeType());
    CPPUNIT_ASSERT_EQUAL(String("A pixel."), pic->description());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(150), pic->data().size());

    pic = lst[1];
    CPPUNIT_ASSERT_EQUAL(FLAC::Picture::BackCover, pic->type());
    CPPUNIT_ASSERT_EQUAL(5, pic->width());
    CPPUNIT_ASSERT_EQUAL(6, pic->height());
    CPPUNIT_ASSERT_EQUAL(16, pic->colorDepth());
    CPPUNIT_ASSERT_EQUAL(7, pic->numColors());
    CPPUNIT_ASSERT_EQUAL(String("image/jpeg"), pic->mimeType());
    CPPUNIT_ASSERT_EQUAL(String("new image"), pic->description());
    CPPUNIT_ASSERT_EQUAL(ByteVector("JPEG data"), pic->data());
  }

  void testReplacePicture()
  {
    ScopedFileCopy copy("silence-44-s", ".flac");
    string newname = copy.fileName();

    FLAC::File *f = new FLAC::File(newname.c_str());
    List<FLAC::Picture *> lst = f->pictureList();
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), lst.size());

    FLAC::Picture *newpic = new FLAC::Picture();
    newpic->setType(FLAC::Picture::BackCover);
    newpic->setWidth(5);
    newpic->setHeight(6);
    newpic->setColorDepth(16);
    newpic->setNumColors(7);
    newpic->setMimeType("image/jpeg");
    newpic->setDescription("new image");
    newpic->setData("JPEG data");
    f->removePictures();
    f->addPicture(newpic);
    f->save();

    f = new FLAC::File(newname.c_str());
    lst = f->pictureList();
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), lst.size());

    FLAC::Picture *pic = lst[0];
    CPPUNIT_ASSERT_EQUAL(FLAC::Picture::BackCover, pic->type());
    CPPUNIT_ASSERT_EQUAL(5, pic->width());
    CPPUNIT_ASSERT_EQUAL(6, pic->height());
    CPPUNIT_ASSERT_EQUAL(16, pic->colorDepth());
    CPPUNIT_ASSERT_EQUAL(7, pic->numColors());
    CPPUNIT_ASSERT_EQUAL(String("image/jpeg"), pic->mimeType());
    CPPUNIT_ASSERT_EQUAL(String("new image"), pic->description());
    CPPUNIT_ASSERT_EQUAL(ByteVector("JPEG data"), pic->data());
  }

  void testRemoveAllPictures()
  {
    ScopedFileCopy copy("silence-44-s", ".flac");
    string newname = copy.fileName();

    FLAC::File *f = new FLAC::File(newname.c_str());
    List<FLAC::Picture *> lst = f->pictureList();
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), lst.size());

    f->removePictures();
    f->save();

    f = new FLAC::File(newname.c_str());
    lst = f->pictureList();
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0), lst.size());
  }

  void testRepeatedSave()
  {
    ScopedFileCopy copy("silence-44-s", ".flac");
    string newname = copy.fileName();

    FLAC::File *f = new FLAC::File(newname.c_str());
    Tag *tag = f->tag();
    CPPUNIT_ASSERT_EQUAL(String("Silence"), tag->title());
    tag->setTitle("NEW TITLE");
    f->save();
    CPPUNIT_ASSERT_EQUAL(String("NEW TITLE"), tag->title());
    tag->setTitle("NEW TITLE 2");
    f->save();
    CPPUNIT_ASSERT_EQUAL(String("NEW TITLE 2"), tag->title());

    f = new FLAC::File(newname.c_str());
    tag = f->tag();
    CPPUNIT_ASSERT_EQUAL(String("NEW TITLE 2"), tag->title());
  }

  void testSaveMultipleValues()
  {
    ScopedFileCopy copy("silence-44-s", ".flac");
    string newname = copy.fileName();

    FLAC::File *f = new FLAC::File(newname.c_str());
    Ogg::XiphComment* c = f->xiphComment(true);
    c->addField("ARTIST", "artist 1", true);
    c->addField("ARTIST", "artist 2", false);
    f->save();
    delete f;

    f = new FLAC::File(newname.c_str());
    c = f->xiphComment(true);
    Ogg::FieldListMap m = c->fieldListMap();
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2), m["ARTIST"].size());
    CPPUNIT_ASSERT_EQUAL(String("artist 1"), m["ARTIST"][0]);
    CPPUNIT_ASSERT_EQUAL(String("artist 2"), m["ARTIST"][1]);
  }

  void testDict()
  {
    // test unicode & multiple values with dict interface
    ScopedFileCopy copy("silence-44-s", ".flac");
    string newname = copy.fileName();

    FLAC::File *f = new FLAC::File(newname.c_str());
    PropertyMap dict;
    dict["ARTIST"].append("artøst 1");
    dict["ARTIST"].append("artöst 2");
    f->setProperties(dict);
    f->save();
    delete f;

    f = new FLAC::File(newname.c_str());
    dict = f->properties();
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2), dict["ARTIST"].size());
    CPPUNIT_ASSERT_EQUAL(String("artøst 1"), dict["ARTIST"][0]);
    CPPUNIT_ASSERT_EQUAL(String("artöst 2"), dict["ARTIST"][1]);
  }

  void testInvalid()
  {
    ScopedFileCopy copy("silence-44-s", ".flac");
    PropertyMap map;
    map["HÄÖ"] = String("bla");
    FLAC::File f(copy.fileName().c_str());
    PropertyMap invalid = f.setProperties(map);
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), invalid.size());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0), f.properties().size());
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestFLAC);
