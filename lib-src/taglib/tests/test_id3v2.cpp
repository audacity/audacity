#include <cppunit/extensions/HelperMacros.h>
#include <string>
#include <stdio.h>
#include <id3v2tag.h>
#include <mpegfile.h>
#include <id3v2frame.h>
#include <uniquefileidentifierframe.h>
#include <textidentificationframe.h>
#include <attachedpictureframe.h>
#include <generalencapsulatedobjectframe.h>
#include <relativevolumeframe.h>
#include <popularimeterframe.h>
#include <urllinkframe.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class PublicFrame : public ID3v2::Frame
{
  public:
    PublicFrame() : ID3v2::Frame(ByteVector("XXXX\0\0\0\0\0\0", 10)) {}
    String readStringField(const ByteVector &data, String::Type encoding,
                           int *positon = 0)
      { return ID3v2::Frame::readStringField(data, encoding, positon); }
    virtual String toString() const { return String::null; }
    virtual void parseFields(const ByteVector &) {}
    virtual ByteVector renderFields() const { return ByteVector::null; }
};

class TestID3v2 : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestID3v2);
  CPPUNIT_TEST(testUnsynchDecode);
  CPPUNIT_TEST(testUTF16BEDelimiter);
  CPPUNIT_TEST(testUTF16Delimiter);
  CPPUNIT_TEST(testReadStringField);
  CPPUNIT_TEST(testParseAPIC);
  CPPUNIT_TEST(testParseAPIC_UTF16_BOM);
  CPPUNIT_TEST(testParseGEOB);
  CPPUNIT_TEST(testPOPMtoString);
  CPPUNIT_TEST(testParsePOPM);
  CPPUNIT_TEST(testParsePOPMWithoutCounter);
  CPPUNIT_TEST(testRenderPOPM);
  CPPUNIT_TEST(testPOPMFromFile);
  CPPUNIT_TEST(testParseRelativeVolumeFrame);
  CPPUNIT_TEST(testParseUniqueFileIdentifierFrame);
  CPPUNIT_TEST(testParseEmptyUniqueFileIdentifierFrame);
  CPPUNIT_TEST(testBrokenFrame1);
  CPPUNIT_TEST(testItunes24FrameSize);
  CPPUNIT_TEST(testParseUrlLinkFrame);
  CPPUNIT_TEST(testRenderUrlLinkFrame);
  CPPUNIT_TEST(testParseUserUrlLinkFrame);
  CPPUNIT_TEST(testRenderUserUrlLinkFrame);
  CPPUNIT_TEST(testSaveUTF16Comment);
  CPPUNIT_TEST_SUITE_END();

public:

  void testUnsynchDecode()
  {
    MPEG::File f("data/unsynch.id3", false);
    CPPUNIT_ASSERT(f.tag());
    CPPUNIT_ASSERT_EQUAL(String("My babe just cares for me"), f.tag()->title());
  }

  void testUTF16BEDelimiter()
  {
    ID3v2::TextIdentificationFrame f(ByteVector("TPE1"), String::UTF16BE);
    StringList sl;
    sl.append("Foo");
    sl.append("Bar");
    f.setText(sl);
    CPPUNIT_ASSERT_EQUAL((unsigned int)(4+4+2+1+6+2+6), f.render().size());
  }

  void testUTF16Delimiter()
  {
    ID3v2::TextIdentificationFrame f(ByteVector("TPE1"), String::UTF16);
    StringList sl;
    sl.append("Foo");
    sl.append("Bar");
    f.setText(sl);
    CPPUNIT_ASSERT_EQUAL((unsigned int)(4+4+2+1+8+2+8), f.render().size());
  }

  void testBrokenFrame1()
  {
    MPEG::File f("data/broken-tenc.id3", false);
    CPPUNIT_ASSERT(f.tag());
    CPPUNIT_ASSERT(!f.ID3v2Tag()->frameListMap().contains("TENC"));
  }

  void testReadStringField()
  {
    PublicFrame f;
    ByteVector data("abc\0", 4);
    String str = f.readStringField(data, String::Latin1);
    CPPUNIT_ASSERT_EQUAL(String("abc"), str);
  }

  // http://bugs.kde.org/show_bug.cgi?id=151078
  void testParseAPIC()
  {
    ID3v2::AttachedPictureFrame f(ByteVector("APIC"
                                             "\x00\x00\x00\x07"
                                             "\x00\x00"
                                             "\x00"
                                             "m\x00"
                                             "\x01"
                                             "d\x00"
                                             "\x00", 17));
    CPPUNIT_ASSERT_EQUAL(String("m"), f.mimeType());
    CPPUNIT_ASSERT_EQUAL(ID3v2::AttachedPictureFrame::FileIcon, f.type());
    CPPUNIT_ASSERT_EQUAL(String("d"), f.description());
  }

  void testParseAPIC_UTF16_BOM()
  {
    ID3v2::AttachedPictureFrame f(ByteVector(
      "\x41\x50\x49\x43\x00\x02\x0c\x59\x00\x00\x01\x69\x6d\x61\x67\x65"
      "\x2f\x6a\x70\x65\x67\x00\x00\xfe\xff\x00\x63\x00\x6f\x00\x76\x00"
      "\x65\x00\x72\x00\x2e\x00\x6a\x00\x70\x00\x67\x00\x00\xff\xd8\xff",
      16 * 3));
    CPPUNIT_ASSERT_EQUAL(String("image/jpeg"), f.mimeType());
    CPPUNIT_ASSERT_EQUAL(ID3v2::AttachedPictureFrame::Other, f.type());
    CPPUNIT_ASSERT_EQUAL(String("cover.jpg"), f.description());
    CPPUNIT_ASSERT_EQUAL(ByteVector("\xff\xd8\xff", 3), f.picture());
  }

  // http://bugs.kde.org/show_bug.cgi?id=151078
  void testParseGEOB()
  {
    ID3v2::GeneralEncapsulatedObjectFrame f(ByteVector("GEOB"
                                             "\x00\x00\x00\x08"
                                             "\x00\x00"
                                             "\x00"
                                             "m\x00"
                                             "f\x00"
                                             "d\x00"
                                             "\x00", 18));
    CPPUNIT_ASSERT_EQUAL(String("m"), f.mimeType());
    CPPUNIT_ASSERT_EQUAL(String("f"), f.fileName());
    CPPUNIT_ASSERT_EQUAL(String("d"), f.description());
  }

  void testParsePOPM()
  {
    ID3v2::PopularimeterFrame f(ByteVector("POPM"
                                           "\x00\x00\x00\x17"
                                           "\x00\x00"
                                           "email@example.com\x00"
                                           "\x02"
                                           "\x00\x00\x00\x03", 33));
    CPPUNIT_ASSERT_EQUAL(String("email@example.com"), f.email());
    CPPUNIT_ASSERT_EQUAL(2, f.rating());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(3), f.counter());
  }

  void testParsePOPMWithoutCounter()
  {
    ID3v2::PopularimeterFrame f(ByteVector("POPM"
                                           "\x00\x00\x00\x13"
                                           "\x00\x00"
                                           "email@example.com\x00"
                                           "\x02", 29));
    CPPUNIT_ASSERT_EQUAL(String("email@example.com"), f.email());
    CPPUNIT_ASSERT_EQUAL(2, f.rating());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0), f.counter());
  }

  void testRenderPOPM()
  {
    ID3v2::PopularimeterFrame f;
    f.setEmail("email@example.com");
    f.setRating(2);
    f.setCounter(3);
    CPPUNIT_ASSERT_EQUAL(
      ByteVector("POPM"
                 "\x00\x00\x00\x17"
                 "\x00\x00"
                 "email@example.com\x00"
                 "\x02"
                 "\x00\x00\x00\x03", 33),
      f.render());
  }

  void testPOPMtoString()
  {
    ID3v2::PopularimeterFrame f;
    f.setEmail("email@example.com");
    f.setRating(2);
    f.setCounter(3);
    CPPUNIT_ASSERT_EQUAL(
      String("email@example.com rating=2 counter=3"), f.toString());
  }

  void testPOPMFromFile()
  {
    string newname = copyFile("xing", ".mp3");

    ID3v2::PopularimeterFrame *f = new ID3v2::PopularimeterFrame();
    f->setEmail("email@example.com");
    f->setRating(200);
    f->setCounter(3);

    MPEG::File foo(newname.c_str());
    foo.ID3v2Tag()->addFrame(f);
    foo.save();

    MPEG::File bar(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(String("email@example.com"), dynamic_cast<ID3v2::PopularimeterFrame *>(bar.ID3v2Tag()->frameList("POPM").front())->email());
    CPPUNIT_ASSERT_EQUAL(200, dynamic_cast<ID3v2::PopularimeterFrame *>(bar.ID3v2Tag()->frameList("POPM").front())->rating());
    deleteFile(newname);
  }

  // http://bugs.kde.org/show_bug.cgi?id=150481
  void testParseRelativeVolumeFrame()
  {
    ID3v2::RelativeVolumeFrame f(
      ByteVector("RVA2"              // Frame ID
                 "\x00\x00\x00\x0B"  // Frame size
                 "\x00\x00"          // Frame flags
                 "ident\x00"         // Identification
                 "\x02"              // Type of channel
                 "\x00\x0F"          // Volume adjustment
                 "\x08"              // Bits representing peak
                 "\x45", 21));       // Peak volume
    CPPUNIT_ASSERT_EQUAL(String("ident"), f.identification());
    CPPUNIT_ASSERT_EQUAL(15.0f / 512.0f,
                         f.volumeAdjustment(ID3v2::RelativeVolumeFrame::FrontRight));
    CPPUNIT_ASSERT_EQUAL((uchar)8,
                         f.peakVolume(ID3v2::RelativeVolumeFrame::FrontRight).bitsRepresentingPeak);
    CPPUNIT_ASSERT_EQUAL(ByteVector("\x45"),
                         f.peakVolume(ID3v2::RelativeVolumeFrame::FrontRight).peakVolume);
  }

  void testParseUniqueFileIdentifierFrame()
  {
    ID3v2::UniqueFileIdentifierFrame f(
      ByteVector("UFID"                 // Frame ID
                 "\x00\x00\x00\x09"     // Frame size
                 "\x00\x00"             // Frame flags
                 "owner\x00"            // Owner identifier
                 "\x00\x01\x02", 19));  // Identifier
    CPPUNIT_ASSERT_EQUAL(String("owner"),
                         f.owner());
    CPPUNIT_ASSERT_EQUAL(ByteVector("\x00\x01\x02", 3),
                         f.identifier());
  }

  void testParseEmptyUniqueFileIdentifierFrame()
  {
    ID3v2::UniqueFileIdentifierFrame f(
      ByteVector("UFID"                 // Frame ID
                 "\x00\x00\x00\x01"     // Frame size
                 "\x00\x00"             // Frame flags
                 "\x00"                 // Owner identifier
                 "", 11));              // Identifier
    CPPUNIT_ASSERT_EQUAL(String(),
                         f.owner());
    CPPUNIT_ASSERT_EQUAL(ByteVector(),
                         f.identifier());
  }

  void testParseUrlLinkFrame()
  {
    ID3v2::UrlLinkFrame f(
      ByteVector("WOAF"                      // Frame ID
                 "\x00\x00\x00\x12"          // Frame size
                 "\x00\x00"                  // Frame flags
                 "http://example.com", 28)); // URL
    CPPUNIT_ASSERT_EQUAL(String("http://example.com"), f.url());
  }

  void testRenderUrlLinkFrame()
  {
    ID3v2::UrlLinkFrame f("WOAF");
    f.setUrl("http://example.com");
    CPPUNIT_ASSERT_EQUAL(
      ByteVector("WOAF"                      // Frame ID
                 "\x00\x00\x00\x12"          // Frame size
                 "\x00\x00"                  // Frame flags
                 "http://example.com", 28),  // URL
      f.render());
  }

  void testParseUserUrlLinkFrame()
  {
    ID3v2::UserUrlLinkFrame f(
      ByteVector("WXXX"                      // Frame ID
                 "\x00\x00\x00\x17"          // Frame size
                 "\x00\x00"                  // Frame flags
                 "\x00"                      // Text encoding
                 "foo\x00"                   // Description
                 "http://example.com", 33)); // URL
    CPPUNIT_ASSERT_EQUAL(String("foo"), f.description());
    CPPUNIT_ASSERT_EQUAL(String("http://example.com"), f.url());
  }

  void testRenderUserUrlLinkFrame()
  {
    ID3v2::UserUrlLinkFrame f;
    f.setDescription("foo");
    f.setUrl("http://example.com");
    CPPUNIT_ASSERT_EQUAL(
      ByteVector("WXXX"                      // Frame ID
                 "\x00\x00\x00\x17"          // Frame size
                 "\x00\x00"                  // Frame flags
                 "\x00"                      // Text encoding
                 "foo\x00"                   // Description
                 "http://example.com", 33),  // URL
      f.render());
  }

  void testItunes24FrameSize()
  {
    MPEG::File f("data/005411.id3", false);
    CPPUNIT_ASSERT(f.tag());
    CPPUNIT_ASSERT(f.ID3v2Tag()->frameListMap().contains("TIT2"));
    CPPUNIT_ASSERT_EQUAL(String("Sunshine Superman"), f.ID3v2Tag()->frameListMap()["TIT2"].front()->toString());
  }

  void testSaveUTF16Comment()
  {
    String::Type defaultEncoding = ID3v2::FrameFactory::instance()->defaultTextEncoding();
    string newname = copyFile("xing", ".mp3");
    ID3v2::FrameFactory::instance()->setDefaultTextEncoding(String::UTF16);
    MPEG::File foo(newname.c_str());
    foo.strip();
    foo.tag()->setComment("Test comment!");
    foo.save();
    MPEG::File bar(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(String("Test comment!"), bar.tag()->comment());
    deleteFile(newname);
    ID3v2::FrameFactory::instance()->setDefaultTextEncoding(defaultEncoding);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestID3v2);
