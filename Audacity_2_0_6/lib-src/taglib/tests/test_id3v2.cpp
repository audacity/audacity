#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>
#include <stdio.h>
// so evil :(
#define protected public
#include <id3v2tag.h>
#include <mpegfile.h>
#include <id3v2frame.h>
#undef protected
#include <uniquefileidentifierframe.h>
#include <textidentificationframe.h>
#include <attachedpictureframe.h>
#include <unsynchronizedlyricsframe.h>
#include <generalencapsulatedobjectframe.h>
#include <relativevolumeframe.h>
#include <popularimeterframe.h>
#include <urllinkframe.h>
#include <ownershipframe.h>
#include <unknownframe.h>
#include <tdebug.h>
#include <tpropertymap.h>
#include <cppunit/extensions/HelperMacros.h>
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
  CPPUNIT_TEST(testDowngradeUTF8ForID3v23);
  CPPUNIT_TEST(testUTF16BEDelimiter);
  CPPUNIT_TEST(testUTF16Delimiter);
  CPPUNIT_TEST(testReadStringField);
  CPPUNIT_TEST(testParseAPIC);
  CPPUNIT_TEST(testParseAPIC_UTF16_BOM);
  CPPUNIT_TEST(testParseAPICv22);
  CPPUNIT_TEST(testDontRender22);
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
  CPPUNIT_TEST(testParseOwnershipFrame);
  CPPUNIT_TEST(testRenderOwnershipFrame);
  CPPUNIT_TEST(testSaveUTF16Comment);
  CPPUNIT_TEST(testUpdateGenre23_1);
  CPPUNIT_TEST(testUpdateGenre23_2);
  CPPUNIT_TEST(testUpdateGenre24);
  CPPUNIT_TEST(testUpdateDate22);
  CPPUNIT_TEST(testDowngradeTo23);
  // CPPUNIT_TEST(testUpdateFullDate22); TODO TYE+TDA should be upgraded to TDRC together
  CPPUNIT_TEST(testCompressedFrameWithBrokenLength);
  CPPUNIT_TEST(testW000);
  CPPUNIT_TEST(testPropertyInterface);
  CPPUNIT_TEST(testPropertyInterface2);
  CPPUNIT_TEST(testDeleteFrame);
  CPPUNIT_TEST(testSaveAndStripID3v1ShouldNotAddFrameFromID3v1ToId3v2);
  CPPUNIT_TEST_SUITE_END();

public:

  void testUnsynchDecode()
  {
    MPEG::File f(TEST_FILE_PATH_C("unsynch.id3"), false);
    CPPUNIT_ASSERT(f.tag());
    CPPUNIT_ASSERT_EQUAL(String("My babe just cares for me"), f.tag()->title());
  }

  void testDowngradeUTF8ForID3v23()
  {
    ID3v2::TextIdentificationFrame f(ByteVector("TPE1"), String::UTF8);
    StringList sl;
    sl.append("Foo");
    f.setText(sl);
    f.header()->setVersion(3);
    ByteVector data = f.render();
    CPPUNIT_ASSERT_EQUAL((unsigned int)(4+4+2+1+6+2), data.size());
    ID3v2::TextIdentificationFrame f2(data);
    CPPUNIT_ASSERT_EQUAL(sl, f2.fieldList());
    CPPUNIT_ASSERT_EQUAL(String::UTF16, f2.textEncoding());
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
    MPEG::File f(TEST_FILE_PATH_C("broken-tenc.id3"), false);
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

  void testParseAPICv22()
  {
    ID3v2::FrameFactory *factory = ID3v2::FrameFactory::instance();
    ByteVector data = ByteVector("PIC"
                                 "\x00\x00\x08"
                                 "\x00"
                                 "JPG"
                                 "\x01"
                                 "d\x00"
                                 "\x00", 18);
    ID3v2::AttachedPictureFrame *frame =
        static_cast<TagLib::ID3v2::AttachedPictureFrame*>(factory->createFrame(data, TagLib::uint(2)));

    CPPUNIT_ASSERT(frame);
    CPPUNIT_ASSERT_EQUAL(String("image/jpeg"), frame->mimeType());
    CPPUNIT_ASSERT_EQUAL(ID3v2::AttachedPictureFrame::FileIcon, frame->type());
    CPPUNIT_ASSERT_EQUAL(String("d"), frame->description());
  }

  void testDontRender22()
  {
    ID3v2::FrameFactory *factory = ID3v2::FrameFactory::instance();
    ByteVector data = ByteVector("FOO"
                                 "\x00\x00\x08"
                                 "\x00"
                                 "JPG"
                                 "\x01"
                                 "d\x00"
                                 "\x00", 18);
    ID3v2::AttachedPictureFrame *frame =
        static_cast<TagLib::ID3v2::AttachedPictureFrame*>(factory->createFrame(data, TagLib::uint(2)));

    CPPUNIT_ASSERT(frame);

    ID3v2::Tag tag;
    tag.addFrame(frame);
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1034), tag.render().size());
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
    ScopedFileCopy copy("xing", ".mp3");
    string newname = copy.fileName();

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
  
  void testParseOwnershipFrame()
  {
    ID3v2::OwnershipFrame f(
                            ByteVector("OWNE"                      // Frame ID
                                       "\x00\x00\x00\x19"          // Frame size
                                       "\x00\x00"                  // Frame flags
                                       "\x00"                      // Text encoding
                                       "GBP1.99\x00"               // Price paid
                                       "20120905"                  // Date of purchase
                                       "Beatport", 35));           // Seller
    CPPUNIT_ASSERT_EQUAL(String("GBP1.99"), f.pricePaid());
    CPPUNIT_ASSERT_EQUAL(String("20120905"), f.datePurchased());
    CPPUNIT_ASSERT_EQUAL(String("Beatport"), f.seller());
  }

  void testRenderOwnershipFrame()
  {
    ID3v2::OwnershipFrame f;
    f.setPricePaid("GBP1.99");
    f.setDatePurchased("20120905");
    f.setSeller("Beatport");
    CPPUNIT_ASSERT_EQUAL(
                         ByteVector("OWNE"                      // Frame ID
                                    "\x00\x00\x00\x19"          // Frame size
                                    "\x00\x00"                  // Frame flags
                                    "\x00"                      // Text encoding
                                    "GBP1.99\x00"               // Price paid
                                    "20120905"                  // Date of purchase
                                    "Beatport", 35),  // URL
                         f.render());
  }

  void testItunes24FrameSize()
  {
    MPEG::File f(TEST_FILE_PATH_C("005411.id3"), false);
    CPPUNIT_ASSERT(f.tag());
    CPPUNIT_ASSERT(f.ID3v2Tag()->frameListMap().contains("TIT2"));
    CPPUNIT_ASSERT_EQUAL(String("Sunshine Superman"), f.ID3v2Tag()->frameListMap()["TIT2"].front()->toString());
  }

  void testSaveUTF16Comment()
  {
    String::Type defaultEncoding = ID3v2::FrameFactory::instance()->defaultTextEncoding();
    ScopedFileCopy copy("xing", ".mp3");
    string newname = copy.fileName();
    ID3v2::FrameFactory::instance()->setDefaultTextEncoding(String::UTF16);
    MPEG::File foo(newname.c_str());
    foo.strip();
    foo.tag()->setComment("Test comment!");
    foo.save();
    MPEG::File bar(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(String("Test comment!"), bar.tag()->comment());
    ID3v2::FrameFactory::instance()->setDefaultTextEncoding(defaultEncoding);
  }

  void testUpdateGenre23_1()
  {
    // "Refinement" is the same as the ID3v1 genre - duplicate
    ID3v2::FrameFactory *factory = ID3v2::FrameFactory::instance();
    ByteVector data = ByteVector("TCON"                 // Frame ID
                                 "\x00\x00\x00\x10"     // Frame size
                                 "\x00\x00"             // Frame flags
                                 "\x00"                 // Encoding
                                 "(22)Death Metal", 26);     // Text
    ID3v2::TextIdentificationFrame *frame =
        static_cast<TagLib::ID3v2::TextIdentificationFrame*>(factory->createFrame(data, TagLib::uint(3)));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), frame->fieldList().size());
    CPPUNIT_ASSERT_EQUAL(String("Death Metal"), frame->fieldList()[0]);

    ID3v2::Tag tag;
    tag.addFrame(frame);
    CPPUNIT_ASSERT_EQUAL(String("Death Metal"), tag.genre());
  }

  void testUpdateGenre23_2()
  {
    // "Refinement" is different from the ID3v1 genre
    ID3v2::FrameFactory *factory = ID3v2::FrameFactory::instance();
    ByteVector data = ByteVector("TCON"                 // Frame ID
                                 "\x00\x00\x00\x13"     // Frame size
                                 "\x00\x00"             // Frame flags
                                 "\x00"                 // Encoding
                                 "(4)Eurodisco", 23);   // Text
    ID3v2::TextIdentificationFrame *frame =
        static_cast<TagLib::ID3v2::TextIdentificationFrame*>(factory->createFrame(data, TagLib::uint(3)));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2), frame->fieldList().size());
    CPPUNIT_ASSERT_EQUAL(String("4"), frame->fieldList()[0]);
    CPPUNIT_ASSERT_EQUAL(String("Eurodisco"), frame->fieldList()[1]);

    ID3v2::Tag tag;
    tag.addFrame(frame);
    CPPUNIT_ASSERT_EQUAL(String("Disco Eurodisco"), tag.genre());
  }

  void testUpdateGenre24()
  {
    ID3v2::FrameFactory *factory = ID3v2::FrameFactory::instance();
    ByteVector data = ByteVector("TCON"                   // Frame ID
                                 "\x00\x00\x00\x0D"       // Frame size
                                 "\x00\x00"               // Frame flags
                                 "\0"                   // Encoding
                                 "14\0Eurodisco", 23);     // Text
    ID3v2::TextIdentificationFrame *frame =
        static_cast<TagLib::ID3v2::TextIdentificationFrame*>(factory->createFrame(data, TagLib::uint(4)));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2), frame->fieldList().size());
    CPPUNIT_ASSERT_EQUAL(String("14"), frame->fieldList()[0]);
    CPPUNIT_ASSERT_EQUAL(String("Eurodisco"), frame->fieldList()[1]);

    ID3v2::Tag tag;
    tag.addFrame(frame);
    CPPUNIT_ASSERT_EQUAL(String("R&B Eurodisco"), tag.genre());
  }

  void testUpdateDate22()
  {
    MPEG::File f(TEST_FILE_PATH_C("id3v22-tda.mp3"), false);
    CPPUNIT_ASSERT(f.tag());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2010), f.tag()->year());
  }

  void testUpdateFullDate22()
  {
    MPEG::File f(TEST_FILE_PATH_C("id3v22-tda.mp3"), false);
    CPPUNIT_ASSERT(f.tag());
    CPPUNIT_ASSERT_EQUAL(String("2010-04-03"), f.ID3v2Tag()->frameListMap()["TDRC"].front()->toString());
  }

  void testDowngradeTo23()
  {
    ScopedFileCopy copy("xing", ".mp3");
    string newname = copy.fileName();

    ID3v2::TextIdentificationFrame *tf;
    MPEG::File foo(newname.c_str());
    tf = new ID3v2::TextIdentificationFrame("TDOR", String::Latin1);
    tf->setText("2011-03-16");
    foo.ID3v2Tag()->addFrame(tf);
    tf = new ID3v2::TextIdentificationFrame("TDRC", String::Latin1);
    tf->setText("2012-04-17T12:01");
    foo.ID3v2Tag()->addFrame(tf);
    tf = new ID3v2::TextIdentificationFrame("TMCL", String::Latin1);
    tf->setText(StringList().append("Guitar").append("Artist 1").append("Drums").append("Artist 2"));
    foo.ID3v2Tag()->addFrame(tf);
    tf = new ID3v2::TextIdentificationFrame("TIPL", String::Latin1);
    tf->setText(StringList().append("Producer").append("Artist 3").append("Mastering").append("Artist 4"));
    foo.ID3v2Tag()->addFrame(tf);
    foo.ID3v2Tag()->addFrame(new ID3v2::TextIdentificationFrame("TDRL", String::Latin1));
    foo.ID3v2Tag()->addFrame(new ID3v2::TextIdentificationFrame("TDTG", String::Latin1));
    foo.ID3v2Tag()->addFrame(new ID3v2::TextIdentificationFrame("TMOO", String::Latin1));
    foo.ID3v2Tag()->addFrame(new ID3v2::TextIdentificationFrame("TPRO", String::Latin1));
    foo.ID3v2Tag()->addFrame(new ID3v2::TextIdentificationFrame("TSOA", String::Latin1));
    foo.ID3v2Tag()->addFrame(new ID3v2::TextIdentificationFrame("TSOT", String::Latin1));
    foo.ID3v2Tag()->addFrame(new ID3v2::TextIdentificationFrame("TSST", String::Latin1));
    foo.ID3v2Tag()->addFrame(new ID3v2::TextIdentificationFrame("TSOP", String::Latin1));
    foo.save(MPEG::File::AllTags, true, 3);

    MPEG::File bar(newname.c_str());
    tf = static_cast<ID3v2::TextIdentificationFrame *>(bar.ID3v2Tag()->frameList("TDOR").front());
    CPPUNIT_ASSERT(tf);
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), tf->fieldList().size());
    CPPUNIT_ASSERT_EQUAL(String("2011"), tf->fieldList().front());
    tf = static_cast<ID3v2::TextIdentificationFrame *>(bar.ID3v2Tag()->frameList("TDRC").front());
    CPPUNIT_ASSERT(tf);
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), tf->fieldList().size());
    CPPUNIT_ASSERT_EQUAL(String("2012"), tf->fieldList().front());
    tf = dynamic_cast<ID3v2::TextIdentificationFrame *>(bar.ID3v2Tag()->frameList("TIPL").front());
    CPPUNIT_ASSERT(tf);
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(8), tf->fieldList().size());
    CPPUNIT_ASSERT_EQUAL(String("Guitar"), tf->fieldList()[0]);
    CPPUNIT_ASSERT_EQUAL(String("Artist 1"), tf->fieldList()[1]);
    CPPUNIT_ASSERT_EQUAL(String("Drums"), tf->fieldList()[2]);
    CPPUNIT_ASSERT_EQUAL(String("Artist 2"), tf->fieldList()[3]);
    CPPUNIT_ASSERT_EQUAL(String("Producer"), tf->fieldList()[4]);
    CPPUNIT_ASSERT_EQUAL(String("Artist 3"), tf->fieldList()[5]);
    CPPUNIT_ASSERT_EQUAL(String("Mastering"), tf->fieldList()[6]);
    CPPUNIT_ASSERT_EQUAL(String("Artist 4"), tf->fieldList()[7]);
    CPPUNIT_ASSERT(!bar.ID3v2Tag()->frameListMap().contains("TDRL"));
    CPPUNIT_ASSERT(!bar.ID3v2Tag()->frameListMap().contains("TDTG"));
    CPPUNIT_ASSERT(!bar.ID3v2Tag()->frameListMap().contains("TMOO"));
    CPPUNIT_ASSERT(!bar.ID3v2Tag()->frameListMap().contains("TPRO"));
    CPPUNIT_ASSERT(!bar.ID3v2Tag()->frameListMap().contains("TSOA"));
    CPPUNIT_ASSERT(!bar.ID3v2Tag()->frameListMap().contains("TSOT"));
    CPPUNIT_ASSERT(!bar.ID3v2Tag()->frameListMap().contains("TSST"));
    CPPUNIT_ASSERT(!bar.ID3v2Tag()->frameListMap().contains("TSOP"));
  }

  void testCompressedFrameWithBrokenLength()
  {
    MPEG::File f(TEST_FILE_PATH_C("compressed_id3_frame.mp3"), false);
    CPPUNIT_ASSERT(f.ID3v2Tag()->frameListMap().contains("APIC"));

#ifdef HAVE_ZLIB

    ID3v2::AttachedPictureFrame *frame 
      = dynamic_cast<TagLib::ID3v2::AttachedPictureFrame*>(f.ID3v2Tag()->frameListMap()["APIC"].front());
    CPPUNIT_ASSERT(frame);
    CPPUNIT_ASSERT_EQUAL(String("image/bmp"), frame->mimeType());
    CPPUNIT_ASSERT_EQUAL(ID3v2::AttachedPictureFrame::Other, frame->type());
    CPPUNIT_ASSERT_EQUAL(String(""), frame->description());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(86414), frame->picture().size());

#else

    // Skip the test if ZLIB is not installed.
    // The message "Compressed frames are currently not supported." will be displayed.

    ID3v2::UnknownFrame *frame 
      = dynamic_cast<TagLib::ID3v2::UnknownFrame*>(f.ID3v2Tag()->frameListMap()["APIC"].front());
    CPPUNIT_ASSERT(frame);

#endif
  }
  
  void testW000()
  {
    MPEG::File f(TEST_FILE_PATH_C("w000.mp3"), false);
    CPPUNIT_ASSERT(f.ID3v2Tag()->frameListMap().contains("W000"));
    ID3v2::UrlLinkFrame *frame =
    dynamic_cast<TagLib::ID3v2::UrlLinkFrame*>(f.ID3v2Tag()->frameListMap()["W000"].front());
    CPPUNIT_ASSERT(frame);
    CPPUNIT_ASSERT_EQUAL(String("lukas.lalinsky@example.com____"), frame->url());
  }

  void testPropertyInterface()
  {
    ScopedFileCopy copy("rare_frames", ".mp3");
    string newname = copy.fileName();
    MPEG::File f(newname.c_str());
    PropertyMap dict = f.ID3v2Tag(false)->properties();
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(6), dict.size());

    CPPUNIT_ASSERT(dict.contains("USERTEXTDESCRIPTION1"));
    CPPUNIT_ASSERT(dict.contains("QuodLibet::USERTEXTDESCRIPTION2"));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2), dict["USERTEXTDESCRIPTION1"].size());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2), dict["QuodLibet::USERTEXTDESCRIPTION2"].size());
    CPPUNIT_ASSERT_EQUAL(String("userTextData1"), dict["USERTEXTDESCRIPTION1"][0]);
    CPPUNIT_ASSERT_EQUAL(String("userTextData2"), dict["USERTEXTDESCRIPTION1"][1]);
    CPPUNIT_ASSERT_EQUAL(String("userTextData1"), dict["QuodLibet::USERTEXTDESCRIPTION2"][0]);
    CPPUNIT_ASSERT_EQUAL(String("userTextData2"), dict["QuodLibet::USERTEXTDESCRIPTION2"][1]);

    CPPUNIT_ASSERT_EQUAL(String("Pop"), dict["GENRE"].front());

    CPPUNIT_ASSERT_EQUAL(String("http://a.user.url"), dict["URL:USERURL"].front());

    CPPUNIT_ASSERT_EQUAL(String("http://a.user.url/with/empty/description"), dict["URL"].front());
    CPPUNIT_ASSERT_EQUAL(String("A COMMENT"), dict["COMMENT"].front());

    CPPUNIT_ASSERT_EQUAL(1u, dict.unsupportedData().size());
    CPPUNIT_ASSERT_EQUAL(String("UFID/supermihi@web.de"), dict.unsupportedData().front());
  }

  void testPropertyInterface2()
  {
    ID3v2::Tag tag;
    ID3v2::UnsynchronizedLyricsFrame *frame1 = new ID3v2::UnsynchronizedLyricsFrame();
    frame1->setDescription("test");
    frame1->setText("la-la-la test");
    tag.addFrame(frame1);

    ID3v2::UnsynchronizedLyricsFrame *frame2 = new ID3v2::UnsynchronizedLyricsFrame();
    frame2->setDescription("");
    frame2->setText("la-la-la nodescription");
    tag.addFrame(frame2);

    ID3v2::AttachedPictureFrame *frame3 = new ID3v2::AttachedPictureFrame();
    frame3->setDescription("test picture");
    tag.addFrame(frame3);

    ID3v2::TextIdentificationFrame *frame4 = new ID3v2::TextIdentificationFrame("TIPL");
    frame4->setText("single value is invalid for TIPL");
    tag.addFrame(frame4);

    ID3v2::TextIdentificationFrame *frame5 = new ID3v2::TextIdentificationFrame("TMCL");
    StringList tmclData;
    tmclData.append("VIOLIN");
    tmclData.append("a violinist");
    tmclData.append("PIANO");
    tmclData.append("a pianist");
    frame5->setText(tmclData);
    tag.addFrame(frame5);

    ID3v2::UniqueFileIdentifierFrame *frame6 = new ID3v2::UniqueFileIdentifierFrame("http://musicbrainz.org", "152454b9-19ba-49f3-9fc9-8fc26545cf41");
    tag.addFrame(frame6);

    ID3v2::UniqueFileIdentifierFrame *frame7 = new ID3v2::UniqueFileIdentifierFrame("http://example.com", "123");
    tag.addFrame(frame7);

    ID3v2::UserTextIdentificationFrame *frame8 = new ID3v2::UserTextIdentificationFrame();
    frame8->setDescription("MusicBrainz Album Id");
    frame8->setText("95c454a5-d7e0-4d8f-9900-db04aca98ab3");
    tag.addFrame(frame8);

    PropertyMap properties = tag.properties();

    CPPUNIT_ASSERT_EQUAL(3u, properties.unsupportedData().size());
    CPPUNIT_ASSERT(properties.unsupportedData().contains("TIPL"));
    CPPUNIT_ASSERT(properties.unsupportedData().contains("APIC"));
    CPPUNIT_ASSERT(properties.unsupportedData().contains("UFID/http://example.com"));

    CPPUNIT_ASSERT(properties.contains("PERFORMER:VIOLIN"));
    CPPUNIT_ASSERT(properties.contains("PERFORMER:PIANO"));
    CPPUNIT_ASSERT_EQUAL(String("a violinist"), properties["PERFORMER:VIOLIN"].front());
    CPPUNIT_ASSERT_EQUAL(String("a pianist"), properties["PERFORMER:PIANO"].front());

    CPPUNIT_ASSERT(properties.contains("LYRICS"));
    CPPUNIT_ASSERT(properties.contains("LYRICS:TEST"));

    CPPUNIT_ASSERT(properties.contains("MUSICBRAINZ_TRACKID"));
    CPPUNIT_ASSERT_EQUAL(String("152454b9-19ba-49f3-9fc9-8fc26545cf41"), properties["MUSICBRAINZ_TRACKID"].front());

    CPPUNIT_ASSERT(properties.contains("MUSICBRAINZ_ALBUMID"));
    CPPUNIT_ASSERT_EQUAL(String("95c454a5-d7e0-4d8f-9900-db04aca98ab3"), properties["MUSICBRAINZ_ALBUMID"].front());

    tag.removeUnsupportedProperties(properties.unsupportedData());
    CPPUNIT_ASSERT(tag.frameList("APIC").isEmpty());
    CPPUNIT_ASSERT(tag.frameList("TIPL").isEmpty());
    CPPUNIT_ASSERT_EQUAL((ID3v2::UniqueFileIdentifierFrame *)0, ID3v2::UniqueFileIdentifierFrame::findByOwner(&tag, "http://example.com"));
    CPPUNIT_ASSERT_EQUAL(frame6, ID3v2::UniqueFileIdentifierFrame::findByOwner(&tag, "http://musicbrainz.org"));
  }

  void testDeleteFrame()
  {
    ScopedFileCopy copy("rare_frames", ".mp3");
    string newname = copy.fileName();
    MPEG::File f(newname.c_str());
    ID3v2::Tag *t = f.ID3v2Tag();
    ID3v2::Frame *frame = t->frameList("TCON")[0];
    CPPUNIT_ASSERT_EQUAL(1u, t->frameList("TCON").size());
    t->removeFrame(frame, true);
    f.save(MPEG::File::ID3v2);
    
    MPEG::File f2(newname.c_str());
    t = f2.ID3v2Tag();
    CPPUNIT_ASSERT(t->frameList("TCON").isEmpty());
  }
  
  void testSaveAndStripID3v1ShouldNotAddFrameFromID3v1ToId3v2()
  {
    ScopedFileCopy copy("xing", ".mp3");
    string newname = copy.fileName();
    
    {
      MPEG::File foo(newname.c_str());
      foo.tag()->setArtist("Artist");
      foo.save(MPEG::File::ID3v1 | MPEG::File::ID3v2);
    }
    
    {
      MPEG::File bar(newname.c_str());
      bar.ID3v2Tag()->removeFrames("TPE1");
      // Should strip ID3v1 here and not add old values to ID3v2 again
      bar.save(MPEG::File::ID3v2, true);
    }
    
    MPEG::File f(newname.c_str());
    CPPUNIT_ASSERT(!f.ID3v2Tag()->frameListMap().contains("TPE1"));
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestID3v2);
