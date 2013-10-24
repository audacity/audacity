#include <string>
#include <stdio.h>
#include <tag.h>
#include <tbytevectorlist.h>
#include <rifffile.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class PublicRIFF : public RIFF::File
{
public:
  PublicRIFF(FileName file) : RIFF::File(file, BigEndian) {};
  TagLib::uint riffSize() { return RIFF::File::riffSize(); };
  TagLib::uint chunkCount() { return RIFF::File::chunkCount(); };
  TagLib::uint chunkOffset(TagLib::uint i) { return RIFF::File::chunkOffset(i); };
  TagLib::uint chunkPadding(TagLib::uint i) { return RIFF::File::chunkPadding(i); };
  TagLib::uint chunkDataSize(TagLib::uint i) { return RIFF::File::chunkDataSize(i); };
  ByteVector chunkName(TagLib::uint i) { return RIFF::File::chunkName(i); };
  ByteVector chunkData(TagLib::uint i) { return RIFF::File::chunkData(i); };
  void setChunkData(const ByteVector &name, const ByteVector &data) {
    RIFF::File::setChunkData(name, data);
  };
  virtual TagLib::Tag* tag() const { return 0; };
  virtual TagLib::AudioProperties* audioProperties() const { return 0;};
  virtual bool save() { return false; };
};

class TestRIFF : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestRIFF);
  CPPUNIT_TEST(testPadding);
  CPPUNIT_TEST(testLastChunkAtEvenPosition);
  CPPUNIT_TEST(testLastChunkAtEvenPosition2);
  CPPUNIT_TEST(testLastChunkAtEvenPosition3);
  CPPUNIT_TEST_SUITE_END();

public:

  void testPadding()
  {
    ScopedFileCopy copy("empty", ".aiff");
    string filename = copy.fileName();

    PublicRIFF *f = new PublicRIFF(filename.c_str());
    CPPUNIT_ASSERT_EQUAL(ByteVector("TEST"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0x1728 + 8), f->chunkOffset(2));

    f->setChunkData("TEST", "foo");
    delete f;

    f = new PublicRIFF(filename.c_str());
    CPPUNIT_ASSERT_EQUAL(ByteVector("TEST"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("foo"), f->chunkData(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(3), f->chunkDataSize(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0x1728 + 8), f->chunkOffset(2));

    f->setChunkData("SSND", "abcd");

    CPPUNIT_ASSERT_EQUAL(ByteVector("SSND"), f->chunkName(1));
    CPPUNIT_ASSERT_EQUAL(ByteVector("abcd"), f->chunkData(1));

    f->seek(f->chunkOffset(1));
    CPPUNIT_ASSERT_EQUAL(ByteVector("abcd"), f->readBlock(4));

    CPPUNIT_ASSERT_EQUAL(ByteVector("TEST"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("foo"), f->chunkData(2));

    f->seek(f->chunkOffset(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("foo"), f->readBlock(3));

    delete f;

    f = new PublicRIFF(filename.c_str());

    CPPUNIT_ASSERT_EQUAL(ByteVector("SSND"), f->chunkName(1));
    CPPUNIT_ASSERT_EQUAL(ByteVector("abcd"), f->chunkData(1));

    CPPUNIT_ASSERT_EQUAL(ByteVector("TEST"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("foo"), f->chunkData(2));
  }

  void testLastChunkAtEvenPosition()
  {
    ScopedFileCopy copy("noise", ".aif");
    string filename = copy.fileName();

    PublicRIFF *f = new PublicRIFF(filename.c_str());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0xff0 + 8), f->chunkOffset(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(311), f->chunkDataSize(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("SSND"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), f->chunkPadding(2));
    CPPUNIT_ASSERT_EQUAL(long(4400), f->length());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4399 - 8), f->riffSize());
    f->setChunkData("TEST", "abcd");
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4088), f->chunkOffset(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(311), f->chunkDataSize(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("SSND"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), f->chunkPadding(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4408), f->chunkOffset(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4), f->chunkDataSize(3));
    CPPUNIT_ASSERT_EQUAL(ByteVector("TEST"), f->chunkName(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0), f->chunkPadding(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4412 - 8), f->riffSize());
    delete f;

    f = new PublicRIFF(filename.c_str());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4088), f->chunkOffset(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(311), f->chunkDataSize(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("SSND"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), f->chunkPadding(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4408), f->chunkOffset(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4), f->chunkDataSize(3));
    CPPUNIT_ASSERT_EQUAL(ByteVector("TEST"), f->chunkName(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0), f->chunkPadding(3));
    CPPUNIT_ASSERT_EQUAL(long(4412), f->length());
    delete f;
  }

  void testLastChunkAtEvenPosition2()
  {
    ScopedFileCopy copy("noise_odd", ".aif");
    string filename = copy.fileName();

    PublicRIFF *f = new PublicRIFF(filename.c_str());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0xff0 + 8), f->chunkOffset(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(311), f->chunkDataSize(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("SSND"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0), f->chunkPadding(2));
    CPPUNIT_ASSERT_EQUAL(long(4399), f->length());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4399 - 8), f->riffSize());
    f->setChunkData("TEST", "abcd");
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4088), f->chunkOffset(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(311), f->chunkDataSize(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("SSND"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), f->chunkPadding(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4408), f->chunkOffset(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4), f->chunkDataSize(3));
    CPPUNIT_ASSERT_EQUAL(ByteVector("TEST"), f->chunkName(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0), f->chunkPadding(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4412 - 8), f->riffSize());
    delete f;

    f = new PublicRIFF(filename.c_str());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4088), f->chunkOffset(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(311), f->chunkDataSize(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("SSND"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), f->chunkPadding(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4408), f->chunkOffset(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4), f->chunkDataSize(3));
    CPPUNIT_ASSERT_EQUAL(ByteVector("TEST"), f->chunkName(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0), f->chunkPadding(3));
    CPPUNIT_ASSERT_EQUAL(long(4412), f->length());
    delete f;
  }

  void testLastChunkAtEvenPosition3()
  {
    ScopedFileCopy copy("noise_odd", ".aif");
    string filename = copy.fileName();

    PublicRIFF *f = new PublicRIFF(filename.c_str());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0xff0 + 8), f->chunkOffset(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(311), f->chunkDataSize(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("SSND"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(0), f->chunkPadding(2));
    CPPUNIT_ASSERT_EQUAL(long(4399), f->length());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4399 - 8), f->riffSize());
    f->setChunkData("TEST", "abc");
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4088), f->chunkOffset(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(311), f->chunkDataSize(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("SSND"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), f->chunkPadding(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4408), f->chunkOffset(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(3), f->chunkDataSize(3));
    CPPUNIT_ASSERT_EQUAL(ByteVector("TEST"), f->chunkName(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), f->chunkPadding(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4411 - 8), f->riffSize());
    delete f;

    f = new PublicRIFF(filename.c_str());
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4088), f->chunkOffset(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(311), f->chunkDataSize(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("SSND"), f->chunkName(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), f->chunkPadding(2));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(4408), f->chunkOffset(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(3), f->chunkDataSize(3));
    CPPUNIT_ASSERT_EQUAL(ByteVector("TEST"), f->chunkName(3));
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), f->chunkPadding(3));
    CPPUNIT_ASSERT_EQUAL(long(4412), f->length());
    delete f;
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestRIFF);
