#include <tbytevectorstream.h>
#include <cppunit/extensions/HelperMacros.h>

using namespace std;
using namespace TagLib;

class TestByteVectorStream : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestByteVectorStream);
  CPPUNIT_TEST(testInitialData);
  CPPUNIT_TEST(testWriteBlock);
  CPPUNIT_TEST(testWriteBlockResize);
  CPPUNIT_TEST(testReadBlock);
  CPPUNIT_TEST(testRemoveBlock);
  CPPUNIT_TEST(testInsert);
  CPPUNIT_TEST_SUITE_END();

public:

  void testInitialData()
  {
    ByteVector v("abcd");
    ByteVectorStream stream(v);

    CPPUNIT_ASSERT_EQUAL(ByteVector("abcd"), *stream.data());
  }

  void testWriteBlock()
  {
    ByteVector v("abcd");
    ByteVectorStream stream(v);

    stream.seek(1);
    stream.writeBlock(ByteVector("xx"));
    CPPUNIT_ASSERT_EQUAL(ByteVector("axxd"), *stream.data());
  }

  void testWriteBlockResize()
  {
    ByteVector v("abcd");
    ByteVectorStream stream(v);

    stream.seek(3);
    stream.writeBlock(ByteVector("xx"));
    CPPUNIT_ASSERT_EQUAL(ByteVector("abcxx"), *stream.data());
    stream.seek(5);
    stream.writeBlock(ByteVector("yy"));
    CPPUNIT_ASSERT_EQUAL(ByteVector("abcxxyy"), *stream.data());
  }

  void testReadBlock()
  {
    ByteVector v("abcd");
    ByteVectorStream stream(v);

    CPPUNIT_ASSERT_EQUAL(ByteVector("a"), stream.readBlock(1));
    CPPUNIT_ASSERT_EQUAL(ByteVector("bc"), stream.readBlock(2));
    CPPUNIT_ASSERT_EQUAL(ByteVector("d"), stream.readBlock(3));
    CPPUNIT_ASSERT_EQUAL(ByteVector::null, stream.readBlock(3));
  }

  void testRemoveBlock()
  {
    ByteVector v("abcd");
    ByteVectorStream stream(v);

    stream.removeBlock(1, 1);
    CPPUNIT_ASSERT_EQUAL(ByteVector("acd"), *stream.data());
    stream.removeBlock(0, 2);
    CPPUNIT_ASSERT_EQUAL(ByteVector("d"), *stream.data());
    stream.removeBlock(0, 2);
    CPPUNIT_ASSERT_EQUAL(ByteVector(""), *stream.data());
  }

  void testInsert()
  {
    ByteVector v("abcd");
    ByteVectorStream stream(v);

    stream.insert(ByteVector("xx"), 1, 1);
    CPPUNIT_ASSERT_EQUAL(ByteVector("axxcd"), *stream.data());
    stream.insert(ByteVector("yy"), 0, 2);
    CPPUNIT_ASSERT_EQUAL(ByteVector("yyxcd"), *stream.data());
    stream.insert(ByteVector("foa"), 3, 2);
    CPPUNIT_ASSERT_EQUAL(ByteVector("yyxfoa"), *stream.data());
    stream.insert(ByteVector("123"), 3, 0);
    CPPUNIT_ASSERT_EQUAL(ByteVector("yyx123foa"), *stream.data());
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestByteVectorStream);
