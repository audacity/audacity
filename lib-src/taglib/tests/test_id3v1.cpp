#include <cppunit/extensions/HelperMacros.h>
#include <string>
#include <stdio.h>
#include <id3v1tag.h>

using namespace std;
using namespace TagLib;

class TestID3v1 : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestID3v1);
  CPPUNIT_TEST(testStripWhiteSpace);
  CPPUNIT_TEST_SUITE_END();

public:

  void testStripWhiteSpace()
  {
    ID3v1::StringHandler h;
    CPPUNIT_ASSERT_EQUAL(String("Foo"), h.parse(ByteVector("Foo                ")));
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestID3v1);
