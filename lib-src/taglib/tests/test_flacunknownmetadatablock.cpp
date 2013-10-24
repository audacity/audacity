#include <string>
#include <stdio.h>
#include <tag.h>
#include <tstringlist.h>
#include <tbytevectorlist.h>
#include <flacunknownmetadatablock.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestFLACUnknownMetadataBlock : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestFLACUnknownMetadataBlock);
  CPPUNIT_TEST(testAccessors);
  CPPUNIT_TEST_SUITE_END();

public:

  void testAccessors()
  {
    ByteVector data("abc\x01", 4);
    FLAC::UnknownMetadataBlock block(42, data);
    CPPUNIT_ASSERT_EQUAL(42, block.code());
    CPPUNIT_ASSERT_EQUAL(data, block.data());
    CPPUNIT_ASSERT_EQUAL(data, block.render());
    ByteVector data2("xxx", 3);
    block.setCode(13);
    block.setData(data2);
    CPPUNIT_ASSERT_EQUAL(13, block.code());
    CPPUNIT_ASSERT_EQUAL(data2, block.data());
    CPPUNIT_ASSERT_EQUAL(data2, block.render());
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestFLACUnknownMetadataBlock);
