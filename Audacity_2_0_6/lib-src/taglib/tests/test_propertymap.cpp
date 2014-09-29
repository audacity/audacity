#include <cppunit/extensions/HelperMacros.h>
#include <tpropertymap.h>
class TestPropertyMap : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestPropertyMap);
  CPPUNIT_TEST(testInvalidKeys);
  CPPUNIT_TEST_SUITE_END();

public:
  void testInvalidKeys()
  {
    TagLib::PropertyMap map1;
    CPPUNIT_ASSERT(map1.isEmpty());
    map1["ÄÖÜ"].append("test");
    CPPUNIT_ASSERT_EQUAL(map1.size(), 1u);

    TagLib::PropertyMap map2;
    map2["ÄÖÜ"].append("test");
    CPPUNIT_ASSERT(map1 == map2);
    CPPUNIT_ASSERT(map1.contains(map2));

    map2["ARTIST"] = TagLib::String("Test Artist");
    CPPUNIT_ASSERT(map1 != map2);
    CPPUNIT_ASSERT(map2.contains(map1));

    map2["ÄÖÜ"].append("test 2");
    CPPUNIT_ASSERT(!map2.contains(map1));

  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(TestPropertyMap);
