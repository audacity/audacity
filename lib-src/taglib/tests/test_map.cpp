#include <cppunit/extensions/HelperMacros.h>
#include <tstring.h>
#include <tmap.h>

using namespace std;
using namespace TagLib;

class TestMap : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestMap);
  CPPUNIT_TEST(testInsert);
  CPPUNIT_TEST_SUITE_END();

public:

  void testInsert()
  {
    Map<String, int> m;
    m.insert("foo", 3);
    CPPUNIT_ASSERT_EQUAL(3, m["foo"]);
    m.insert("foo", 7);
    CPPUNIT_ASSERT_EQUAL(7, m["foo"]);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestMap);
