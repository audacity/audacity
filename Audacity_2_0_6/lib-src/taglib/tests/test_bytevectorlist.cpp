#include <tbytevector.h>
#include <tbytevectorlist.h>
#include <cppunit/extensions/HelperMacros.h>

using namespace std;
using namespace TagLib;

class TestByteVectorList : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestByteVectorList);
  CPPUNIT_TEST(testSplitSingleChar);
  CPPUNIT_TEST(testSplitSingleChar_2);
  CPPUNIT_TEST_SUITE_END();

public:

  void testSplitSingleChar()
  {
    ByteVector v("a b");

    ByteVectorList l = ByteVectorList::split(v, " ");
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(2), l.size());
    CPPUNIT_ASSERT_EQUAL(ByteVector("a"), l[0]);
    CPPUNIT_ASSERT_EQUAL(ByteVector("b"), l[1]);
  }

  void testSplitSingleChar_2()
  {
    ByteVector v("a");

    ByteVectorList l = ByteVectorList::split(v, " ");
    CPPUNIT_ASSERT_EQUAL(TagLib::uint(1), l.size());
    CPPUNIT_ASSERT_EQUAL(ByteVector("a"), l[0]);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestByteVectorList);
