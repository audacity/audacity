#include <string>
#include <stdio.h>
#include <tag.h>
#include <mp4coverart.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestMP4CoverArt : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestMP4CoverArt);
  CPPUNIT_TEST(testSimple);
  CPPUNIT_TEST(testList);
  CPPUNIT_TEST_SUITE_END();

public:

  void testSimple()
  {
    MP4::CoverArt c(MP4::CoverArt::PNG, "foo");
    CPPUNIT_ASSERT_EQUAL(MP4::CoverArt::PNG, c.format());
    CPPUNIT_ASSERT_EQUAL(ByteVector("foo"), c.data());

    MP4::CoverArt c2(c);
    CPPUNIT_ASSERT_EQUAL(MP4::CoverArt::PNG, c2.format());
    CPPUNIT_ASSERT_EQUAL(ByteVector("foo"), c2.data());

    MP4::CoverArt c3 = c;
    CPPUNIT_ASSERT_EQUAL(MP4::CoverArt::PNG, c3.format());
    CPPUNIT_ASSERT_EQUAL(ByteVector("foo"), c3.data());
  }

  void testList()
  {
    MP4::CoverArtList l;
    l.append(MP4::CoverArt(MP4::CoverArt::PNG, "foo"));
    l.append(MP4::CoverArt(MP4::CoverArt::JPEG, "bar"));

    CPPUNIT_ASSERT_EQUAL(MP4::CoverArt::PNG, l[0].format());
    CPPUNIT_ASSERT_EQUAL(ByteVector("foo"), l[0].data());
    CPPUNIT_ASSERT_EQUAL(MP4::CoverArt::JPEG, l[1].format());
    CPPUNIT_ASSERT_EQUAL(ByteVector("bar"), l[1].data());
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestMP4CoverArt);
