#include <string>
#include <stdio.h>
#include <tag.h>
#include <mp4coverart.h>
#include <mp4item.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestMP4Item : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestMP4Item);
  CPPUNIT_TEST(testCoverArtList);
  CPPUNIT_TEST_SUITE_END();

public:

  void testCoverArtList()
  {
    MP4::CoverArtList l;
    l.append(MP4::CoverArt(MP4::CoverArt::PNG, "foo"));
    l.append(MP4::CoverArt(MP4::CoverArt::JPEG, "bar"));

    MP4::Item i(l);
    MP4::CoverArtList l2 = i.toCoverArtList();

    CPPUNIT_ASSERT_EQUAL(MP4::CoverArt::PNG, l[0].format());
    CPPUNIT_ASSERT_EQUAL(ByteVector("foo"), l[0].data());
    CPPUNIT_ASSERT_EQUAL(MP4::CoverArt::JPEG, l[1].format());
    CPPUNIT_ASSERT_EQUAL(ByteVector("bar"), l[1].data());
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestMP4Item);
