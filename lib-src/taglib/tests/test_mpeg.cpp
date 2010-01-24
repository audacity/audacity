#include <cppunit/extensions/HelperMacros.h>
#include <string>
#include <stdio.h>
#include <mpegfile.h>

using namespace std;
using namespace TagLib;

class TestMPEG : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestMPEG);
  CPPUNIT_TEST(testVersion2DurationWithXingHeader);
  CPPUNIT_TEST_SUITE_END();

public:

  void testVersion2DurationWithXingHeader()
  {
    MPEG::File f("data/mpeg2.mp3");
    CPPUNIT_ASSERT_EQUAL(5387, f.audioProperties()->length());
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestMPEG);
