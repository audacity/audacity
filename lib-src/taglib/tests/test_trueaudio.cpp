#include <cppunit/extensions/HelperMacros.h>
#include <string>
#include <stdio.h>
#include <trueaudiofile.h>

using namespace std;
using namespace TagLib;

class TestTrueAudio : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestTrueAudio);
  CPPUNIT_TEST(testReadPropertiesWithoutID3v2);
  CPPUNIT_TEST_SUITE_END();

public:

  void testReadPropertiesWithoutID3v2()
  {
    TrueAudio::File f("data/empty.tta");
    CPPUNIT_ASSERT(f.audioProperties());
    CPPUNIT_ASSERT_EQUAL(3, f.audioProperties()->length());
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestTrueAudio);
