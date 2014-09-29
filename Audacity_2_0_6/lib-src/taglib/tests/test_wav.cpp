#include <cppunit/extensions/HelperMacros.h>
#include <string>
#include <stdio.h>
#include <tag.h>
#include <tbytevectorlist.h>
#include <wavfile.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestWAV : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestWAV);
  CPPUNIT_TEST(testLength);
  CPPUNIT_TEST(testZeroSizeDataChunk);
  CPPUNIT_TEST_SUITE_END();

public:

  void testLength()
  {
    RIFF::WAV::File f(TEST_FILE_PATH_C("empty.wav"));
    CPPUNIT_ASSERT_EQUAL(true, f.isValid());
    CPPUNIT_ASSERT_EQUAL(3, f.audioProperties()->length());
  }

  void testZeroSizeDataChunk()
  {
    RIFF::WAV::File f(TEST_FILE_PATH_C("zero-size-chunk.wav"));
    CPPUNIT_ASSERT_EQUAL(false, f.isValid());
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestWAV);
