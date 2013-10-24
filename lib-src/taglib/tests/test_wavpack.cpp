#include <cppunit/extensions/HelperMacros.h>
#include <string>
#include <stdio.h>
#include <tag.h>
#include <tbytevectorlist.h>
#include <wavpackfile.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestWavPack : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestWavPack);
  CPPUNIT_TEST(testBasic);
  CPPUNIT_TEST(testLengthScan);
  CPPUNIT_TEST_SUITE_END();

public:

  void testBasic()
  {
    WavPack::File f(TEST_FILE_PATH_C("no_length.wv"));
    WavPack::Properties *props = f.audioProperties();
    CPPUNIT_ASSERT_EQUAL(44100, props->sampleRate());
    CPPUNIT_ASSERT_EQUAL(2, props->channels());
    CPPUNIT_ASSERT_EQUAL(1, props->bitrate());
    CPPUNIT_ASSERT_EQUAL(0x407, props->version());
  }

  void testLengthScan()
  {
    WavPack::File f(TEST_FILE_PATH_C("no_length.wv"));
    WavPack::Properties *props = f.audioProperties();
    CPPUNIT_ASSERT_EQUAL(4, props->length());
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestWavPack);
