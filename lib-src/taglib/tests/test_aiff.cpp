#include <cppunit/extensions/HelperMacros.h>
#include <string>
#include <stdio.h>
#include <tag.h>
#include <tbytevectorlist.h>
#include <aifffile.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestAIFF : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestAIFF);
  CPPUNIT_TEST(testReading);
  CPPUNIT_TEST_SUITE_END();

public:

  void testReading()
  {
    ScopedFileCopy copy("empty", ".aiff");
    string filename = copy.fileName();

    RIFF::AIFF::File *f = new RIFF::AIFF::File(filename.c_str());
    CPPUNIT_ASSERT_EQUAL(705, f->audioProperties()->bitrate());
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestAIFF);
