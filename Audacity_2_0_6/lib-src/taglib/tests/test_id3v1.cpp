#include <string>
#include <stdio.h>
#include <tstring.h>
#include <mpegfile.h>
#include <id3v1tag.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestID3v1 : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestID3v1);
  CPPUNIT_TEST(testStripWhiteSpace);
  CPPUNIT_TEST_SUITE_END();

public:

  void testStripWhiteSpace()
  {
    ScopedFileCopy copy("xing", ".mp3");
    string newname = copy.fileName();

    {
      MPEG::File f(newname.c_str());
      f.ID3v1Tag(true)->setArtist("Artist     ");
      f.save();
    }
    
    {
      MPEG::File f(newname.c_str());
      CPPUNIT_ASSERT(f.ID3v1Tag(false));
      CPPUNIT_ASSERT_EQUAL(String("Artist"), f.ID3v1Tag(false)->artist());
    }
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestID3v1);
