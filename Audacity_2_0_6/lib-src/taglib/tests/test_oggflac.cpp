#include <string>
#include <stdio.h>
#include <tag.h>
#include <tstringlist.h>
#include <tbytevectorlist.h>
#include <oggfile.h>
#include <oggflacfile.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

class TestOggFLAC : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestOggFLAC);
  CPPUNIT_TEST(testFramingBit);
  CPPUNIT_TEST_SUITE_END();

public:

  void testFramingBit()
  {
    ScopedFileCopy copy("empty_flac", ".oga");
    string newname = copy.fileName();

    Ogg::FLAC::File *f = new Ogg::FLAC::File(newname.c_str());
    f->tag()->setArtist("The Artist");
    f->save();
    delete f;

    f = new Ogg::FLAC::File(newname.c_str());
    CPPUNIT_ASSERT_EQUAL(String("The Artist"), f->tag()->artist());

    f->seek(0, File::End);
    int size = f->tell();
    CPPUNIT_ASSERT_EQUAL(9134, size);

    delete f;
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestOggFLAC);
