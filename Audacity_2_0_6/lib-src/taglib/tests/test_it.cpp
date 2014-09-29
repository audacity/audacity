/***************************************************************************
    copyright           : (C) 2011 by Mathias Panzenböck
    email               : grosser.meister.morti@gmx.net
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it  under the terms of the GNU Lesser General Public License version  *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *
 *   MA  02110-1301  USA                                                   *
 ***************************************************************************/

#include <itfile.h>
#include <tstringlist.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;
using TagLib::uint;

static const String titleBefore("test song name");
static const String titleAfter("changed title");

static const String commentBefore(
  "This is a sample name.\n"
  "In module file formats\n"
  "sample names are abused\n"
  "as multiline comments.\n"
  " ");

static const String newComment(
  "This is a sample name!\n"
  "In module file formats\n"
  "sample names are abused\n"
  "as multiline comments.\n"
  "-----------------------------------\n"
  "The previous line is truncated but starting with this line\n"
  "the comment is not limeted in the line length but to 8000\n"
  "additional characters (bytes).\n"
  "\n"
  "This is because it is saved in the 'message' proportion of\n"
  "IT files.");

static const String commentAfter(
  "This is a sample name!\n"
  "In module file formats\n"
  "sample names are abused\n"
  "as multiline comments.\n"
  "-------------------------\n"
  "The previous line is truncated but starting with this line\n"
  "the comment is not limeted in the line length but to 8000\n"
  "additional characters (bytes).\n"
  "\n"
  "This is because it is saved in the 'message' proportion of\n"
  "IT files.");

class TestIT : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestIT);
  CPPUNIT_TEST(testReadTags);
  CPPUNIT_TEST(testWriteTags);
  CPPUNIT_TEST_SUITE_END();

public:
  void testReadTags()
  {
    testRead(TEST_FILE_PATH_C("test.it"), titleBefore, commentBefore);
  }

  void testWriteTags()
  {
    ScopedFileCopy copy("test", ".it");
    {
      IT::File file(copy.fileName().c_str());
      CPPUNIT_ASSERT(file.tag() != 0);
      file.tag()->setTitle(titleAfter);
      file.tag()->setComment(newComment);
      file.tag()->setTrackerName("won't be saved");
      CPPUNIT_ASSERT(file.save());
    }
    testRead(copy.fileName().c_str(), titleAfter, commentAfter);
  }

private:
  void testRead(FileName fileName, const String &title, const String &comment)
  {
    IT::File file(fileName);

    CPPUNIT_ASSERT(file.isValid());

    IT::Properties *p = file.audioProperties();
    Mod::Tag *t = file.tag();

    CPPUNIT_ASSERT(0 != p);
    CPPUNIT_ASSERT(0 != t);

    CPPUNIT_ASSERT_EQUAL( 0, p->length());
    CPPUNIT_ASSERT_EQUAL( 0, p->bitrate());
    CPPUNIT_ASSERT_EQUAL( 0, p->sampleRate());
    CPPUNIT_ASSERT_EQUAL(64, p->channels());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)  0, p->lengthInPatterns());
    CPPUNIT_ASSERT_EQUAL(true, p->stereo());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)  0, p->instrumentCount());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)  5, p->sampleCount());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)  1, p->patternCount());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)535, p->version());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)532, p->compatibleVersion());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)  9, p->flags());
    CPPUNIT_ASSERT_EQUAL((TagLib::uchar)128, p->globalVolume());
    CPPUNIT_ASSERT_EQUAL((TagLib::uchar) 48, p->mixVolume());
    CPPUNIT_ASSERT_EQUAL((TagLib::uchar)125, p->tempo());
    CPPUNIT_ASSERT_EQUAL((TagLib::uchar)  6, p->bpmSpeed());
    CPPUNIT_ASSERT_EQUAL((TagLib::uchar)128, p->panningSeparation());
    CPPUNIT_ASSERT_EQUAL((TagLib::uchar)  0, p->pitchWheelDepth());
    CPPUNIT_ASSERT_EQUAL(title, t->title());
    CPPUNIT_ASSERT_EQUAL(String::null, t->artist());
    CPPUNIT_ASSERT_EQUAL(String::null, t->album());
    CPPUNIT_ASSERT_EQUAL(comment, t->comment());
    CPPUNIT_ASSERT_EQUAL(String::null, t->genre());
    CPPUNIT_ASSERT_EQUAL(0U, t->year());
    CPPUNIT_ASSERT_EQUAL(0U, t->track());
    CPPUNIT_ASSERT_EQUAL(String("Impulse Tracker"), t->trackerName());
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(TestIT);
