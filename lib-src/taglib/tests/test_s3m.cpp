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

#include <s3mfile.h>
#include <cppunit/extensions/HelperMacros.h>
#include "utils.h"

using namespace std;
using namespace TagLib;

static const String titleBefore("test song name");
static const String titleAfter("changed title");

static const String commentBefore(
  "This is an instrument name.\n"
  "Module file formats\n"
  "abuse instrument names\n"
  "as multiline comments.\n"
  " ");

static const String newComment(
  "This is an instrument name!\n"
  "Module file formats\n"
  "abuse instrument names\n"
  "as multiline comments.\n"
  "-----------------------------------\n"
  "This line will be dropped and the previous is truncated.");

static const String commentAfter(
  "This is an instrument name!\n"
  "Module file formats\n"
  "abuse instrument names\n"
  "as multiline comments.\n"
  "---------------------------");

class TestS3M : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestS3M);
  CPPUNIT_TEST(testReadTags);
  CPPUNIT_TEST(testWriteTags);
  CPPUNIT_TEST_SUITE_END();

public:
  void testReadTags()
  {
    testRead(TEST_FILE_PATH_C("test.s3m"), titleBefore, commentBefore);
  }

  void testWriteTags()
  {
    ScopedFileCopy copy("test", ".s3m");
    {
      S3M::File file(copy.fileName().c_str());
      CPPUNIT_ASSERT(file.tag() != 0);
      file.tag()->setTitle(titleAfter);
      file.tag()->setComment(newComment);
      file.tag()->setTrackerName("won't be saved");
      CPPUNIT_ASSERT(file.save());
    }
    testRead(copy.fileName().c_str(), titleAfter, commentAfter);
    CPPUNIT_ASSERT(fileEqual(
      copy.fileName(),
      TEST_FILE_PATH_C("changed.s3m")));
  }

private:
  void testRead(FileName fileName, const String &title, const String &comment)
  {
    S3M::File file(fileName);

    CPPUNIT_ASSERT(file.isValid());

    S3M::Properties *p = file.audioProperties();
    Mod::Tag *t = file.tag();

    CPPUNIT_ASSERT(0 != p);
    CPPUNIT_ASSERT(0 != t);

    CPPUNIT_ASSERT_EQUAL( 0, p->length());
    CPPUNIT_ASSERT_EQUAL( 0, p->bitrate());
    CPPUNIT_ASSERT_EQUAL( 0, p->sampleRate());
    CPPUNIT_ASSERT_EQUAL(16, p->channels());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)   0, p->lengthInPatterns());
    CPPUNIT_ASSERT_EQUAL(false, p->stereo());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)   5, p->sampleCount());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)   1, p->patternCount());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)   0, p->flags());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)4896, p->trackerVersion());
    CPPUNIT_ASSERT_EQUAL((TagLib::ushort)   2, p->fileFormatVersion());
    CPPUNIT_ASSERT_EQUAL((TagLib::uchar) 64, p->globalVolume());
    CPPUNIT_ASSERT_EQUAL((TagLib::uchar) 48, p->masterVolume());
    CPPUNIT_ASSERT_EQUAL((TagLib::uchar)125, p->tempo());
    CPPUNIT_ASSERT_EQUAL((TagLib::uchar)  6, p->bpmSpeed());
    CPPUNIT_ASSERT_EQUAL(title, t->title());
    CPPUNIT_ASSERT_EQUAL(String::null, t->artist());
    CPPUNIT_ASSERT_EQUAL(String::null, t->album());
    CPPUNIT_ASSERT_EQUAL(comment, t->comment());
    CPPUNIT_ASSERT_EQUAL(String::null, t->genre());
    CPPUNIT_ASSERT_EQUAL(0U, t->year());
    CPPUNIT_ASSERT_EQUAL(0U, t->track());
    CPPUNIT_ASSERT_EQUAL(String("ScreamTracker III"), t->trackerName());
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(TestS3M);
