/* Copyright (C) 2003 Scott Wheeler <wheeler@kde.org>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <cppunit/extensions/HelperMacros.h>
#include <id3v2synchdata.h>

using namespace std;
using namespace TagLib;

class TestID3v2SynchData : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestID3v2SynchData);
  CPPUNIT_TEST(test1);
  CPPUNIT_TEST(test2);
  CPPUNIT_TEST(test3);
  CPPUNIT_TEST(testDecode1);
  CPPUNIT_TEST(testDecode2);
  CPPUNIT_TEST_SUITE_END();

public:

  void test1()
  {
    char data[] = { 0, 0, 0, 127 };
    ByteVector v(data, 4);

    CPPUNIT_ASSERT_EQUAL(ID3v2::SynchData::toUInt(v), TagLib::uint(127));
    CPPUNIT_ASSERT_EQUAL(ID3v2::SynchData::fromUInt(127), v);
  }

  void test2()
  {
    char data[] = { 0, 0, 1, 0 };
    ByteVector v(data, 4);

    CPPUNIT_ASSERT_EQUAL(ID3v2::SynchData::toUInt(v), TagLib::uint(128));
    CPPUNIT_ASSERT_EQUAL(ID3v2::SynchData::fromUInt(128), v);
  }

  void test3()
  {
    char data[] = { 0, 0, 1, 1 };
    ByteVector v(data, 4);

    CPPUNIT_ASSERT_EQUAL(ID3v2::SynchData::toUInt(v), TagLib::uint(129));
    CPPUNIT_ASSERT_EQUAL(ID3v2::SynchData::fromUInt(129), v);
  }

  void testDecode1()
  {
    ByteVector a("\xff\x00\x00", 3);
    a = ID3v2::SynchData::decode(a);
    CPPUNIT_ASSERT_EQUAL((unsigned int)2, a.size());
    CPPUNIT_ASSERT_EQUAL(ByteVector("\xff\x00", 2), a);
  }

  void testDecode2()
  {
    ByteVector a("\xff\x44", 2);
    a = ID3v2::SynchData::decode(a);
    CPPUNIT_ASSERT_EQUAL((unsigned int)2, a.size());
    CPPUNIT_ASSERT_EQUAL(ByteVector("\xff\x44", 2), a);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestID3v2SynchData);
