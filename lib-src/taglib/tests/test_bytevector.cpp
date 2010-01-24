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
#include <tbytevector.h>
#include <tbytevectorlist.h>

using namespace std;
using namespace TagLib;

class TestByteVector : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestByteVector);
  CPPUNIT_TEST(testByteVector);
  CPPUNIT_TEST(testFind1);
  CPPUNIT_TEST(testFind2);
  CPPUNIT_TEST(testRfind1);
  CPPUNIT_TEST(testRfind2);
  CPPUNIT_TEST_SUITE_END();

public:

  void testConversion(unsigned int i, unsigned char a, unsigned char b, unsigned char c, unsigned char d)
  {
    ByteVector v(4, 0);

    v[3] = a;
    v[2] = b;
    v[1] = c;
    v[0] = d;
    CPPUNIT_ASSERT(v.toUInt(false) == i);

    v[0] = a;
    v[1] = b;
    v[2] = c;
    v[3] = d;
    CPPUNIT_ASSERT(v.toUInt() == i);
  }

  void testByteVector()
  {
    ByteVector v("foobar");

    CPPUNIT_ASSERT(v.find("ob") == 2);
    CPPUNIT_ASSERT(v.find('b') == 3);

    ByteVector n(4, 0);
    n[0] = 1;
    CPPUNIT_ASSERT(n.toUInt(true) == 16777216);
    CPPUNIT_ASSERT(n.toUInt(false) == 1);
    CPPUNIT_ASSERT(ByteVector::fromUInt(16777216, true) == n);
    CPPUNIT_ASSERT(ByteVector::fromUInt(1, false) == n);

    CPPUNIT_ASSERT(ByteVector::fromUInt(0xa0).toUInt() == 0xa0);

    testConversion(0x000000a0, 0x00, 0x00, 0x00, 0xa0);
    testConversion(0xd50bf072, 0xd5, 0x0b, 0xf0, 0x72);

    ByteVector intVector(2, 0);
    intVector[0] = char(0xfc);
    intVector[1] = char(0x00);
    CPPUNIT_ASSERT(intVector.toShort() == -1024);
    intVector[0] = char(0x04);
    intVector[1] = char(0x00);
    CPPUNIT_ASSERT(intVector.toShort() == 1024);

    CPPUNIT_ASSERT(ByteVector::fromLongLong(1).toLongLong() == 1);
    CPPUNIT_ASSERT(ByteVector::fromLongLong(0).toLongLong() == 0);
    CPPUNIT_ASSERT(ByteVector::fromLongLong(0xffffffffffffffffLL).toLongLong() == -1);
    CPPUNIT_ASSERT(ByteVector::fromLongLong(0xfffffffffffffffeLL).toLongLong() == -2);
    CPPUNIT_ASSERT(ByteVector::fromLongLong(1024).toLongLong() == 1024);

    ByteVector a1("foo");
    a1.append("bar");
    CPPUNIT_ASSERT(a1 == "foobar");

    ByteVector a2("foo");
    a2.append("b");
    CPPUNIT_ASSERT(a2 == "foob");

    ByteVector a3;
    a3.append("b");
    CPPUNIT_ASSERT(a3 == "b");

    ByteVector s1("foo");
    CPPUNIT_ASSERT(ByteVectorList::split(s1, " ").size() == 1);

    ByteVector s2("f");
    CPPUNIT_ASSERT(ByteVectorList::split(s2, " ").size() == 1);


    CPPUNIT_ASSERT(ByteVector().size() == 0);
    CPPUNIT_ASSERT(ByteVector("asdf").clear().size() == 0);
    CPPUNIT_ASSERT(ByteVector("asdf").clear() == ByteVector());
  }

  void testFind1()
  {
    CPPUNIT_ASSERT_EQUAL(4, ByteVector("....SggO."). find("SggO"));
    CPPUNIT_ASSERT_EQUAL(4, ByteVector("....SggO."). find("SggO", 0));
    CPPUNIT_ASSERT_EQUAL(4, ByteVector("....SggO."). find("SggO", 1));
    CPPUNIT_ASSERT_EQUAL(4, ByteVector("....SggO."). find("SggO", 2));
    CPPUNIT_ASSERT_EQUAL(4, ByteVector("....SggO."). find("SggO", 3));
    CPPUNIT_ASSERT_EQUAL(4, ByteVector("....SggO."). find("SggO", 4));
    CPPUNIT_ASSERT_EQUAL(-1, ByteVector("....SggO."). find("SggO", 5));
    CPPUNIT_ASSERT_EQUAL(-1, ByteVector("....SggO."). find("SggO", 6));
    CPPUNIT_ASSERT_EQUAL(-1, ByteVector("....SggO."). find("SggO", 7));
    CPPUNIT_ASSERT_EQUAL(-1, ByteVector("....SggO."). find("SggO", 8));
  }

  void testFind2()
  {
    CPPUNIT_ASSERT_EQUAL(0, ByteVector("\x01", 1).find("\x01"));
    CPPUNIT_ASSERT_EQUAL(0, ByteVector("\x01\x02", 2).find("\x01\x02"));
    CPPUNIT_ASSERT_EQUAL(-1, ByteVector("\x01", 1).find("\x02"));
    CPPUNIT_ASSERT_EQUAL(-1, ByteVector("\x01\x02", 2).find("\x01\x03"));
  }

  void testRfind1()
  {
    CPPUNIT_ASSERT_EQUAL(1, ByteVector(".OggS....").rfind("OggS", 0));
    CPPUNIT_ASSERT_EQUAL(1, ByteVector(".OggS....").rfind("OggS", 1));
    CPPUNIT_ASSERT_EQUAL(1, ByteVector(".OggS....").rfind("OggS", 2));
    CPPUNIT_ASSERT_EQUAL(1, ByteVector(".OggS....").rfind("OggS", 3));
    CPPUNIT_ASSERT_EQUAL(1, ByteVector(".OggS....").rfind("OggS", 4));
    CPPUNIT_ASSERT_EQUAL(1, ByteVector(".OggS....").rfind("OggS", 5));
    CPPUNIT_ASSERT_EQUAL(1, ByteVector(".OggS....").rfind("OggS", 6));
    CPPUNIT_ASSERT_EQUAL(1, ByteVector(".OggS....").rfind("OggS", 7));
    CPPUNIT_ASSERT_EQUAL(1, ByteVector(".OggS....").rfind("OggS", 8));
    CPPUNIT_ASSERT_EQUAL(1, ByteVector(".OggS....").rfind("OggS"));
  }

  void testRfind2()
  {
    ByteVector r0("**************");
    ByteVector r1("OggS**********");
    ByteVector r2("**********OggS");
    ByteVector r3("OggS******OggS");
    ByteVector r4("OggS*OggS*OggS");

    CPPUNIT_ASSERT_EQUAL(-1, r0.find("OggS"));
    CPPUNIT_ASSERT_EQUAL(-1, r0.rfind("OggS"));
    CPPUNIT_ASSERT_EQUAL(0, r1.find("OggS"));
    CPPUNIT_ASSERT_EQUAL(0, r1.rfind("OggS"));
    CPPUNIT_ASSERT_EQUAL(10, r2.find("OggS"));
    CPPUNIT_ASSERT_EQUAL(10, r2.rfind("OggS"));
    CPPUNIT_ASSERT_EQUAL(0, r3.find("OggS"));
    CPPUNIT_ASSERT_EQUAL(10, r3.rfind("OggS"));
    CPPUNIT_ASSERT_EQUAL(10, r4.rfind("OggS"));
    CPPUNIT_ASSERT_EQUAL(10, r4.rfind("OggS", 0));
    CPPUNIT_ASSERT_EQUAL(5, r4.rfind("OggS", 7));
    CPPUNIT_ASSERT_EQUAL(10, r4.rfind("OggS", 12));
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestByteVector);
