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

#include <tstring.h>
#include <string.h>
#include <cppunit/extensions/HelperMacros.h>

using namespace std;
using namespace TagLib;

class TestString : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestString);
  CPPUNIT_TEST(testString);
  CPPUNIT_TEST(testRfind);
  CPPUNIT_TEST(testUTF16Encode);
  CPPUNIT_TEST(testUTF16Decode);
  CPPUNIT_TEST(testUTF16DecodeInvalidBOM);
  CPPUNIT_TEST(testUTF16DecodeEmptyWithBOM);
  CPPUNIT_TEST(testAppendCharDetach);
  CPPUNIT_TEST(testAppendStringDetach);
  CPPUNIT_TEST(testToInt);
  CPPUNIT_TEST(testSubstr);
  CPPUNIT_TEST(testNewline);
  CPPUNIT_TEST_SUITE_END();

public:

  void testString()
  {
    String s = "taglib string";
    ByteVector v = "taglib string";
    CPPUNIT_ASSERT(v == s.data(String::Latin1));

    char str[] = "taglib string";
    CPPUNIT_ASSERT(strcmp(s.toCString(), str) == 0);

    String unicode("José Carlos", String::UTF8);
    CPPUNIT_ASSERT(strcmp(unicode.toCString(), "Jos\xe9 Carlos") == 0);

    String latin = "Jos\xe9 Carlos";
    CPPUNIT_ASSERT(strcmp(latin.toCString(true), "José Carlos") == 0);

    String c;
    c = "1";
    CPPUNIT_ASSERT(c == L"1");

    c = L'\u4E00';
    CPPUNIT_ASSERT(c == L"\u4E00");

    String unicode2(unicode.to8Bit(true), String::UTF8);
    CPPUNIT_ASSERT(unicode == unicode2);

	String unicode3(L"\u65E5\u672C\u8A9E");
	CPPUNIT_ASSERT(*(unicode3.toCWString() + 1) == L'\u672C');

    CPPUNIT_ASSERT(strcmp(String::number(0).toCString(), "0") == 0);
    CPPUNIT_ASSERT(strcmp(String::number(12345678).toCString(), "12345678") == 0);
    CPPUNIT_ASSERT(strcmp(String::number(-12345678).toCString(), "-12345678") == 0);

    String n = "123";
    CPPUNIT_ASSERT(n.toInt() == 123);

    n = "-123";
    CPPUNIT_ASSERT(n.toInt() == -123);

    CPPUNIT_ASSERT(String("0").toInt() == 0);
    CPPUNIT_ASSERT(String("1").toInt() == 1);

    CPPUNIT_ASSERT(String("  foo  ").stripWhiteSpace() == String("foo"));
    CPPUNIT_ASSERT(String("foo    ").stripWhiteSpace() == String("foo"));
    CPPUNIT_ASSERT(String("    foo").stripWhiteSpace() == String("foo"));

    CPPUNIT_ASSERT(memcmp(String("foo").data(String::Latin1).data(), "foo", 3) == 0);
    CPPUNIT_ASSERT(memcmp(String("f").data(String::Latin1).data(), "f", 1) == 0);

    ByteVector utf16 = unicode.data(String::UTF16);

  // Check to make sure that the BOM is there and that the data size is correct

    CPPUNIT_ASSERT(utf16.size() == 2 + (unicode.size() * 2));

    CPPUNIT_ASSERT(unicode == String(utf16, String::UTF16));
  }

  void testUTF16Encode()
  {
    String a("foo");
    ByteVector b("\0f\0o\0o", 6);
    ByteVector c("f\0o\0o\0", 6);
    ByteVector d("\377\376f\0o\0o\0", 8);
    CPPUNIT_ASSERT(a.data(String::UTF16BE) != a.data(String::UTF16LE));
    CPPUNIT_ASSERT(b == a.data(String::UTF16BE));
    CPPUNIT_ASSERT(c == a.data(String::UTF16LE));
    CPPUNIT_ASSERT_EQUAL(d, a.data(String::UTF16));
  }

  void testUTF16Decode()
  {
    String a("foo");
    ByteVector b("\0f\0o\0o", 6);
    ByteVector c("f\0o\0o\0", 6);
    ByteVector d("\377\376f\0o\0o\0", 8);
    CPPUNIT_ASSERT_EQUAL(a, String(b, String::UTF16BE));
    CPPUNIT_ASSERT_EQUAL(a, String(c, String::UTF16LE));
    CPPUNIT_ASSERT_EQUAL(a, String(d, String::UTF16));
  }

  // this test is expected to print "TagLib: String::prepare() -
  // Invalid UTF16 string." on the console 3 times
  void testUTF16DecodeInvalidBOM()
  {
    ByteVector b(" ", 1);
    ByteVector c("  ", 2);
    ByteVector d("  \0f\0o\0o", 8);
    CPPUNIT_ASSERT_EQUAL(String(), String(b, String::UTF16));
    CPPUNIT_ASSERT_EQUAL(String(), String(c, String::UTF16));
    CPPUNIT_ASSERT_EQUAL(String(), String(d, String::UTF16));
  }

  void testUTF16DecodeEmptyWithBOM()
  {
    ByteVector a("\377\376", 2);
    ByteVector b("\376\377", 2);
    CPPUNIT_ASSERT_EQUAL(String(), String(a, String::UTF16));
    CPPUNIT_ASSERT_EQUAL(String(), String(b, String::UTF16));
  }

  void testAppendStringDetach()
  {
    String a("a");
    String b = a;
    a += "b";
    CPPUNIT_ASSERT_EQUAL(String("ab"), a);
    CPPUNIT_ASSERT_EQUAL(String("a"), b);
  }

  void testAppendCharDetach()
  {
    String a("a");
    String b = a;
    a += 'b';
    CPPUNIT_ASSERT_EQUAL(String("ab"), a);
    CPPUNIT_ASSERT_EQUAL(String("a"), b);
  }

  void testRfind()
  {
    CPPUNIT_ASSERT_EQUAL(-1, String("foo.bar").rfind(".", 0));
    CPPUNIT_ASSERT_EQUAL(-1, String("foo.bar").rfind(".", 1));
    CPPUNIT_ASSERT_EQUAL(-1, String("foo.bar").rfind(".", 2));
    CPPUNIT_ASSERT_EQUAL(3, String("foo.bar").rfind(".", 3));
    CPPUNIT_ASSERT_EQUAL(3, String("foo.bar").rfind(".", 4));
    CPPUNIT_ASSERT_EQUAL(3, String("foo.bar").rfind(".", 5));
    CPPUNIT_ASSERT_EQUAL(3, String("foo.bar").rfind(".", 6));
    CPPUNIT_ASSERT_EQUAL(3, String("foo.bar").rfind(".", 7));
    CPPUNIT_ASSERT_EQUAL(3, String("foo.bar").rfind("."));
  }

  void testToInt()
  {
    bool ok;
    CPPUNIT_ASSERT_EQUAL(String("123").toInt(&ok), 123);
    CPPUNIT_ASSERT_EQUAL(ok, true);

    CPPUNIT_ASSERT_EQUAL(String("-123").toInt(&ok), -123);
    CPPUNIT_ASSERT_EQUAL(ok, true);

    CPPUNIT_ASSERT_EQUAL(String("abc").toInt(&ok), 0);
    CPPUNIT_ASSERT_EQUAL(ok, false);

    CPPUNIT_ASSERT_EQUAL(String("1x").toInt(&ok), 1);
    CPPUNIT_ASSERT_EQUAL(ok, false);

    CPPUNIT_ASSERT_EQUAL(String("").toInt(&ok), 0);
    CPPUNIT_ASSERT_EQUAL(ok, false);

    CPPUNIT_ASSERT_EQUAL(String("-").toInt(&ok), 0);
    CPPUNIT_ASSERT_EQUAL(ok, false);

    CPPUNIT_ASSERT_EQUAL(String("123").toInt(), 123);
    CPPUNIT_ASSERT_EQUAL(String("-123").toInt(), -123);
    CPPUNIT_ASSERT_EQUAL(String("123aa").toInt(), 123);
    CPPUNIT_ASSERT_EQUAL(String("-123aa").toInt(), -123);
  }

  void testSubstr()
  {
    CPPUNIT_ASSERT_EQUAL(String("01"), String("0123456").substr(0, 2));
    CPPUNIT_ASSERT_EQUAL(String("12"), String("0123456").substr(1, 2));
    CPPUNIT_ASSERT_EQUAL(String("123456"), String("0123456").substr(1, 200));
  }

  void testNewline()
  {
    ByteVector cr("abc\x0dxyz", 7);
    ByteVector lf("abc\x0axyz", 7);
    ByteVector crlf("abc\x0d\x0axyz", 8);

    CPPUNIT_ASSERT_EQUAL(uint(7), String(cr).size());
    CPPUNIT_ASSERT_EQUAL(uint(7), String(lf).size());
    CPPUNIT_ASSERT_EQUAL(uint(8), String(crlf).size());

    CPPUNIT_ASSERT_EQUAL(L'\x0d', String(cr)[3]);
    CPPUNIT_ASSERT_EQUAL(L'\x0a', String(lf)[3]);
    CPPUNIT_ASSERT_EQUAL(L'\x0d', String(crlf)[3]);
    CPPUNIT_ASSERT_EQUAL(L'\x0a', String(crlf)[4]);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestString);
