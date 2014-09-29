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
#include <tlist.h>

using namespace std;
using namespace TagLib;

class TestList : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestList);
  CPPUNIT_TEST(testList);
  CPPUNIT_TEST_SUITE_END();

public:

  void testList()
  {
    List<int> l1;
    List<int> l2;
    List<int> l3;
    l1.append(2);
    l2.append(3);
    l2.append(4);
    l1.append(l2);
    l1.prepend(1);
    l3.append(1);
    l3.append(2);
    l3.append(3);
    l3.append(4);
    CPPUNIT_ASSERT(l1 == l3);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestList);
