/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: CryptoTests.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include <catch2/catch.hpp>

#include "crypto/SHA256.h"

TEST_CASE("SHA256", "")
{
    crypto::SHA256 sha256;

    REQUIRE(sha256.Finalize() == "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855");

    sha256.Update("a");

    REQUIRE(
        sha256.Finalize()
        == "CA978112CA1BBDCAFAC231B39A23DC4DA786EFF8147C4E72B9807785AFEE48BB");

    sha256.Update("bc", 2);

    REQUIRE(
        sha256.Finalize()
        == "1E0BBD6C686BA050B8EB03FFEEDC64FDC9D80947FCE821ABBE5D6DC8D252C5AC");

    REQUIRE(
        crypto::sha256("Audacity")
        == "CF21DBCF13FC6B66CCBF713B88AD02EC1EF0C6196ACAF28B0FB48D04858A5D04");

    REQUIRE(
        crypto::sha256(
            " is a free, open source, cross-platform audio software for multi-track recording and editing.")
        == "00E7C81A5357B1734035CE4CAE5DC0B3F886D22C8AF2E3952E2F5569A994B8A8");
}
