/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <gtest/gtest.h>

#include "types/mnemonicstring.h"

#include "log.h"

using namespace mu;

class Global_MnemonicStringTests : public ::testing::Test
{
public:
};

TEST_F(Global_MnemonicStringTests, processMnemonic)
{
    {
        //! [GIVEN] A MnemonicString with the ampersand at the beginning
        MnemonicString mStr(TranslatableString::untranslatable("&Start Mnemonic"));

        //! [CHECK]
        EXPECT_EQ(mStr.qTranslatedWithMnemonicAmpersand(), "&Start Mnemonic");
        EXPECT_EQ(mStr.qTranslatedWithMnemonicUnderline(), "<u>S</u>tart Mnemonic");
        EXPECT_EQ(mStr.qTranslatedWithoutMnemonic(), "Start Mnemonic");
    }

    {
        //! [GIVEN] A MnemonicString with the ampersand in the middle
        MnemonicString mStr(TranslatableString::untranslatable("Mid&dle Mnemonic"));

        //! [CHECK]
        EXPECT_EQ(mStr.qTranslatedWithMnemonicAmpersand(), "Mid&dle Mnemonic");
        EXPECT_EQ(mStr.qTranslatedWithMnemonicUnderline(), "Mid<u>d</u>le Mnemonic");
        EXPECT_EQ(mStr.qTranslatedWithoutMnemonic(), "Middle Mnemonic");
    }

    {
        //! [GIVEN] A MnemonicString with an escaped ampersand (&&)
        MnemonicString mStr(TranslatableString::untranslatable("Ampersand && Mnemonic"));

        //! [CHECK]
        EXPECT_EQ(mStr.qTranslatedWithMnemonicAmpersand(), "Ampersand && Mnemonic");
        EXPECT_EQ(mStr.qTranslatedWithMnemonicUnderline(), "Ampersand & Mnemonic");
        EXPECT_EQ(mStr.qTranslatedWithoutMnemonic(), "Ampersand & Mnemonic");
    }

    {
        //! [GIVEN] A MnemonicString with a trailing ampersand (it's invalid, but shouldn't cause problems)
        MnemonicString mStr(TranslatableString::untranslatable("Trailing Mnemonic&"));

        //! [CHECK]
        EXPECT_EQ(mStr.qTranslatedWithMnemonicAmpersand(), "Trailing Mnemonic&");
        EXPECT_EQ(mStr.qTranslatedWithMnemonicUnderline(), "Trailing Mnemonic");
        EXPECT_EQ(mStr.qTranslatedWithoutMnemonic(), "Trailing Mnemonic");
    }

    {
        //! [GIVEN] A MnemonicString with multiple ampersands (it's invalid, but shouldn't cause problems)
        MnemonicString mStr(TranslatableString::untranslatable("Mu&lti Mnemo&nic"));

        //! [CHECK]
        EXPECT_EQ(mStr.qTranslatedWithMnemonicAmpersand(), "Mu&lti Mnemo&nic");
        EXPECT_EQ(mStr.qTranslatedWithMnemonicUnderline(), "Mu<u>l</u>ti Mnemo<u>n</u>ic");
        EXPECT_EQ(mStr.qTranslatedWithoutMnemonic(), "Multi Mnemonic");
    }

    {
        //! [GIVEN] A MnemonicString with a mnemonic ampersands and an escaped ampersand
        MnemonicString mStr(TranslatableString::untranslatable("Mu&lti && Mnemonic"));

        //! [CHECK]
        EXPECT_EQ(mStr.qTranslatedWithMnemonicAmpersand(), "Mu&lti && Mnemonic");
        EXPECT_EQ(mStr.qTranslatedWithMnemonicUnderline(), "Mu<u>l</u>ti & Mnemonic");
        EXPECT_EQ(mStr.qTranslatedWithoutMnemonic(), "Multi & Mnemonic");
    }
}
