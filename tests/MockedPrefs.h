/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MockedPrefs.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

class MockedFileConfig;

struct MockedPrefs final
{
   MockedPrefs();
   ~MockedPrefs();

   MockedPrefs(const MockedPrefs&) = delete;
   MockedPrefs& operator=(const MockedPrefs&) = delete;
   MockedPrefs(MockedPrefs&&) = delete;
   MockedPrefs& operator=(MockedPrefs&&) = delete;

private:
   std::unique_ptr<MockedFileConfig> mConfig;
};
