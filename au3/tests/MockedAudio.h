/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MockedAudio.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

struct MockedAudio final
{
    MockedAudio();
    ~MockedAudio();

    MockedAudio(const MockedAudio&) = delete;
    MockedAudio& operator=(const MockedAudio&) = delete;
    MockedAudio(MockedAudio&&) = delete;
    MockedAudio& operator=(MockedAudio&&) = delete;
};
