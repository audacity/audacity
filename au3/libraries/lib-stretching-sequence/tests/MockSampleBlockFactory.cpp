/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MockSampleBlockFactory.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "MockSampleBlockFactory.h"

static SampleBlockFactory::Factory::Scope scope { [](AudacityProject&) {
        return std::make_shared<MockSampleBlockFactory>();
    } };
