/*!
  @file UserException.cpp
  @brief implements UserException

  Created by Paul Licameli on 11/27/16.

*/

#include "UserException.h"
#include "BasicUI.h"

UserException::~UserException()
{
}

void UserException::DelayedHandlerAction()
{
}

void UserException::WithCancellableProgress(
    std::function<void(const ProgressReporter&)> action,
    TranslatableString title, TranslatableString message)
{
    using namespace BasicUI;
    auto progress
        =MakeProgress(std::move(title), std::move(message), ProgressShowCancel);
    const auto reportProgress = [&](double progressFraction) {
        const auto result = progress->Poll(progressFraction * 1000, 1000);
        if (result != ProgressResult::Success) {
            throw UserException {};
        }
    };
    action(reportProgress);
}
