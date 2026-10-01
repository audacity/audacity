/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CheckoutRebaseOperation.h

**********************************************************************/
#pragma once

#include <functional>
#include <string>

class AudacityProject;

namespace audacity::cloud::audiocom::sync {
//! Replays the audio changes of a cloud project's checkout on top of the
//! server's head and pushes the result as a new snapshot (see RebaseCheckout).
/*!
 For a checkout whose save was rejected because the project changed on the
 server. The checkout's own sync state is left as is: it stays based on its
 snapshot, so a further save from it conflicts and rebases again.

 The checkout may only have replaced the blocks it was given (those the main
 instance locked, everything else being locked in the checkout), which is
 checked before anything is pushed.

 Waits for the head to be fully synced first. `onDone` is called on the main
 thread, with an empty string on success, else the reason of the failure.
 */
CLOUD_AUDIOCOM_API void RebaseOntoHead(AudacityProject& project, std::function<void(std::string error)> onDone);
} // namespace audacity::cloud::audiocom::sync
