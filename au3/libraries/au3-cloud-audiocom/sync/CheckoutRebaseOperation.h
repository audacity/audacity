/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CheckoutRebaseOperation.h

**********************************************************************/
#pragma once

#include <functional>
#include <memory>
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

//! What the server's head changed, ready to apply (see FetchHeadChanges)
struct HeadChanges;
using HeadChangesPtr = std::shared_ptr<const HeadChanges>;

//! Checks what the server's head changed compared with this instance's base
//! snapshot: only blocks edit-locked in the project may have been replaced
//! (what a checkout is allowed to do). If so, downloads the head's new blocks,
//! so that its changes can be applied to the open project without losing what's
//! unsaved there (see ApplyHeadChanges). Waits for the head to be fully synced.
/*!
 `onDone` is called on the main thread with the changes, or with the reason
 why they can't be taken.
 */
CLOUD_AUDIOCOM_API void FetchHeadChanges(AudacityProject& project,
                                         std::function<void(HeadChangesPtr changes, std::string error)> onDone);

//! Applies fetched changes to the open project: each replaced run of blocks is
//! swapped for the head's new blocks (stored afresh, so with ids of this
//! project's own), and the head becomes the project's base snapshot, so that
//! its next save builds on it. Must be called on the main thread.
//! @return empty on success, else the reason of the failure
CLOUD_AUDIOCOM_API std::string ApplyHeadChanges(AudacityProject& project, const HeadChanges& changes);
} // namespace audacity::cloud::audiocom::sync
