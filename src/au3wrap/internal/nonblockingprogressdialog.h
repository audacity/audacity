/*
 * Audacity: A Digital Audio Editor
 */

#pragma once

#include "BasicUI.h"

namespace au::effects::NonBlockingProgressDialog {
/*
 * \brief Shows and updates an Au4 progress dialog on the main thread while executing `task` on a new, spawned thread
 * to minimize time spent in `task` waiting on progress-dialog refreshes.
 *
 * \param task The task to be executed.
 * `au3dialog` is expected to be called upon by the task itself as it will cause updates in the Au4 progress dialog.
 * `canceled` reports whether the cancel button was pressed on the Au4 progress dialog.
 */
void execute(std::function<void(BasicUI::ProgressDialog& au3dialog, const bool& canceled)> task);
}
