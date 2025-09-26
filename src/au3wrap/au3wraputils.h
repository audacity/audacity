/*
 * Audacity: A Digital Audio Editor
 */

#pragma once

#include "global/types/ret.h"

#include "libraries/lib-basic-ui/BasicUI.h"

namespace au::au3 {
/*!
 * \brief A task to be executed on a worker thread.
 * \param au3progress Dialog interface needed by the task to provide progress updates.
 * \return `true` on success, `false` on failure.
 */
using Task = std::function<bool (::BasicUI::ProgressDialog& au3progress)>;

/*!
 * \brief Shows and updates an Au4 progress dialog on the main thread while executing `task` on a new, spawned thread
 * to minimize time spent in `task` waiting on progress-dialog refreshes.
 *
 * \param task The task to be executed.
 *
 * \param title Title of the progress dialog
 *
 * \param errorCode In case of an error (like a caught exception) when executing the task or if the task returns `false`,
 * the return code that should be given to the return value.
 * This is a convenience parameter: the user could also set the code on the returned `muse::Ret`.
 */
muse::Ret executeInBackground(Task task, std::string title, muse::Ret::Code errorCode = muse::Ret::Code::InternalError);
}
