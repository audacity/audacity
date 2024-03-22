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
#ifndef MU_GLOBAL_PROGRESS_H
#define MU_GLOBAL_PROGRESS_H

#include "async/channel.h"
#include "async/notification.h"

#include "types/retval.h"
#include "types/val.h"

namespace mu {
using ProgressResult = RetVal<Val>;

struct Progress
{
    // Communication from the creator of the progress to the clients
    async::Notification started;
    async::Channel<int64_t /*current*/, int64_t /*total*/, std::string /*title*/> progressChanged;
    async::Channel<ProgressResult> finished;

    // Communication from the clients to the creator of the progress that it should cancel its work
    async::Notification cancelRequested;

    void cancel()
    {
        cancelRequested.notify();
        finished.send(make_ret(Ret::Code::Cancel));
    }
};

using ProgressPtr = std::shared_ptr<Progress>;
}

#ifndef NO_QT_SUPPORT
Q_DECLARE_METATYPE(mu::Progress*)
#endif

#endif // MU_GLOBAL_PROGRESS_H
