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
#ifndef MU_UI_IUIACTIONSREGISTER_H
#define MU_UI_IUIACTIONSREGISTER_H

#include <memory>

#include "modularity/imoduleinterface.h"
#include "iuiactionsmodule.h"
#include "uitypes.h"
#include "async/channel.h"

namespace mu::ui {
class IUiActionsRegister : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IUiActionsRegister)

public:
    virtual ~IUiActionsRegister() = default;

    virtual void reg(const IUiActionsModulePtr& actions) = 0;

    virtual const UiAction& action(const actions::ActionCode& code) const = 0;
    virtual const std::vector<UiAction> getActions() const = 0;
    virtual UiActionState actionState(const actions::ActionCode& code) const = 0;
    virtual async::Channel<actions::ActionCodeList> actionStateChanged() const = 0;
};
}

#endif // MU_UI_IUIACTIONSREGISTER_H
