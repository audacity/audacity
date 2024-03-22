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
#ifndef MU_UI_UIACTIONSREGISTER_H
#define MU_UI_UIACTIONSREGISTER_H

#include <vector>
#include <unordered_map>

#include "../iuiactionsregister.h"
#include "modularity/ioc.h"
#include "shortcuts/ishortcutsregister.h"
#include "iuicontextresolver.h"
#include "async/asyncable.h"

namespace mu::ui {
class UiActionsRegister : public IUiActionsRegister, public async::Asyncable
{
    INJECT(IUiContextResolver, uicontextResolver)
    INJECT(shortcuts::IShortcutsRegister, shortcutsRegister)
public:
    UiActionsRegister() = default;

    void init();

    void reg(const IUiActionsModulePtr& actions) override;
    const UiAction& action(const actions::ActionCode& code) const override;
    const std::vector<UiAction> getActions() const override;
    UiActionState actionState(const actions::ActionCode& code) const override;
    async::Channel<actions::ActionCodeList> actionStateChanged() const override;

private:

    struct Info
    {
        IUiActionsModulePtr module;
        ui::UiAction action;
        ui::UiActionState state;

        bool isValid() const
        {
            return module != nullptr && action.isValid();
        }
    };

    Info& info(const actions::ActionCode& code);
    const Info& info(const actions::ActionCode& code) const;

    void updateShortcuts(const actions::ActionCodeList& codes);
    void updateShortcutsAll();

    void updateEnabled(const actions::ActionCodeList& codes);
    void updateEnabledAll();
    void doUpdateEnabled(Info& inf, const IUiContextResolverPtr& ctxResolver, const UiContext& currentCtx,
                         actions::ActionCodeList& changedList);

    void updateCheckedAll();
    void updateChecked(const actions::ActionCodeList& codes);

    std::unordered_map<actions::ActionCode, Info> m_actions;
    async::Channel<actions::ActionCodeList> m_actionStateChanged;
};
}

#endif // MU_UI_UIACTIONSREGISTER_H
