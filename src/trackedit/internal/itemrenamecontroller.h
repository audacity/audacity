/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include "framework/actions/iactionsdispatcher.h"
#include "framework/actions/actionable.h"

#include "context/iglobalcontext.h"
#include "../iitemrenamecontroller.h"
#include "../iselectioncontroller.h"
#include "itracknavigationcontroller.h"

namespace au::trackedit {
class ItemRenameController : public IItemRenameController, public muse::actions::Actionable, public muse::async::Asyncable,
    public muse::Contextable
{
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<ISelectionController> selectionController { this };
    muse::ContextInject<ITrackNavigationController> trackNavigationController { this };

public:
    ItemRenameController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    void requestLabelTitleEdit(const LabelKey& labelKey) override;
    std::optional<LabelKey> pendingLabelTitleEdit() const override;
    void labelTitleEditRequestHandled() override;
    muse::async::Channel<LabelKey> labelTitleEditRequested() const override;

private:
    void renameSelectedItem();

    std::optional<LabelKey> m_pendingLabelTitleEdit;
    muse::async::Channel<LabelKey> m_labelTitleEditRequested;
};
}
