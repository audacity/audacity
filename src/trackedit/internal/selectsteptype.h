/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "context/iglobalcontext.h"
#include "macromanager/imacrosteptype.h"

namespace au::trackedit {
inline static const std::string SELECT_ENHANCED_STEP_TYPE("trackedit/select/enhanced");
inline static const muse::Uri SELECT_STEP_DIALOG_URI("audacity://trackedit/select/step");
inline static const muse::TranslatableString SELECT_ENHANCED_STEP_TITLE("trackedit", "Select (enhanced)");

class SelectEnhancedStepType : public macromanager::IMacroStepType, public muse::Contextable, public muse::async::Asyncable
{
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher{ this };
    muse::ContextInject<context::IGlobalContext> globalContext{ this };

public:
    explicit SelectEnhancedStepType(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    std::string id() const override;
    muse::TranslatableString title() const override;
    muse::String summary(const muse::Val& data) const override;
    muse::Val defaultData() const override;
    std::optional<muse::UriQuery> editUri() const override;
    muse::async::Promise<muse::Ret> execute(const muse::Val& data) const override;
};
}
