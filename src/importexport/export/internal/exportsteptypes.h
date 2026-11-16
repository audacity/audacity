/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "macromanager/imacrosteptype.h"

#include "exportcommandoptions.h"

namespace au::importexport {
inline static const std::string EXPORT_AUDIO_STEP_TYPE("importexport/export/audio");
inline static const std::string EXPORT_SIMPLE_STEP_TYPE("importexport/export/simple");
inline static const muse::Uri EXPORT_AUDIO_STEP_DIALOG_URI("audacity://importexport/export/audio-step");
inline static const muse::Uri EXPORT_SIMPLE_STEP_DIALOG_URI("audacity://importexport/export/simple-step");
//: Macro step title: shown in the list of steps that can be added to a macro
inline static const muse::TranslatableString EXPORT_AUDIO_STEP_TITLE("export", "Export audio");
//: Macro step title: shown in the list of steps that can be added to a macro
inline static const muse::TranslatableString EXPORT_SIMPLE_STEP_TITLE("export", "Export audio (simple)");

class ExportStepType : public macromanager::IMacroStepType, public muse::Contextable, public muse::async::Asyncable
{
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher{ this };

public:
    explicit ExportStepType(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

protected:
    muse::async::Promise<muse::Ret> runExport(const ExportFormatCommand& command) const;
};

class ExportAudioStepType : public ExportStepType
{
public:
    using ExportStepType::ExportStepType;

    std::string id() const override;
    muse::TranslatableString title() const override;
    muse::String summary(const muse::Val& data) const override;
    muse::Val defaultData() const override;
    std::optional<muse::UriQuery> editUri() const override;
    muse::async::Promise<muse::Ret> execute(const muse::Val& data) const override;
};

class ExportSimpleStepType : public ExportStepType
{
public:
    using ExportStepType::ExportStepType;

    std::string id() const override;
    muse::TranslatableString title() const override;
    muse::String summary(const muse::Val& data) const override;
    muse::Val defaultData() const override;
    std::optional<muse::UriQuery> editUri() const override;
    muse::async::Promise<muse::Ret> execute(const muse::Val& data) const override;
};
}
