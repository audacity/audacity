/*
* Audacity: A Digital Audio Editor
*/
#include "recordmodule.h"

#include "modularity/ioc.h"

#include "framework/interactive/iinteractiveuriregister.h"
#include "rcommand/icommandsregister.h"
#include "rcommand/icommandsstate.h"

#include "internal/recordcommandsregister.h"
#include "internal/recordcommandsstate.h"
#include "internal/recordconfiguration.h"
#include "internal/recordcontroller.h"
#include "internal/recordmetercontroller.h"
#include "internal/timedrecordingscenario.h"
#include "internal/au3/au3record.h"

using namespace au::record;
using namespace muse;
using namespace muse::modularity;

static const std::string mname("record");

std::string RecordModule::moduleName() const
{
    return mname;
}

void RecordModule::registerExports()
{
    m_configuration = std::make_shared<RecordConfiguration>();
    m_meterController = std::make_shared<RecordMeterController>();

    globalIoc()->registerExport<IRecordConfiguration>(mname, m_configuration);
    globalIoc()->registerExport<IRecordMeterController>(mname, m_meterController);
}

void RecordModule::resolveImports()
{
    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://record/timedrecording"), "Audacity.Record", "TimedRecordingDialog");
        ir->registerQmlUri(muse::Uri("audacity://record/timedrecording/progress"), "Audacity.Record", "TimedRecordingProgressDialog");
    }

    auto cr = globalIoc()->resolve<muse::rcommand::ICommandsRegister>(mname);
    if (cr) {
        cr->reg(std::make_shared<RecordCommandsRegister>());
    }
}

void RecordModule::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_configuration->init();
}

IContextSetup* RecordModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new RecordContext(ctx);
}

// =====================================================
// RecordContext
// =====================================================

void RecordContext::registerExports()
{
    m_controller = std::make_shared<RecordController>(iocContext());
    m_record = std::make_shared<Au3Record>(iocContext());
    m_timedRecordingScenario = std::make_shared<TimedRecordingScenario>(iocContext());

    ioc()->registerExport<IRecordController>(mname, m_controller);
    ioc()->registerExport<IRecord>(mname, m_record);
    ioc()->registerExport<ITimedRecordingScenario>(mname, m_timedRecordingScenario);
}

void RecordContext::resolveImports()
{
    auto cs = ioc()->resolve<muse::rcommand::ICommandsState>(mname);
    if (cs) {
        cs->reg(std::make_shared<RecordCommandsState>(iocContext()));
    }
}

void RecordContext::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_controller->init();
    m_record->init();
    m_timedRecordingScenario->init();
}

void RecordContext::onDeinit()
{
    m_controller->deinit();
}
