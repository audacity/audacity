/*
* Audacity: A Digital Audio Editor
*/
#include "recordmodule.h"

#include <QQmlEngine>
#include <QtQml>

#include "modularity/ioc.h"

#include "ui/iuiactionsregister.h"

#include "internal/recordconfiguration.h"
#include "internal/recordcontroller.h"
#include "internal/recordmetercontroller.h"
#include "internal/recorduiactions.h"
#include "internal/au3/au3record.h"
#include "view/common/recordmetermodel.h"
#include "view/common/leadinrecordingindicatormodel.h"

using namespace au::record;
using namespace muse;
using namespace muse::modularity;
using namespace muse::ui;
using namespace muse::actions;

static const std::string mname("record");

static void record_init_qrc()
{
    Q_INIT_RESOURCE(record);
}

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

void RecordModule::registerResources()
{
    record_init_qrc();
}

void RecordModule::registerUiTypes()
{
    qmlRegisterType<RecordMeterModel>("Audacity.Record", 1, 0, "RecordMeterModel");
    qmlRegisterType<LeadInRecordingIndicatorModel>("Audacity.Record", 1, 0, "LeadInRecordingIndicatorModel");
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
    m_uiActions = std::make_shared<RecordUiActions>(iocContext(), m_controller);
    m_record = std::make_shared<Au3Record>(iocContext());

    ioc()->registerExport<IRecordController>(mname, m_controller);
    ioc()->registerExport<IRecord>(mname, m_record);
}

void RecordContext::resolveImports()
{
}

void RecordContext::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_controller->init();
    m_uiActions->init();
    m_record->init();

    auto ar = ioc()->resolve<IUiActionsRegister>(mname);
    if (ar) {
        ar->reg(m_uiActions);
    }
}

void RecordContext::onDeinit()
{
    m_controller->deinit();
}
