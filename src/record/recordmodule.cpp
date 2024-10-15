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
#include "internal/recorduiactions.h"
#include "internal/au3/au3record.h"

using namespace au::record;
using namespace muse;
using namespace muse::modularity;
using namespace muse::ui;
using namespace muse::actions;

static void record_init_qrc()
{
    Q_INIT_RESOURCE(record);
}

std::string RecordModule::moduleName() const
{
    return "record";
}

void RecordModule::registerExports()
{
    m_configuration = std::make_shared<RecordConfiguration>();
    m_controller = std::make_shared<RecordController>();
    m_uiActions = std::make_shared<RecordUiActions>(m_controller);
    m_record = std::make_shared<Au3Record>();

    ioc()->registerExport<IRecordConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IRecordController>(moduleName(), m_controller);
    ioc()->registerExport<IRecord>(moduleName(), m_record);
}

void RecordModule::resolveImports()
{
    auto ar = ioc()->resolve<IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_uiActions);
    }
}

void RecordModule::registerResources()
{
    record_init_qrc();
}

void RecordModule::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_controller->init();
    m_uiActions->init();
    m_record->init();
}

void RecordModule::onDeinit()
{
    m_controller->deinit();
}
