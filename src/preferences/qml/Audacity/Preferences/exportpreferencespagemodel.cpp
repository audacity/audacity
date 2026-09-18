/*
 * Audacity: A Digital Audio Editor
 */
#include "exportpreferencespagemodel.h"

using namespace au::appshell;
using namespace au::project;

using SaveBehavior = au::preferences::SaveBehaviorPref::SaveBehavior;

ExportPreferencesPageModel::ExportPreferencesPageModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void ExportPreferencesPageModel::init()
{
    exportConfiguration()->askExportLocationTypeChanged().onNotify(this, [this]() {
        emit askExportLocationTypeChanged();
    });
}

bool ExportPreferencesPageModel::askExportLocationType() const
{
    return exportConfiguration()->askExportLocationType();
}

void ExportPreferencesPageModel::setAskExportLocationType(bool ask)
{
    if (ask == askExportLocationType()) {
        return;
    }

    exportConfiguration()->setAskExportLocationType(ask);
}

SaveBehavior ExportPreferencesPageModel::saveBehavior() const
{
    if (projectConfiguration()->shouldAskSaveLocationType()) {
        return SaveBehavior::AlwaysAsk;
    }

    switch (projectConfiguration()->lastUsedSaveLocationType()) {
    case SaveLocationType::Cloud:
        return SaveBehavior::AlwaysSaveToCloud;
    case SaveLocationType::Local:
        return SaveBehavior::AlwaysSaveToComputer;
    case SaveLocationType::Undefined:
        break;
    }

    return SaveBehavior::AlwaysAsk;
}

void ExportPreferencesPageModel::setSaveBehavior(SaveBehavior behavior)
{
    if (behavior == saveBehavior()) {
        return;
    }

    switch (behavior) {
    case SaveBehavior::AlwaysAsk:
        projectConfiguration()->setShouldAskSaveLocationType(true);
        break;
    case SaveBehavior::AlwaysSaveToCloud:
        projectConfiguration()->setShouldAskSaveLocationType(false);
        projectConfiguration()->setLastUsedSaveLocationType(SaveLocationType::Cloud);
        break;
    case SaveBehavior::AlwaysSaveToComputer:
        projectConfiguration()->setShouldAskSaveLocationType(false);
        projectConfiguration()->setLastUsedSaveLocationType(SaveLocationType::Local);
        break;
    }

    emit saveBehaviorChanged();
}
