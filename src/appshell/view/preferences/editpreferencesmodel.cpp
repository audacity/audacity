#include "editpreferencesmodel.h"

#include "settings.h"

namespace au::appshell {
EditPreferencesModel::EditPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

void EditPreferencesModel::init()
{
    trackeditConfiguration()->askBeforeConvertingToMonoOrStereoChanged().onNotify(nullptr, [this]{
        emit askBeforeConvertingToMonoOrStereoChanged();
    });
}

bool EditPreferencesModel::askBeforeConvertingToMonoOrStereo() const
{
    return trackeditConfiguration()->askBeforeConvertingToMonoOrStereo();
}

void EditPreferencesModel::setAskBeforeConvertingToMonoOrStereo(bool value)
{
    trackeditConfiguration()->setAskBeforeConvertingToMonoOrStereo(value);
}
}
