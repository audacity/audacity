/*
* Audacity: A Digital Audio Editor
*/

#include "au3tagsaccessor.h"
#include "project/types/projectmeta.h"

#include "au3wrap/au3types.h"

using namespace au::project;

ProjectMeta Au3TagsAccessor::tags() const
{
    auto project = reinterpret_cast<au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());

    ProjectMeta au4Tags;
    Tags& au3Tags = Tags::Get(*project);

    for (auto row : au3Tags.GetRange()) {
        auto tag = row.first.ToStdString();
        if (tag == TITLE_TAG) {
            au4Tags.title = QString::fromStdString(row.second.ToStdString());
        }

        const QString key = QString::fromStdString(row.first.ToStdString());
        const QString val = QString::fromStdString(row.second.ToStdString());

        bool assigned = false;
        // assign to standard fields if matched
        for (size_t i = 0; i < project::standardTags.size(); ++i) {
            if (key == project::standardTags[i]) {
                au4Tags.*(kStdMembers[i]) = val;
                assigned = true;
                break;
            }
        }

        if (!assigned) {
            au4Tags.additionalTags.insert(key, val);
        }
    }

    return au4Tags;
}

void Au3TagsAccessor::setTags(ProjectMeta au4Tags)
{
    auto project = reinterpret_cast<au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());

    Tags& au3Tags = Tags::Get(*project);

    for (size_t i = 0; i < project::standardTags.size(); ++i) {
        const QString& key = project::standardTags[i];
        const QString& val = au4Tags.*(kStdMembers[i]);

        au3Tags.SetTag(key.toStdString(), val.toStdString());
    }

    for (auto it = au4Tags.additionalTags.cbegin(); it != au4Tags.additionalTags.cend(); ++it) {
        const QString& key = it.key();
        if (key.isEmpty()) {
            continue;
        }

        const QString val = it.value().toString();

        au3Tags.SetTag(key.toStdString(), val.toStdString());
    }
}
