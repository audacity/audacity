/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
#include "newprojectmodel.h"
#include "ui/view/musicalsymbolcodes.h"
#include "log.h"

using namespace au::project;
using namespace muse;
using namespace muse::ui;

NewProjectModel::NewProjectModel(QObject* parent)
    : QObject(parent)
{
}

QString NewProjectModel::preferredProjectCreationMode() const
{
    return QString();

    // switch (configuration()->preferredScoreCreationMode()) {
    // case PreferredScoreCreationMode::FromInstruments: return "FromInstruments";
    // case PreferredScoreCreationMode::FromTemplate: return "FromTemplate";
    // }

    // return "";
}

bool NewProjectModel::createProject(const QVariant& info)
{
    return false;
    // ProjectCreateOptions options = parseOptions(info.toMap());

    // auto project = notationCreator()->newProject();
    // Ret ret = project->createNew(options);

    // if (!ret) {
    //     LOGE() << ret.toString();
    //     return false;
    // }

    // globalContext()->setCurrentProject(project);

    // bool isScoreCreatedFromInstruments = options.templatePath.empty();
    // updatePreferredScoreCreationMode(isScoreCreatedFromInstruments);

    // return true;
}

ProjectCreateOptions NewProjectModel::parseOptions(const QVariantMap& info) const
{
    return ProjectCreateOptions();

    // ProjectCreateOptions projectOptions;

    // projectOptions.title = info["title"].toString();
    // projectOptions.subtitle = info["subtitle"].toString();
    // projectOptions.composer = info["composer"].toString();
    // projectOptions.lyricist = info["lyricist"].toString();
    // projectOptions.copyright = info["copyright"].toString();

    // projectOptions.templatePath = info["templatePath"].toString();

    // ScoreCreateOptions& scoreOptions = projectOptions.projectOptions;

    // scoreOptions.withTempo = info["withTempo"].toBool();

    // QVariantMap orderMap = info["scoreOrder"].toMap();
    // scoreOptions.order = instrumentsRepository()->order(orderMap["id"].toString().toStdString());
    // scoreOptions.order.customized = orderMap["customized"].toBool();

    // return projectOptions;
}

void NewProjectModel::updatePreferredProjectCreationMode(bool isProjectCreatedFromInstruments)
{
    // if (isScoreCreatedFromInstruments) {
    //     configuration()->setPreferredScoreCreationMode(PreferredScoreCreationMode::FromInstruments);
    // } else {
    //     configuration()->setPreferredScoreCreationMode(PreferredScoreCreationMode::FromTemplate);
    // }
}
