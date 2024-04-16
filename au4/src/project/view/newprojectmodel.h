/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
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
#ifndef MU_PROJECT_NEWSCOREMODEL_H
#define MU_PROJECT_NEWSCOREMODEL_H

#include <QObject>

#include "modularity/ioc.h"

#include "project/iprojectconfiguration.h"
#include "project/iprojectcreator.h"
#include "notation/notationtypes.h"
#include "context/iglobalcontext.h"
#include "notation/iinstrumentsrepository.h"

namespace mu::project {
class NewScoreModel : public QObject
{
    Q_OBJECT

    INJECT(IProjectConfiguration, configuration)
    INJECT(IProjectCreator, notationCreator)
    INJECT(context::IGlobalContext, globalContext)
    INJECT(notation::IInstrumentsRepository, instrumentsRepository)

public:
    explicit NewScoreModel(QObject* parent = nullptr);

    Q_INVOKABLE QString preferredScoreCreationMode() const;
    Q_INVOKABLE bool createScore(const QVariant& info);

private:
    project::ProjectCreateOptions parseOptions(const QVariantMap& info) const;
    notation::DurationType noteIconToDurationType(int noteIconCode) const;
    void updatePreferredScoreCreationMode(bool isScoreCreatedFromInstruments);
};
}

#endif // MU_PROJECT_NEWSCOREMODEL_H
