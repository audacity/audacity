/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/


#ifndef KD_LAYOUT_SAVER_INSTANTIATOR_P_H
#define KD_LAYOUT_SAVER_INSTANTIATOR_P_H

#include "LayoutSaver.h"

#include <QObject>

class QQuickItem;

namespace KDDockWidgets {


/**
 * @brief A QObject wrapper around LayoutSaver so it can be used in QML.
 * Use it from QML, like: LayoutSaver { id: saver }
 * For C++, just use KDDockWidgets::LayoutSaver directly
 */
class DOCKS_EXPORT LayoutSaverInstantiator
    : public QObject,
      public LayoutSaver
{
    Q_OBJECT
public:
    explicit LayoutSaverInstantiator(QObject *parent = nullptr);
    ~LayoutSaverInstantiator() override;

    Q_INVOKABLE bool saveToFile(const QString &jsonFilename);
    Q_INVOKABLE bool restoreFromFile(const QString &jsonFilename);
};

}

#endif
