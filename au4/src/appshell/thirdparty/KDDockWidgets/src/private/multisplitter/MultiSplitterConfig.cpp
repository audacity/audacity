/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "MultiSplitterConfig.h"
#include "Widget.h"
#include "Item_p.h"
#include "Separator_p.h"

#ifdef KDMULTISPLITTER_QTQUICK
#include <QQmlEngine>
#endif

namespace Layouting {

Config::Config()
{
    registerQmlTypes();
}

Separator *Config::createSeparator(Widget *parent) const
{
    if (m_separatorFactoryFunc)
        return m_separatorFactoryFunc(parent);

    return nullptr;
}

void Config::registerQmlTypes()
{
#ifdef KDMULTISPLITTER_QTQUICK
    qmlRegisterUncreatableType<Item>("com.kdab.kddockwidgets.multisplitter", 1, 0,
                                     "KDMultiSplitter", QStringLiteral("enum access"));
#endif
}

Config &Config::self()
{
    static Config config;
    return config;
}

Config::~Config()
{
}

int Config::separatorThickness() const
{
    // TODO: Make Item call Config::separatorThickness instead ?
    return Item::separatorThickness;
}

void Config::setSeparatorThickness(int value)
{
    if (value < 0 || value >= 100) {
        qWarning() << Q_FUNC_INFO << "Invalid value" << value;
        return;
    }

    Layouting::Item::separatorThickness = value;
}

void Config::setSeparatorFactoryFunc(SeparatorFactoryFunc func)
{
    if (m_separatorFactoryFunc && !func) {
        qWarning() << Q_FUNC_INFO << "Refusing to store nullptr separator func";
        return;
    }

    m_separatorFactoryFunc = func;
}

SeparatorFactoryFunc Config::separatorFactoryFunc() const
{
    return m_separatorFactoryFunc;
}

Config::Flags Config::flags() const
{
    return m_flags;
}

void Config::setFlags(Flags flags)
{
    if (m_flags == flags)
        return;

    // validations here, if any

    m_flags = flags;
}

}
