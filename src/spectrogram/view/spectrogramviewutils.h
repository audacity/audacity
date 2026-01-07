/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QString>

namespace au::spectrogram {
template<typename PropertyType>
struct TableEntry {
    const int index;
    const PropertyType value;
    const QString name;
};

template<typename PropertyType>
using Table = std::vector<TableEntry<PropertyType> >;

template<typename PropertyType>
QString propertyNameFromIndex(const Table<PropertyType>& table, int index)
{
    for (const auto& entry : table) {
        if (entry.index == index) {
            return entry.name;
        }
    }
    return QString();
}

template<typename PropertyType>
QList<QString> propertyNames(const Table<PropertyType>& table)
{
    QList<QString> names;
    names.reserve(static_cast<int>(table.size()));
    for (const auto& entry : table) {
        names.push_back(entry.name);
    }
    return names;
}

template<typename PropertyType>
QList<PropertyType> propertyValues(const Table<PropertyType>& table)
{
    QList<PropertyType> values;
    values.reserve(static_cast<int>(table.size()));
    for (const auto& entry : table) {
        values.push_back(entry.value);
    }
    return values;
}

template<typename PropertyType>
PropertyType propertyValue(const Table<PropertyType>& table, int index)
{
    for (const auto& entry : table) {
        if (entry.index == index) {
            return entry.value;
        }
    }
    return static_cast<PropertyType>(0);
}

template<typename PropertyType>
int propertyIndex(const Table<PropertyType>& table, PropertyType value)
{
    for (const auto& entry : table) {
        if (entry.value == value) {
            return entry.index;
        }
    }
    return 0;
}
}

#define CONNECT_SETTING_CHANGED(signal, control, roles) \
    connect(m_settingsModel, &AbstractSpectrogramSettingsModel::signal, this, \
            std::bind(&QAbstractListModel::dataChanged, this, \
                      index(control), index(control), roles));
