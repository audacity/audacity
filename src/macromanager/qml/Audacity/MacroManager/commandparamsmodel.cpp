/*
* Audacity: A Digital Audio Editor
*/
#include "commandparamsmodel.h"

using namespace au::macromanager;
using namespace muse;
using namespace muse::rcommand;

namespace {
CommandParamsModel::ParamType toParamType(DataType type)
{
    switch (type) {
    case DataType::Integer: return CommandParamsModel::ParamType::Integer;
    case DataType::Float: return CommandParamsModel::ParamType::Float;
    case DataType::Boolean: return CommandParamsModel::ParamType::Boolean;
    case DataType::Array: return CommandParamsModel::ParamType::Array;
    default: return CommandParamsModel::ParamType::String;
    }
}

QVariant defaultValue(CommandParamsModel::ParamType type, const QVariant& min)
{
    switch (type) {
    case CommandParamsModel::ParamType::Integer: return min.isValid() ? min.toInt() : 0;
    case CommandParamsModel::ParamType::Float: return min.isValid() ? min.toDouble() : 0.0;
    case CommandParamsModel::ParamType::Boolean: return false;
    default: return QString();
    }
}

QVariant boundValue(const Val& val)
{
    return val.isNull() ? QVariant() : val.toQVariant();
}

QVariant toParamValue(CommandParamsModel::ParamType type, const QVariant& value)
{
    switch (type) {
    case CommandParamsModel::ParamType::Integer: return value.toInt();
    case CommandParamsModel::ParamType::Float: return value.toDouble();
    case CommandParamsModel::ParamType::Boolean: return value.toBool();
    case CommandParamsModel::ParamType::Array: {
        QVariantList list;
        for (const QString& part : value.toString().split(',', Qt::SkipEmptyParts)) {
            list << part.trimmed();
        }
        return list;
    }
    default: return value.toString();
    }
}
}

CommandParamsModel::CommandParamsModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

QVariant CommandParamsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount()) {
        return QVariant();
    }

    const Item& item = m_items.at(index.row());

    switch (role) {
    case KeyRole: return item.key;
    case DescriptionRole: return item.description;
    case TypeRole: return static_cast<int>(item.type);
    case MinRole: return item.min;
    case MaxRole: return item.max;
    case ValueRole: return item.value;
    default: return QVariant();
    }
}

int CommandParamsModel::rowCount(const QModelIndex&) const
{
    return m_items.size();
}

QHash<int, QByteArray> CommandParamsModel::roleNames() const
{
    static const QHash<int, QByteArray> roles {
        { KeyRole, "key" },
        { DescriptionRole, "description" },
        { TypeRole, "type" },
        { MinRole, "min" },
        { MaxRole, "max" },
        { ValueRole, "value" }
    };

    return roles;
}

QString CommandParamsModel::title() const
{
    return m_title;
}

void CommandParamsModel::load(const QString& command, const QVariantMap& params)
{
    const CommandInfo& info = commandsRegister()->commandInfo(Command(command.toStdString()));

    beginResetModel();

    m_items.clear();
    for (const auto& [key, arg] : info.inputSchema.args) {
        Item item;
        item.key = QString::fromStdString(key);
        item.description = arg.description.toQString();
        item.type = toParamType(arg.type);
        item.min = boundValue(arg.min);
        item.max = boundValue(arg.max);

        const QVariant stored = params.value(item.key);
        if (item.type == ParamType::Array) {
            item.value = stored.toStringList().join(", ");
        } else {
            item.value = stored.isValid() ? stored : defaultValue(item.type, item.min);
        }

        m_items.append(item);
    }

    endResetModel();

    m_title = info.title.qTranslatedWithoutMnemonic();
    emit titleChanged();
}

void CommandParamsModel::setValue(int row, const QVariant& value)
{
    if (row < 0 || row >= m_items.size()) {
        return;
    }

    m_items[row].value = value;

    const QModelIndex idx = index(row);
    emit dataChanged(idx, idx, { ValueRole });
}

QVariantMap CommandParamsModel::params() const
{
    QVariantMap params;
    for (const Item& item : m_items) {
        params[item.key] = toParamValue(item.type, item.value);
    }

    return params;
}
