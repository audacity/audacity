/*
* Audacity: A Digital Audio Editor
*/
#include "dynamicexportoptionsmodel.h"

namespace {
static QVariant toQVariant(const OptionValue& v)
{
    if (auto b = std::get_if<bool>(&v)) {
        return *b;
    }
    if (auto i = std::get_if<int>(&v)) {
        return *i;
    }
    if (auto s = std::get_if<std::string>(&v)) {
        return QString::fromUtf8(s->c_str());
    }

    return {};
}

static OptionValue fromQVariant(const QVariant& v)
{
    if (v.canConvert<bool>() && (v.typeId() == QMetaType::Bool)) {
        return v.toBool();
    }

    if (v.canConvert<int>()
        && (v.typeId() == QMetaType::Int || v.typeId() == QMetaType::LongLong || v.typeId() == QMetaType::UInt)) {
        return v.toInt();
    }

    if (v.canConvert<double>() && v.typeId() == QMetaType::Double) {
        return qRound(v.toDouble());
    }

    if (v.canConvert<QString>()) {
        return v.toString().toStdString();
    }

    return {};
}

static bool valuesEqual(const au::importexport::OptionValue& a, const au::importexport::OptionValue& b)
{
    if (a.index() != b.index()) {
        return false;
    }
    if (auto ca = std::get_if<bool>(&a)) {
        return *ca == *std::get_if<bool>(&b);
    }
    if (auto ca = std::get_if<int>(&a)) {
        return *ca == *std::get_if<int>(&b);
    }
    if (auto ca = std::get_if<std::string>(&a)) {
        return *ca == *std::get_if<std::string>(&b);
    }
    return false;
}
}

DynamicExportOptionsModel::DynamicExportOptionsModel(QObject*)
{
}

void DynamicExportOptionsModel::init()
{
    reloadFromEditor();

    exportConfiguration()->currentFormatChanged().onNotify(this, [this] {
        reloadFromEditor();
    });
}

int DynamicExportOptionsModel::rowCount(const QModelIndex& parent) const
{
    if (parent.isValid()) {
        return 0;
    }
    return m_rows.size();
}

QVariant DynamicExportOptionsModel::data(const QModelIndex& idx, int role) const
{
    if (!idx.isValid()) {
        return {};
    }

    const Row& r = m_rows[idx.row()];
    switch (role) {
    case IdRole: return r.id;
    case TitleRole: return r.title;
    case TypeRole: return r.type;
    case ReadOnlyRole: return r.flags.testFlag(ReadOnly);
    case HiddenRole: return r.flags.testFlag(Hidden);
    case ValueRole: return r.value;
    case MinRole: return r.min;
    case MaxRole: return r.max;
    case NamesRole: return r.names;
    case ValuesRole: return QVariant(r.values);
    }
    return {};
}

bool DynamicExportOptionsModel::setData(const QModelIndex& idx, const QVariant& v, int role)
{
    if (!idx.isValid()) {
        return false;
    }

    Row& r = m_rows[idx.row()];
    if (r.flags.testFlag(ReadOnly)) {
        return false;
    }
    if (r.value == v) {
        return false;
    }
    r.value = v;

    std::optional<au::importexport::ExportOption> option_opt = exporter()->option(idx.row());
    if (!option_opt.has_value()) {
        return false;
    }
    au::importexport::ExportOption option = option_opt.value();

    std::optional<au::importexport::OptionValue> value_opt = exporter()->value(option.id);
    if (!value_opt.has_value()) {
        return false;
    }
    au::importexport::OptionValue value = value_opt.value();

    value = fromQVariant(v);
    exporter()->setValue(option.id, value);

    reloadFromEditor();

    return true;
}

Qt::ItemFlags DynamicExportOptionsModel::flags(const QModelIndex& idx) const
{
    if (!idx.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags f = Qt::ItemIsEnabled;
    const auto& r = m_rows[idx.row()];
    if (!r.flags.testFlag(ReadOnly)) {
        f |= Qt::ItemIsEditable;
    }

    return f;
}

QHash<int, QByteArray> DynamicExportOptionsModel::roleNames() const
{
    return {
        { IdRole, "id" },
        { TitleRole, "title" },
        { TypeRole, "type" },
        { ReadOnlyRole, "readOnly" },
        { HiddenRole, "hidden" },
        { ValueRole, "value" },
        { MinRole, "min" },
        { MaxRole, "max" },
        { NamesRole, "names" },
        { ValuesRole, "values" },
    };
}

void DynamicExportOptionsModel::reloadFromEditor()
{
    std::vector<Row> newRows;
    newRows.reserve(exporter()->optionsCount());

    for (int i = 0; i < exporter()->optionsCount(); ++i) {
        std::optional<au::importexport::ExportOption> option_opt = exporter()->option(i);
        IF_ASSERT_FAILED(option_opt.has_value()) {
            continue;
        }
        au::importexport::ExportOption option = option_opt.value();

        std::optional<au::importexport::OptionValue> value_opt = exporter()->value(option.id);
        IF_ASSERT_FAILED(value_opt.has_value()) {
            continue;
        }
        au::importexport::OptionValue value = value_opt.value();

        Row r;
        r.id = option.id;
        r.title = QString::fromStdString(option.title);

        r.flags = Flags{};
        if (option.flags & au::importexport::ExportOption::ReadOnly) {
            r.flags |= ReadOnly;
        }
        if (option.flags & au::importexport::ExportOption::Hidden) {
            r.flags |= Hidden;
        }

        const auto typeMask = (option.flags & au::importexport::ExportOption::TypeMask);
        if (typeMask == au::importexport::ExportOption::TypeEnum) {
            r.type = ExportOptionType::Type::TypeEnum;

            r.names.clear();
            r.values.clear();
            r.names.reserve(option.values.size());
            r.values.reserve(option.values.size());

            int selectedIndex = -1;
            for (int idx = 0; idx < static_cast<int>(option.values.size()); ++idx) {
                if (idx < static_cast<int>(option.names.size())) {
                    r.names.push_back(QString::fromStdString(option.names[idx]));
                } else {
                    r.names.push_back(QStringLiteral("(unnamed)"));
                }

                r.values.push_back(toQVariant(option.values[idx]));
                if (valuesEqual(value, option.values[idx])) {
                    selectedIndex = idx;
                }
            }

            r.value = (selectedIndex >= 0) ? QVariant{ selectedIndex } : QVariant { -1 };
        } else if (typeMask == au::importexport::ExportOption::TypeRange) {
            r.type = ExportOptionType::Type::TypeRange;

            int minv = 0, maxv = 0;
            if (option.values.size() >= 2) {
                if (auto pmin = std::get_if<int>(&option.values[0])) {
                    minv = *pmin;
                }
                if (auto pmax = std::get_if<int>(&option.values[1])) {
                    maxv = *pmax;
                }
            }
            r.min = minv;
            r.max = maxv;

            if (auto pi = std::get_if<int>(&value)) {
                r.value = *pi;
            } else {
                r.value = minv;
            }
        } else {
            if (std::holds_alternative<bool>(value)) {
                r.type = ExportOptionType::Type::TypeBool;
                r.value = toQVariant(value);
            } else if (std::holds_alternative<std::string>(value)) {
                r.type = ExportOptionType::Type::TypeString;
                r.value = toQVariant(value);
            } else {
                r.type = ExportOptionType::Type::TypeString;
                r.value = {};
            }
        }

        newRows.push_back(std::move(r));
    }

    beginResetModel();
    m_rows = std::move(newRows);
    endResetModel();
}
