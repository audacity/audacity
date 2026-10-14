/*
* Audacity: A Digital Audio Editor
*/
#include "macrocommandsmodel.h"

#include <algorithm>

#include "global/stringutils.h"

using namespace au::macromanager;
using namespace muse;
using namespace muse::rcommand;

namespace {
const Uri COMMAND_PARAMS_URI("audacity://macromanager/commandparams");
const std::string EFFECT_APPLY_PATH_PREFIX("effects/apply/");
const std::string EFFECT_CONFIGURE_PATH_PREFIX("effects/configure/");

bool isEffectApplyCommand(const Command& command)
{
    return strings::startsWith(command.path(), EFFECT_APPLY_PATH_PREFIX);
}

Command effectConfigureCommand(const Command& applyCommand)
{
    const std::string encodedId = applyCommand.path().substr(EFFECT_APPLY_PATH_PREFIX.size());
    return Command(std::string(COMMAND_SCHEME) + "://" + EFFECT_CONFIGURE_PATH_PREFIX + encodedId);
}
}

MacroCommandsModel::MacroCommandsModel(QObject* parent)
    : QAbstractListModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this)), m_runOnFilesLauncher(this)
{
}

QVariant MacroCommandsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || !isRowValid(index.row())) {
        return QVariant();
    }

    const MacroStep& step = m_macro.steps.at(index.row());

    switch (role) {
    case StepRole: return index.row() + 1;
    case CodeRole: return QString::fromStdString(step.command.toString());
    case TitleRole: return titleOf(step.command);
    case HasParamsRole: return hasParams(step.command);
    default: return QVariant();
    }
}

int MacroCommandsModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_macro.steps.size());
}

QHash<int, QByteArray> MacroCommandsModel::roleNames() const
{
    static const QHash<int, QByteArray> roles {
        { StepRole, "step" },
        { CodeRole, "code" },
        { TitleRole, "title" },
        { HasParamsRole, "hasParams" }
    };

    return roles;
}

bool MacroCommandsModel::isEmpty() const
{
    return m_macro.steps.empty();
}

bool MacroCommandsModel::isRunning() const
{
    return runner()->isRunning();
}

void MacroCommandsModel::load(const QString& macroName)
{
    beginResetModel();
    m_macro = repository()->macro(macroName.toStdString()).value_or(Macro { macroName.toStdString(), {} });
    endResetModel();

    emit isEmptyChanged();

    runner()->isRunningChanged().onNotify(this, [this]() {
        emit isRunningChanged();
    });
}

void MacroCommandsModel::add(const QString& code)
{
    insert(rowCount(), code);
}

void MacroCommandsModel::insert(int row, const QString& code)
{
    const Command command(code.toStdString());
    if (!command.isValid()) {
        return;
    }

    row = std::clamp(row, 0, rowCount());

    beginInsertRows(QModelIndex(), row, row);
    m_macro.steps.insert(m_macro.steps.begin() + row, MacroStep { command, {} });
    endInsertRows();

    notifyStepsChanged(row + 1, rowCount() - 1);
    emit isEmptyChanged();
}

void MacroCommandsModel::move(int from, int to)
{
    to = std::clamp(to, 0, rowCount() - 1);

    if (!isRowValid(from) || from == to) {
        return;
    }

    beginMoveRows(QModelIndex(), from, from, QModelIndex(), to > from ? to + 1 : to);
    MacroStep step = std::move(m_macro.steps.at(from));
    m_macro.steps.erase(m_macro.steps.begin() + from);
    m_macro.steps.insert(m_macro.steps.begin() + to, std::move(step));
    endMoveRows();

    notifyStepsChanged(std::min(from, to), std::max(from, to));
}

void MacroCommandsModel::remove(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    beginRemoveRows(QModelIndex(), row, row);
    m_macro.steps.erase(m_macro.steps.begin() + row);
    endRemoveRows();

    notifyStepsChanged(row, rowCount() - 1);
    emit isEmptyChanged();
}

void MacroCommandsModel::editParams(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    if (isEffectApplyCommand(m_macro.steps.at(row).command)) {
        configureEffectParams(row);
    } else {
        editSchemaParams(row);
    }
}

bool MacroCommandsModel::save()
{
    return repository()->save(m_macro);
}

void MacroCommandsModel::run()
{
    runner()->run(m_macro);
}

void MacroCommandsModel::runOnFiles()
{
    m_runOnFilesLauncher.launch(m_macro);
}

bool MacroCommandsModel::isRowValid(int row) const
{
    return row >= 0 && row < rowCount();
}

QString MacroCommandsModel::titleOf(const Command& command) const
{
    const CommandInfo& info = commandsRegister()->commandInfo(command);
    if (!info.isValid()) {
        return QString::fromStdString(command.toString());
    }

    return info.title.qTranslatedWithoutMnemonic();
}

bool MacroCommandsModel::hasParams(const Command& command) const
{
    if (isEffectApplyCommand(command)) {
        return true;
    }

    return !commandsRegister()->commandInfo(command).inputSchema.args.empty();
}

void MacroCommandsModel::notifyStepsChanged(int fromRow, int toRow)
{
    if (fromRow > toRow || !isRowValid(fromRow) || !isRowValid(toRow)) {
        return;
    }

    emit dataChanged(index(fromRow), index(toRow), { StepRole });
}

void MacroCommandsModel::setStepParams(int row, const Params& params)
{
    if (!isRowValid(row)) {
        return;
    }

    m_macro.steps[row].params = params;
}

void MacroCommandsModel::configureEffectParams(int row)
{
    const MacroStep& step = m_macro.steps.at(row);

    commandDispatcher()->dispatch(effectConfigureCommand(step.command), step.params)
    .onResolve(this, [this, row](const Response& response) {
        if (!response.ret) {
            return;
        }

        setStepParams(row, response.val<Params>());
    });
}

void MacroCommandsModel::editSchemaParams(int row)
{
    const MacroStep& step = m_macro.steps.at(row);

    UriQuery uri(COMMAND_PARAMS_URI);
    uri.addParam("command", Val(step.command.toString()));
    uri.addParam("params", Val(ValMap(step.params)));

    const RetVal<Val> result = interactive()->openSync(uri);
    if (!result.ret) {
        return;
    }

    Params params;
    for (const auto& [key, val] : result.val.toMap()) {
        params[key] = val;
    }

    setStepParams(row, params);
}
