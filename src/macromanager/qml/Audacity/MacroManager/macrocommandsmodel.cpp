/*
* Audacity: A Digital Audio Editor
*/
#include "macrocommandsmodel.h"

#include <algorithm>

#include "framework/global/stringutils.h"

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
    : QAbstractListModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this)), m_actions(this), m_runOnFilesLauncher(this)
{
}

QVariant MacroCommandsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || !isRowValid(index.row())) {
        return QVariant();
    }

    const MacroItem& item = m_macro.steps.at(index.row());
    const MacroStep* step = std::get_if<MacroStep>(&item);
    const QString macroName = step ? QString() : QString::fromStdString(std::get<MacroReference>(item).name);

    switch (role) {
    case StepRole: return index.row() + 1;
    case CodeRole: return step ? QString::fromStdString(step->command.toString()) : macroName;
    case TitleRole: return step ? titleOf(step->command) : macroName;
    case HasParamsRole: return step && hasParams(step->command);
    case IsMacroRole: return !step;
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
        { HasParamsRole, "hasParams" },
        { IsMacroRole, "isMacro" }
    };

    return roles;
}

QString MacroCommandsModel::name() const
{
    return QString::fromStdString(m_macro.name);
}

bool MacroCommandsModel::isBuiltin() const
{
    return m_macro.builtin;
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

    emit nameChanged();
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

    if (!hasParams(command)) {
        insertStep(row, MacroStep { command, {} });
        return;
    }

    requestParams(command, {}, [this, row, command](const Params& params) {
        insertStep(row, MacroStep { command, params });
    });
}

void MacroCommandsModel::addMacro(const QString& name)
{
    insertMacro(rowCount(), name);
}

void MacroCommandsModel::insertMacro(int row, const QString& name)
{
    if (name.isEmpty()) {
        return;
    }

    insertStep(row, MacroReference { name.toStdString() });
}

void MacroCommandsModel::move(int from, int to)
{
    to = std::clamp(to, 0, rowCount() - 1);

    if (!isRowValid(from) || from == to) {
        return;
    }

    beginMoveRows(QModelIndex(), from, from, QModelIndex(), to > from ? to + 1 : to);
    MacroItem item = std::move(m_macro.steps.at(from));
    m_macro.steps.erase(m_macro.steps.begin() + from);
    m_macro.steps.insert(m_macro.steps.begin() + to, std::move(item));
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

    const MacroStep* step = std::get_if<MacroStep>(&m_macro.steps.at(row));
    if (!step) {
        return;
    }

    requestParams(step->command, step->params, [this, row](const Params& params) {
        setStepParams(row, params);
    });
}

bool MacroCommandsModel::save()
{
    const Ret ret = repository()->save(m_macro);
    if (!ret) {
        interactive()->error(trc("macromanager", "The macro could not be saved"), ret.text());
        return false;
    }

    return true;
}

void MacroCommandsModel::run()
{
    runner()->run(m_macro);
}

void MacroCommandsModel::runOnFiles()
{
    m_runOnFilesLauncher.launch(m_macro);
}

void MacroCommandsModel::removeAllSteps()
{
    if (m_macro.steps.empty()) {
        return;
    }

    beginResetModel();
    m_macro.steps.clear();
    endResetModel();

    emit isEmptyChanged();
}

void MacroCommandsModel::rename()
{
    const std::optional<std::string> newName = m_actions.rename(m_macro.name);
    if (!newName) {
        return;
    }

    m_macro.name = *newName;
    emit nameChanged();
}

void MacroCommandsModel::exportMacro()
{
    m_actions.exportMacro(m_macro);
}

void MacroCommandsModel::deleteMacro()
{
    if (m_actions.remove(m_macro.name)) {
        emit macroDeleted();
    }
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

void MacroCommandsModel::insertStep(int row, const MacroItem& item)
{
    row = std::clamp(row, 0, rowCount());

    beginInsertRows(QModelIndex(), row, row);
    m_macro.steps.insert(m_macro.steps.begin() + row, item);
    endInsertRows();

    notifyStepsChanged(row + 1, rowCount() - 1);
    emit isEmptyChanged();
}

void MacroCommandsModel::setStepParams(int row, const Params& params)
{
    if (!isRowValid(row)) {
        return;
    }

    if (MacroStep* step = std::get_if<MacroStep>(&m_macro.steps[row])) {
        step->params = params;
    }
}

void MacroCommandsModel::requestParams(const Command& command, const Params& current, const OnParams& onParams)
{
    if (isEffectApplyCommand(command)) {
        requestEffectParams(command, current, onParams);
    } else {
        requestSchemaParams(command, current, onParams);
    }
}

void MacroCommandsModel::requestEffectParams(const Command& command, const Params& current, const OnParams& onParams)
{
    commandDispatcher()->dispatch(effectConfigureCommand(command), current)
    .onResolve(this, [onParams](const Response& response) {
        if (!response.ret) {
            return;
        }

        onParams(response.val<Params>());
    });
}

void MacroCommandsModel::requestSchemaParams(const Command& command, const Params& current, const OnParams& onParams)
{
    UriQuery uri(COMMAND_PARAMS_URI);
    uri.addParam("command", Val(command.toString()));
    uri.addParam("params", Val(ValMap(current)));

    interactive()->open(uri).onResolve(this, [onParams](const Val& result) {
        Params params;
        for (const auto& [key, val] : result.toMap()) {
            params[key] = val;
        }

        onParams(params);
    });
}
