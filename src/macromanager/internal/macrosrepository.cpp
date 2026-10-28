/*
* Audacity: A Digital Audio Editor
*/
#include "macrosrepository.h"

#include <algorithm>
#include <iterator>
#include <map>
#include <ranges>

#include "framework/global/io/path.h"
#include "framework/global/serialization/json.h"
#include "framework/global/translation.h"
#include "framework/global/log.h"

#include "macroserialization.h"

using namespace au::macromanager;
using namespace muse;

namespace {
const std::string MACROS_DIR("macros");
const std::string MACRO_FILE_SUFFIX(".json");

bool references(const Macro& macro, const std::string& name)
{
    return std::ranges::any_of(macro.steps, [&name](const MacroItem& item) {
        const MacroReference* reference = std::get_if<MacroReference>(&item);
        return reference && reference->name == name;
    });
}
}

void MacrosRepository::init()
{
    fileSystem()->makePath(userMacrosPath());

    reload();
}

const MacroList& MacrosRepository::macros() const
{
    return m_macros;
}

std::optional<Macro> MacrosRepository::macro(const std::string& name) const
{
    const auto it = std::ranges::find(m_macros, name, &Macro::name);
    if (it == m_macros.end()) {
        return std::nullopt;
    }

    return *it;
}

std::vector<std::string> MacrosRepository::dependentMacros(const std::string& name) const
{
    std::vector<std::string> dependents;
    std::vector<std::string> pending { name };

    while (!pending.empty()) {
        const std::string current = std::move(pending.back());
        pending.pop_back();

        for (std::string& referencing : referencingMacros(current)) {
            if (referencing == name || std::ranges::find(dependents, referencing) != dependents.end()) {
                continue;
            }

            pending.push_back(referencing);
            dependents.push_back(std::move(referencing));
        }
    }

    return dependents;
}

std::vector<std::string> MacrosRepository::referencingMacros(const std::string& name) const
{
    auto referencing = m_macros
                       | std::views::filter([&name](const Macro& macro) { return macro.name != name && references(macro, name); })
                       | std::views::transform(&Macro::name);
    return { referencing.begin(), referencing.end() };
}

Ret MacrosRepository::validateReferences(const Macro& macro) const
{
    std::vector<std::string> path { macro.name };
    return visitReferences(macro, path);
}

Ret MacrosRepository::visitReferences(const Macro& macro, std::vector<std::string>& path) const
{
    for (const MacroItem& item : macro.steps) {
        const MacroReference* reference = std::get_if<MacroReference>(&item);
        if (!reference) {
            continue;
        }

        if (std::ranges::find(path, reference->name) != path.end()) {
            const QString root = QString::fromStdString(path.front());
            const QString message = path.size() == 1
                                    ? qtrc("macromanager", "Macro “%1” calls itself").arg(root)
                                    : qtrc("macromanager", "Macro “%1” calls itself through “%2”").arg(root,
                                                                                                       QString::fromStdString(path.back()));
            return make_ret(Ret::Code::BadArgs, message.toStdString());
        }

        const std::optional<Macro> referenced = this->macro(reference->name);
        if (!referenced) {
            continue;
        }

        path.push_back(reference->name);
        const Ret ret = visitReferences(*referenced, path);
        path.pop_back();

        if (!ret) {
            return ret;
        }
    }

    return make_ok();
}

Ret MacrosRepository::save(const Macro& macro)
{
    if (macro.name.empty()) {
        return make_ret(Ret::Code::BadArgs, "macro name is empty");
    }

    const Ret valid = validateReferences(macro);
    if (!valid) {
        return valid;
    }

    const io::path_t path = userMacroFilePath(macro.name).value_or(newUserMacroFilePath(macro.name));
    const Ret ret = writeMacroFile(macro, path);
    if (!ret) {
        LOGE() << "failed to save macro " << macro.name << ": " << ret.toString();
        return ret;
    }

    reload();
    m_macrosChanged.notify();

    return make_ok();
}

Ret MacrosRepository::remove(const std::string& name)
{
    if (!macro(name)) {
        return make_ret(Ret::Code::BadArgs, "macro not found");
    }

    Ret ret = fileSystem()->remove(m_userMacroFiles.at(name));
    if (!ret) {
        LOGE() << "failed to remove macro " << name << ": " << ret.toString();
    }

    for (const std::string& referencingName : referencingMacros(name)) {
        if (!ret) {
            break;
        }

        Macro updated = *macro(referencingName);
        std::erase_if(updated.steps, [&name](const MacroItem& item) {
            const MacroReference* reference = std::get_if<MacroReference>(&item);
            return reference && reference->name == name;
        });

        ret = writeMacroFile(updated, m_userMacroFiles.at(referencingName));
    }

    reload();
    m_macrosChanged.notify();

    return ret;
}

Ret MacrosRepository::rename(const std::string& oldName, const std::string& newName)
{
    if (oldName == newName) {
        return make_ok();
    }

    if (macro(newName)) {
        return make_ret(Ret::Code::BadArgs, "a macro with this name already exists");
    }

    const std::optional<Macro> existing = macro(oldName);
    if (!existing) {
        return make_ret(Ret::Code::BadArgs, "macro not found");
    }

    Macro renamed = *existing;
    renamed.name = newName;

    Ret ret = writeMacroFile(renamed, m_userMacroFiles.at(oldName));

    for (const Macro& dependent : m_macros) {
        if (!ret) {
            break;
        }

        if (dependent.name == oldName || !references(dependent, oldName)) {
            continue;
        }

        Macro updated = dependent;
        for (MacroItem& item : updated.steps) {
            MacroReference* reference = std::get_if<MacroReference>(&item);
            if (reference && reference->name == oldName) {
                reference->name = newName;
            }
        }

        ret = writeMacroFile(updated, m_userMacroFiles.at(updated.name));
    }

    reload();
    m_macrosChanged.notify();

    return ret;
}

async::Notification MacrosRepository::macrosChanged() const
{
    return m_macrosChanged;
}

io::path_t MacrosRepository::userMacrosPath() const
{
    return globalConfiguration()->userAppDataPath() + "/" + MACROS_DIR;
}

std::optional<io::path_t> MacrosRepository::userMacroFilePath(const std::string& name) const
{
    const auto it = m_userMacroFiles.find(name);
    if (it == m_userMacroFiles.end()) {
        return std::nullopt;
    }

    return it->second;
}

io::path_t MacrosRepository::newUserMacroFilePath(const std::string& name) const
{
    const io::path_t baseName = userMacrosPath() + "/" + io::escapeFileName(name);

    io::path_t path = baseName + MACRO_FILE_SUFFIX;
    for (int suffix = 2; fileSystem()->exists(path); ++suffix) {
        path = baseName + "_" + std::to_string(suffix) + MACRO_FILE_SUFFIX;
    }

    return path;
}

void MacrosRepository::reload()
{
    std::map<std::string, Macro> macrosByName;
    m_userMacroFiles.clear();

    const RetVal<io::paths_t> userFiles = fileSystem()->scanFiles(userMacrosPath(), { "*" + MACRO_FILE_SUFFIX });
    if (userFiles.ret) {
        for (const io::path_t& path : userFiles.val) {
            RetVal<Macro> macro = readMacroFile(path);
            if (!macro.ret) {
                LOGW() << "failed to load macro file " << path << ": " << macro.ret.toString();
                continue;
            }

            m_userMacroFiles.insert_or_assign(macro.val.name, path);
            macrosByName.insert_or_assign(macro.val.name, std::move(macro.val));
        }
    }

    m_macros.clear();
    std::ranges::move(macrosByName | std::views::values, std::back_inserter(m_macros));
}

RetVal<Macro> MacrosRepository::readMacroFile(const io::path_t& path) const
{
    const RetVal<ByteArray> data = fileSystem()->readFile(path);
    if (!data.ret) {
        return RetVal<Macro>(data.ret);
    }

    std::string error;
    const JsonDocument document = JsonDocument::fromJson(data.val, &error);
    if (!error.empty() || !document.isObject()) {
        return RetVal<Macro>(make_ret(Ret::Code::UnknownError, "invalid macro file: " + error));
    }

    return macroFromJson(document.rootObject());
}

Ret MacrosRepository::writeMacroFile(const Macro& macro, const io::path_t& path) const
{
    const JsonDocument document(toJson(macro));
    return fileSystem()->writeFile(path, document.toJson());
}
