/*
* Audacity: A Digital Audio Editor
*/
#include "macrosrepository.h"

#include <algorithm>

#include "framework/global/configreader.h"
#include "framework/global/io/path.h"
#include "framework/global/serialization/json.h"
#include "framework/global/log.h"

#include "macroserialization.h"

using namespace au::macromanager;
using namespace muse;

namespace {
const std::string MACROS_DIR("macros");
const std::string MACRO_FILE_SUFFIX(".json");
const std::string BUILTIN_MACRO_FILES_KEY("builtin_macro_files");

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

    m_config = ConfigReader::read(":/configs/macros.cfg");

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

        for (const Macro& macro : m_macros) {
            if (macro.name == name || std::ranges::find(dependents, macro.name) != dependents.end()) {
                continue;
            }

            if (references(macro, current)) {
                dependents.push_back(macro.name);
                pending.push_back(macro.name);
            }
        }
    }

    return dependents;
}

Ret MacrosRepository::save(const Macro& macro)
{
    if (macro.name.empty()) {
        return make_ret(Ret::Code::BadArgs, "macro name is empty");
    }

    const Ret ret = writeMacroFile(macro, userMacroFilePath(macro.name));
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
    std::vector<std::string> names = dependentMacros(name);
    names.insert(names.begin(), name);

    for (const std::string& macroName : names) {
        if (!fileSystem()->exists(userMacroFilePath(macroName))) {
            return make_ret(Ret::Code::NotSupported, "built-in macros cannot be removed");
        }
    }

    for (const std::string& macroName : names) {
        const Ret ret = fileSystem()->remove(userMacroFilePath(macroName));
        if (!ret) {
            LOGE() << "failed to remove macro " << macroName << ": " << ret.toString();
            return ret;
        }
    }

    reload();
    m_macrosChanged.notify();

    return make_ok();
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

    if (!fileSystem()->exists(userMacroFilePath(oldName))) {
        return make_ret(Ret::Code::NotSupported, "built-in macros cannot be renamed");
    }

    Macro renamed = *existing;
    renamed.name = newName;

    Ret ret = writeMacroFile(renamed, userMacroFilePath(newName));
    if (!ret) {
        return ret;
    }

    ret = fileSystem()->remove(userMacroFilePath(oldName));
    if (!ret) {
        return ret;
    }

    for (const Macro& dependent : m_macros) {
        if (dependent.builtin || dependent.name == oldName || !references(dependent, oldName)) {
            continue;
        }

        Macro updated = dependent;
        for (MacroItem& item : updated.steps) {
            MacroReference* reference = std::get_if<MacroReference>(&item);
            if (reference && reference->name == oldName) {
                reference->name = newName;
            }
        }

        ret = writeMacroFile(updated, userMacroFilePath(updated.name));
        if (!ret) {
            return ret;
        }
    }

    reload();
    m_macrosChanged.notify();

    return make_ok();
}

async::Notification MacrosRepository::macrosChanged() const
{
    return m_macrosChanged;
}

io::path_t MacrosRepository::userMacrosPath() const
{
    return globalConfiguration()->userAppDataPath() + "/" + MACROS_DIR;
}

io::path_t MacrosRepository::userMacroFilePath(const std::string& name) const
{
    return userMacrosPath() + "/" + io::escapeFileName(name) + MACRO_FILE_SUFFIX;
}

io::paths_t MacrosRepository::builtinMacroFilePaths() const
{
    io::paths_t paths;

    const io::path_t builtinDir = globalConfiguration()->appDataPath() + "/" + MACROS_DIR;
    for (const Val& file : m_config.value(BUILTIN_MACRO_FILES_KEY).toList()) {
        paths.emplace_back(builtinDir + "/" + file.toString());
    }

    return paths;
}

void MacrosRepository::reload()
{
    m_macros.clear();

    for (const io::path_t& path : builtinMacroFilePaths()) {
        loadMacroFile(path, true);
    }

    const RetVal<io::paths_t> userFiles = fileSystem()->scanFiles(userMacrosPath(), { "*" + MACRO_FILE_SUFFIX });
    if (userFiles.ret) {
        for (const io::path_t& path : userFiles.val) {
            loadMacroFile(path, false);
        }
    }

    std::ranges::sort(m_macros, {}, &Macro::name);
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

void MacrosRepository::loadMacroFile(const io::path_t& path, bool builtin)
{
    RetVal<Macro> macro = readMacroFile(path);
    if (!macro.ret) {
        LOGW() << "failed to load macro file " << path << ": " << macro.ret.toString();
        return;
    }

    macro.val.builtin = builtin;

    std::erase_if(m_macros, [&macro](const Macro& m) { return m.name == macro.val.name; });
    m_macros.push_back(std::move(macro.val));
}
