/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "framework/global/io/path.h"
#include "framework/global/types/bytearray.h"
#include "framework/global/types/ret.h"

struct sqlite3;
struct sqlite3_stmt;

namespace au::projectrecovery {
using DatabaseValue = std::variant<int64_t, double, std::string, muse::ByteArray>;

class DatabaseRow
{
public:
    explicit DatabaseRow(sqlite3_stmt* statement)
        : m_statement(statement) {}

    int64_t integer(int column) const;
    double real(int column) const;
    std::string text(int column) const;
    muse::ByteArray blob(int column) const;
    bool isNull(int column) const;

private:
    sqlite3_stmt* m_statement = nullptr;
};

class Database
{
public:
    enum class Mode {
        //! No locks and no -wal or -shm files; only for a file that no process writes and that has no -wal frames
        Immutable,
        ReadOnly,
        ReadWrite,
    };

    Database() = default;
    ~Database();

    Database(const Database&) = delete;
    Database& operator=(const Database&) = delete;

    muse::Ret open(const muse::io::path_t& path, Mode mode);
    void close();

    using RowHandler = std::function<void (const DatabaseRow&)>;

    //! Runs one statement with the parameters ?1, ?2, ... and calls onRow for each row
    muse::Ret query(const std::string& sql, const std::vector<DatabaseValue>& parameters = {}, const RowHandler& onRow = {});
    //! Runs statements without parameters
    muse::Ret exec(const std::string& sql);

    bool hasTable(const std::string& name);
    std::optional<int64_t> pragmaInteger(const std::string& name);

private:
    muse::Ret error() const;

    sqlite3* m_db = nullptr;
};
}
