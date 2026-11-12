/*
 * Audacity: A Digital Audio Editor
 */
#include "database.h"

#include <sqlite3.h>

#include <cstdio>

#include "au3-sqlite-helpers/sqlite/SQLiteUtils.h"

using namespace au::projectrecovery;

namespace {
//! A file: URI; SQLite needs one for the immutable parameter
std::string fileUri(const muse::io::path_t& path)
{
    const std::string& utf8 = path.toStdString();
    // SQLite reads "file:C:/x" as a relative path
    std::string uri = utf8.size() > 1 && utf8[1] == ':' ? "file:/" : "file:";
    for (const char c : utf8) {
        const unsigned char byte = static_cast<unsigned char>(c);
        const bool unreserved = (byte >= 'A' && byte <= 'Z') || (byte >= 'a' && byte <= 'z') || (byte >= '0' && byte <= '9')
                                || byte == '/' || byte == '-' || byte == '_' || byte == '.' || byte == '~' || byte == ':';
        if (unreserved) {
            uri += c;
        } else {
            char hex[4];
            std::snprintf(hex, sizeof(hex), "%%%02X", byte);
            uri += hex;
        }
    }
    return uri;
}

void bindParameter(sqlite3_stmt* statement, int index, const DatabaseValue& value)
{
    if (const auto* integer = std::get_if<int64_t>(&value)) {
        sqlite3_bind_int64(statement, index, *integer);
    } else if (const auto* real = std::get_if<double>(&value)) {
        sqlite3_bind_double(statement, index, *real);
    } else if (const auto* text = std::get_if<std::string>(&value)) {
        sqlite3_bind_text(statement, index, text->data(), static_cast<int>(text->size()), SQLITE_TRANSIENT);
    } else {
        const auto& blob = std::get<muse::ByteArray>(value);
        sqlite3_bind_blob(statement, index, blob.constData(), static_cast<int>(blob.size()), SQLITE_TRANSIENT);
    }
}
}

int64_t DatabaseRow::integer(int column) const
{
    return sqlite3_column_int64(m_statement, column);
}

double DatabaseRow::real(int column) const
{
    return sqlite3_column_double(m_statement, column);
}

std::string DatabaseRow::text(int column) const
{
    const auto* text = sqlite3_column_text(m_statement, column);
    if (!text) {
        return {};
    }
    return std::string(reinterpret_cast<const char*>(text), static_cast<size_t>(sqlite3_column_bytes(m_statement, column)));
}

muse::ByteArray DatabaseRow::blob(int column) const
{
    const auto* data = static_cast<const uint8_t*>(sqlite3_column_blob(m_statement, column));
    if (!data) {
        return {};
    }
    return muse::ByteArray(data, static_cast<size_t>(sqlite3_column_bytes(m_statement, column)));
}

bool DatabaseRow::isNull(int column) const
{
    return sqlite3_column_type(m_statement, column) == SQLITE_NULL;
}

Database::~Database()
{
    close();
}

muse::Ret Database::open(const muse::io::path_t& path, Mode mode)
{
    close();
    if (audacity::sqlite::Initialize().IsError()) {
        return muse::make_ret(muse::Ret::Code::InternalError, "cannot initialize SQLite");
    }
    std::string uri = fileUri(path);
    if (mode == Mode::Immutable) {
        uri += "?immutable=1";
    }
    const int flags = (mode == Mode::ReadWrite ? SQLITE_OPEN_READWRITE : SQLITE_OPEN_READONLY) | SQLITE_OPEN_URI;
    const int rc = sqlite3_open_v2(uri.c_str(), &m_db, flags, nullptr);
    if (rc != SQLITE_OK) {
        const std::string text = m_db ? sqlite3_errmsg(m_db) : sqlite3_errstr(rc);
        close();
        return muse::make_ret(muse::Ret::Code::InternalError, "cannot open the database: " + text);
    }
    return muse::make_ok();
}

void Database::close()
{
    if (m_db) {
        sqlite3_close_v2(m_db);
        m_db = nullptr;
    }
}

muse::Ret Database::error() const
{
    return muse::make_ret(muse::Ret::Code::InternalError, std::string(sqlite3_errmsg(m_db)));
}

muse::Ret Database::query(const std::string& sql, const std::vector<DatabaseValue>& parameters, const RowHandler& onRow)
{
    sqlite3_stmt* statement = nullptr;
    if (sqlite3_prepare_v2(m_db, sql.c_str(), -1, &statement, nullptr) != SQLITE_OK) {
        return error();
    }
    for (size_t i = 0; i < parameters.size(); ++i) {
        bindParameter(statement, static_cast<int>(i + 1), parameters[i]);
    }
    int rc = SQLITE_ROW;
    while ((rc = sqlite3_step(statement)) == SQLITE_ROW) {
        if (onRow) {
            onRow(DatabaseRow(statement));
        }
    }
    sqlite3_finalize(statement);
    if (rc != SQLITE_DONE) {
        return error();
    }
    return muse::make_ok();
}

muse::Ret Database::exec(const std::string& sql)
{
    char* message = nullptr;
    if (sqlite3_exec(m_db, sql.c_str(), nullptr, nullptr, &message) != SQLITE_OK) {
        muse::Ret ret = muse::make_ret(muse::Ret::Code::InternalError, std::string(message ? message : sqlite3_errmsg(m_db)));
        sqlite3_free(message);
        return ret;
    }
    return muse::make_ok();
}

bool Database::hasTable(const std::string& name)
{
    bool found = false;
    query("SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?1;", { name }, [&found](const DatabaseRow&) {
        found = true;
    });
    return found;
}

std::optional<int64_t> Database::pragmaInteger(const std::string& name)
{
    std::optional<int64_t> value;
    const muse::Ret ret = query("PRAGMA " + name + ";", {}, [&value](const DatabaseRow& row) {
        value = row.integer(0);
    });
    if (!ret) {
        return std::nullopt;
    }
    return value;
}
