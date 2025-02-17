/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: SQliteHelpersTests.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include <catch2/catch.hpp>

#include "sqlite/Connection.h"

TEST_CASE("SQLiteHelpers", "")
{
    using namespace audacity::sqlite;

    auto connection = Connection::Open(":memory:", OpenMode::Memory);
    REQUIRE(connection);
    REQUIRE(connection->CheckTableExists("test") == false);
    REQUIRE(!!connection->Execute(
                "CREATE TABLE test (id INTEGER PRIMARY KEY, name TEXT);"));
    REQUIRE(connection->CheckTableExists("test") == true);
    REQUIRE(!!connection->Execute("INSERT INTO test (name) VALUES ('test1');"));

    {
        auto stmt
            =connection->CreateStatement("INSERT INTO test (name) VALUES (?);");

        REQUIRE(stmt);
        auto& ctx = stmt->Prepare();
        ctx.Bind(1, "test2");
        auto run1 = ctx.Run();
        REQUIRE(run1.IsOk());
        REQUIRE(run1.HasRows() == false);

        ctx.Bind(1, "test3");
        auto run2 = ctx.Run();
        REQUIRE(run2.IsOk());
        REQUIRE(run2.HasRows() == false);
    }

    {
        auto stmt = connection->CreateStatement("SELECT * FROM test;");

        REQUIRE(stmt);
        auto run = stmt->Prepare().Run();

        REQUIRE(run.IsOk());
        REQUIRE(run.HasRows() == true);

        int rowId = 1;

        for (auto row : run) {
            int id;
            std::string name;

            REQUIRE(row.Get(0, id));
            REQUIRE(row.Get(1, name));
            REQUIRE(!row.Get(2, id));

            REQUIRE(rowId == id);
            REQUIRE(name == "test" + std::to_string(rowId));

            ++rowId;
        }

        REQUIRE(rowId == 4);
    }
}
