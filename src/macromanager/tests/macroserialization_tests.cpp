/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <limits>

#include "global/serialization/json.h"

#include "macromanager/internal/macroserialization.h"

using namespace muse;
using namespace muse::rcommand;
using namespace au::macromanager;

namespace {
Params roundTrip(const Params& params)
{
    const Macro macro { "macro", { MacroStep { Command("command://test/step"), params } } };

    std::string error;
    const JsonDocument document = JsonDocument::fromJson(JsonDocument(toJson(macro)).toJson(), &error);
    EXPECT_TRUE(error.empty());

    const RetVal<Macro> result = macroFromJson(document.rootObject());
    EXPECT_TRUE(result.ret);
    EXPECT_EQ(result.val.steps.size(), 1);

    return std::get<MacroStep>(result.val.steps.front()).params;
}
}

TEST(MacroManager_MacroSerializationTests, KeepsScalarTypes)
{
    const Params params = roundTrip({
        { "bool", Val(true) },
        { "int", Val(42) },
        { "double", Val(0.5) },
        { "string", Val(std::string("text")) },
    });

    EXPECT_EQ(params.at("bool").type(), Val::Type::Bool);
    EXPECT_EQ(params.at("bool").toBool(), true);
    EXPECT_EQ(params.at("int").type(), Val::Type::Int);
    EXPECT_EQ(params.at("int").toInt(), 42);
    EXPECT_EQ(params.at("double").type(), Val::Type::Double);
    EXPECT_EQ(params.at("double").toDouble(), 0.5);
    EXPECT_EQ(params.at("string").type(), Val::Type::String);
    EXPECT_EQ(params.at("string").toString(), "text");
}

TEST(MacroManager_MacroSerializationTests, KeepsInt64Precision)
{
    const int64_t large = (int64_t(1) << 53) + 1;
    const int64_t negative = std::numeric_limits<int64_t>::min();

    const Params params = roundTrip({
        { "large", Val(large) },
        { "negative", Val(negative) },
        { "list", Val(ValList { Val(large) }) },
    });

    EXPECT_EQ(params.at("large").type(), Val::Type::Int64);
    EXPECT_EQ(params.at("large").toInt64(), large);
    EXPECT_EQ(params.at("negative").type(), Val::Type::Int64);
    EXPECT_EQ(params.at("negative").toInt64(), negative);
    ASSERT_EQ(params.at("list").type(), Val::Type::List);
    EXPECT_EQ(params.at("list").toList().at(0).type(), Val::Type::Int64);
    EXPECT_EQ(params.at("list").toList().at(0).toInt64(), large);
}

TEST(MacroManager_MacroSerializationTests, KeepsMapsThatLookLikeInt64)
{
    const Params params = roundTrip({
        { "map", Val(ValMap { { "int64", Val(std::string("not a number")) } }) },
    });

    ASSERT_EQ(params.at("map").type(), Val::Type::Map);
    EXPECT_EQ(params.at("map").toMap().at("int64").toString(), "not a number");
}

TEST(MacroManager_MacroSerializationTests, KeepsNestedListsAndMaps)
{
    const ValMap map { { "count", Val(3) }, { "name", Val(std::string("a")) } };
    const ValList list { Val(1), Val(std::string("b")), Val(map) };

    const Params params = roundTrip({ { "list", Val(list) } });

    ASSERT_EQ(params.at("list").type(), Val::Type::List);
    const ValList restored = params.at("list").toList();
    ASSERT_EQ(restored.size(), 3);
    EXPECT_EQ(restored.at(0).toInt(), 1);
    EXPECT_EQ(restored.at(1).toString(), "b");
    ASSERT_EQ(restored.at(2).type(), Val::Type::Map);
    EXPECT_EQ(restored.at(2).toMap().at("count").toInt(), 3);
    EXPECT_EQ(restored.at(2).toMap().at("name").toString(), "a");
}
