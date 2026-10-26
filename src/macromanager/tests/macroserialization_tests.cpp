/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

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
