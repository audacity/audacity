/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <gtest/gtest.h>

#include <cstring>
#include <iostream>

#include "serialization/json.h"

#include <QJsonObject>
#include <QJsonDocument>

using namespace mu;

class Global_Ser_Json : public ::testing::Test
{
public:
};

TEST_F(Global_Ser_Json, WriteRead)
{
//    const char json[] ="{"
//                       "   \"hello\" : \"world\", "
//                       "   \"t\" : true , "
//                       "   \"f\" : false, "
//                       "   \"n\": null, "
//                       "   \"i\":123, "
//                       "   \"pi\": 3.1416, "
//                       "   \"a\":[1, 2, 3, 4], "
//                       "   \"o\":{\"key1\":\"val1\", \"key2\":\"val2\"} "
//                       "} ";

    {
//        QJsonObject o;
//        o["pi"] = 3.1416;
//        QByteArray ba = QJsonDocument(o).toJson();
//        std::cout << ba.constData() << "\n";
    }

    ByteArray data;

    // Write
    {
        JsonObject root;

        root["hello"] = "world";
        root["t"] = true;
        root["f"] = false;
        root["n"] = JsonValue();
        root["i"] = 123;
        root["pi"] = 3.1416;

        JsonArray ar;
        ar.append(1);
        ar.append(2);
        ar.append(3);
        ar.append(4);
        root["a"] = ar;

        JsonObject o;
        o["key1"] = "val1";
        o["key2"] = "val2";
        root["o"] = o;

        data = JsonDocument(root).toJson(JsonDocument::Format::Indented);
    }

    std::cout << data.constChar() << std::endl;

    // Read
    {
        JsonDocument doc = JsonDocument::fromJson(data);

        EXPECT_TRUE(doc.isObject());

        JsonObject root = doc.rootObject();

        EXPECT_FALSE(root.contains("notexists"));

        EXPECT_TRUE(root.contains("hello"));
        EXPECT_TRUE(root.value("hello").isString());
        EXPECT_EQ(root.value("hello").toString(), u"world");

        EXPECT_TRUE(root.contains("t"));
        EXPECT_TRUE(root.value("t").isBool());
        EXPECT_TRUE(root.value("t").toBool());

        EXPECT_TRUE(root.contains("f"));
        EXPECT_TRUE(root.value("f").isBool());
        EXPECT_FALSE(root.value("f").toBool());

        EXPECT_TRUE(root.contains("n"));
        EXPECT_TRUE(root.value("n").isNull());

        EXPECT_TRUE(root.contains("i"));
        EXPECT_TRUE(root.value("i").isNumber());
        EXPECT_EQ(root.value("i").toInt(), 123);

        EXPECT_TRUE(root.contains("pi"));
        EXPECT_TRUE(root.value("pi").isNumber());
        EXPECT_DOUBLE_EQ(root.value("pi").toDouble(), 3.1416);

        EXPECT_TRUE(root.contains("a"));
        EXPECT_TRUE(root.value("a").isArray());
        JsonArray ar = root.value("a").toArray();
        EXPECT_EQ(ar.size(), 4);
        EXPECT_EQ(ar.at(0).toInt(), 1);
        EXPECT_EQ(ar.at(3).toInt(), 4);

        EXPECT_TRUE(root.contains("o"));
        EXPECT_TRUE(root.value("o").isObject());
        JsonObject obj = root.value("o").toObject();
        EXPECT_EQ(obj.size(), 2);
        std::vector<std::string> keys = obj.keys();
        EXPECT_EQ(keys.size(), 2);
        EXPECT_EQ(keys.at(0), "key1");
        EXPECT_EQ(keys.at(1), "key2");
    }
}
