/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioSegmentSampleViewTest.cpp

  Vitaly Sverchinsky

**********************************************************************/

#include <catch2/catch.hpp>
#include "BasicSettings.h"

bool IsSame(const wxArrayString& arr, std::initializer_list<wxString> list, bool compareWithCase = true)
{
    if (arr.size() != list.size()) {
        return false;
    }
    for (auto& str : list) {
        if (std::find_if(
                arr.begin(),
                arr.end(),
                [&](const auto& value) { return str.IsSameAs(value, compareWithCase); }
                ) == arr.end()) {
            return false;
        }
    }
    return true;
}

struct Stringifyable final
{
    long value;

    bool operator ==(const Stringifyable& other) const
    {
        return value == other.value;
    }
};

wxString wxToString(const Stringifyable& obj)
{
    return wxString::Format("%ld", obj.value);
}

bool wxFromString(const wxString& str, Stringifyable* obj)
{
    if (str.ToLong(&obj->value)) {
        return true;
    }
    return false;
}

void TestRW(audacity::BasicSettings& settings)
{
    bool orangeValue;
    REQUIRE(settings.Write("orange", true));
    REQUIRE(settings.Read("orange", &orangeValue));
    REQUIRE(orangeValue == true);

    int appleValue;
    REQUIRE(settings.Write("apple", 3));
    REQUIRE(settings.Read("apple", &appleValue));
    REQUIRE(appleValue == 3);

    long long plumValue;
    REQUIRE(settings.Write("plum", std::numeric_limits<long long>::max()));
    REQUIRE(settings.Read("plum", &plumValue));
    REQUIRE(plumValue == std::numeric_limits<long long>::max());

    double pearValue;
    REQUIRE(settings.Write("pear", std::numeric_limits<double>::epsilon()));
    REQUIRE(settings.Read("pear", &pearValue));
    REQUIRE(std::abs(pearValue - std::numeric_limits<double>::epsilon()) <= 0.001);

    wxString pineappleValue;
    REQUIRE(settings.Write("pineapple", "Lorem Ipsum"));
    REQUIRE(settings.Read("pineapple", &pineappleValue));
    REQUIRE(pineappleValue == "Lorem Ipsum");

    Stringifyable mangoValue{ 0 };
    REQUIRE(settings.Write("mango", Stringifyable { 1431655765 }));
    REQUIRE(settings.Read("mango", &mangoValue));
    REQUIRE(mangoValue.value == 1431655765);

    REQUIRE(settings.Read("orange", &orangeValue, false));
    REQUIRE(orangeValue == true);
    REQUIRE(settings.Read("apple", &appleValue, 1000));
    REQUIRE(appleValue == 3);
    REQUIRE(settings.Read("plum", &plumValue, 2000LL));
    REQUIRE(plumValue == std::numeric_limits<long long>::max());
    REQUIRE(settings.Read("pear", &pearValue, 3000.0));
    REQUIRE(std::abs(pearValue - std::numeric_limits<double>::epsilon()) <= 0.001);
    REQUIRE(settings.Read("pineapple", &pineappleValue, { }));
    REQUIRE(pineappleValue == "Lorem Ipsum");

    bool orange1;
    REQUIRE(!settings.Read("orange1", &orange1, orangeValue));
    REQUIRE(orange1 == orangeValue);
    int apple1;
    REQUIRE(!settings.Read("apple1", &apple1, appleValue));
    REQUIRE(apple1 == appleValue);
    long long plum1;
    REQUIRE(!settings.Read("plum1", &plum1, plumValue));
    REQUIRE(plum1 == plumValue);
    double pear1;
    REQUIRE(!settings.Read("pear1", &pear1, pearValue));
    REQUIRE(std::abs(pearValue - pear1) <= 0.001);
    wxString pineapple1;
    REQUIRE(!settings.Read("pineapple1", &pineapple1, pineappleValue));
    REQUIRE(pineapple1 == pineappleValue);
    Stringifyable mango1;
    REQUIRE(!settings.Read("mango1", &mango1, mangoValue));
    REQUIRE(mango1 == mangoValue);

    REQUIRE(settings.Read("pineapple") == pineappleValue);
    REQUIRE(settings.Read("pineapp").IsEmpty());
    REQUIRE(settings.Read("pineapp", "not found") == "not found");
    REQUIRE(settings.Read("pineapp", L"not found") == L"not found");

    REQUIRE(settings.Read("orange", false) == orangeValue);
    REQUIRE(settings.Read("apple", 0) == appleValue);
    REQUIRE(settings.Read("plum", 0LL) == plumValue);
    REQUIRE(settings.Read("pear", 0.0) == pearValue);
    REQUIRE(settings.Read("mango", Stringifyable { -1 }) == mangoValue);

    REQUIRE(settings.Write("moon", "full"));
    REQUIRE(settings.Read("moon") == "full");

    REQUIRE(settings.Write("star", L"yellow"));
    REQUIRE(settings.Read("star") == L"yellow");
}

void TestGroups(audacity::BasicSettings& settings)
{
    settings.Clear();
    REQUIRE(settings.GetGroup().empty());
    REQUIRE(settings.GetChildKeys().empty());
    REQUIRE(settings.GetChildGroups().empty());

    settings.Write("apple", 10);
    settings.Write("orange", true);

    REQUIRE(settings.HasEntry("apple"));

    REQUIRE(IsSame(settings.GetChildKeys(), { "orange", "apple" }));

    {
        //Create child group, and make sure path has switched to that group
        auto group = settings.BeginGroup("group");
        REQUIRE(settings.GetGroup() == "group");
        REQUIRE(settings.GetChildKeys().empty());
        REQUIRE(settings.GetChildGroups().empty());
    }
    //Exit the scope and restore previous path
    REQUIRE(settings.GetGroup().empty());
    REQUIRE(IsSame(settings.GetChildGroups(), { "group" }));
    REQUIRE(settings.HasGroup("group"));

    //Values could be read from/written to the child group without
    //entering them explicitly
    REQUIRE(settings.Write("path/to/variable", 7));
    REQUIRE(IsSame(settings.GetChildGroups(), { "group", "path" }));
    REQUIRE(settings.HasEntry("path/to/variable"));
    REQUIRE(settings.HasGroup("path"));
    REQUIRE(settings.HasGroup("path/to"));
    REQUIRE(!settings.HasGroup("path/to/variable"));
    REQUIRE(settings.Read("path/to/variable", 0) == 7);

    {
        auto group = settings.BeginGroup("group");
        {
            auto subgroup = settings.BeginGroup("subgroup");
            REQUIRE(settings.GetGroup() == "group/subgroup");
            REQUIRE(settings.Write("number", 4));
        }
        REQUIRE(settings.GetGroup() == "group");
        REQUIRE(IsSame(settings.GetChildGroups(), { "subgroup" }));

        REQUIRE(settings.Write("parrot/color", "red"));
        REQUIRE(IsSame(settings.GetChildGroups(), { "subgroup", "parrot" }));

        //Remove one of two subgroups
        REQUIRE(settings.Remove("subgroup"));
        REQUIRE(IsSame(settings.GetChildGroups(), { "parrot" }));

        REQUIRE(settings.Write("guess", 42));
        REQUIRE(IsSame(settings.GetChildKeys(), { "guess" }));
        //Clear all child groups and values within the current group
        REQUIRE(settings.Remove({}));
        REQUIRE(settings.GetGroup() == "group");
        REQUIRE(settings.GetChildGroups().empty());
        REQUIRE(settings.GetChildKeys().empty());
    }
    REQUIRE(IsSame(settings.GetChildGroups(), { "group", "path" }));
    REQUIRE(settings.GetGroup().IsEmpty());

    //Using absolute path's
    {
        auto group1 = settings.BeginGroup("group1");
        REQUIRE(settings.GetGroup() == "group1");

        auto group2 = settings.BeginGroup("group2");
        REQUIRE(settings.GetGroup() == "group1/group2");

        {
            auto group3 = settings.BeginGroup("/group3");
            REQUIRE(settings.GetGroup() == "group3");

            REQUIRE(settings.HasGroup("/group1/group2"));
            REQUIRE(!settings.HasGroup("/group1/group3"));

            REQUIRE(settings.Write("/group1/value", 1));
            REQUIRE(settings.GetGroup() == "group3");
            REQUIRE(settings.Read("/group1/value", 0) == 1);
            REQUIRE(settings.HasEntry("/group1/value"));

            REQUIRE(settings.Exists("/group1/value"));
            REQUIRE(settings.Exists("/group1"));
        }
        REQUIRE(settings.GetGroup() == "group1/group2");
    }

    settings.Clear();
    REQUIRE(settings.GetChildKeys().empty());
    REQUIRE(settings.GetChildGroups().empty());
}
