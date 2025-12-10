/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QColor>

#include <unordered_set>
#include <vector>

class SpectrogramColorRegister final
{
public:
    using NameSet = std::unordered_set<std::string>;

    void EnsureInitialised();
    void RegisterColour(NameSet& allNames, int& iIndex, const QColor& Clr, const std::string& Name);

    QColor& Colour(int iIndex);
    void RegisterColours();

private:
    std::vector<QColor> mColours;
    bool mColorsInitialized = false;
};

extern SpectrogramColorRegister spectrogramColorRegister;
