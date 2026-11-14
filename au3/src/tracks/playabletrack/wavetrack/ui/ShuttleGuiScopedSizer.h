#pragma once

#include "ShuttleGui.h"

template<typename ReturnType, typename ... Args> class ScopedSizer
{
public:
    ScopedSizer(
        ShuttleGui& s, ReturnType(ShuttleGui::* startFunc)(Args...),
        void(ShuttleGui::* endFunc)(), Args... args)
        : mShuttleGui(s)
        , mStartFunction(startFunc)
        , mEndFunction(endFunc)
    {
        (mShuttleGui.*mStartFunction)(std::forward<Args>(args)...);
    }

    ~ScopedSizer()
    {
        (mShuttleGui.*mEndFunction)();
    }

private:
    ShuttleGui& mShuttleGui;
    ReturnType (ShuttleGui::*mStartFunction)(Args...);
    void (ShuttleGui::*mEndFunction)();
};

class ScopedHorizontalLay : public ScopedSizer<void, int, int>
{
public:
    ScopedHorizontalLay(
        ShuttleGui& s, int PositionFlags = wxALIGN_CENTRE, int iProp = 1)
        : ScopedSizer<void, int, int>(
            s, &ShuttleGui::StartHorizontalLay, &ShuttleGui::EndHorizontalLay,
            PositionFlags, iProp)
    {
    }
};

class ScopedInvisiblePanel : public ScopedSizer<wxPanel*, int>
{
public:
    ScopedInvisiblePanel(ShuttleGui& s, int border = 0)
        : ScopedSizer<wxPanel*, int>(
            s, &ShuttleGui::StartInvisiblePanel, &ShuttleGui::EndInvisiblePanel,
            border)
    {
    }
};

class ScopedVerticalLay : public ScopedSizer<void, int>
{
public:
    ScopedVerticalLay(ShuttleGui& s, int iProp = 1)
        : ScopedSizer<void, int>(
            s, &ShuttleGui::StartVerticalLay, &ShuttleGui::EndVerticalLay,
            iProp)
    {
    }
};

class ScopedStatic : public ScopedSizer<wxStaticBox*, const TranslatableString&, int>
{
public:
    ScopedStatic(ShuttleGui& s, const TranslatableString& label, int iProp = 0)
        : ScopedSizer<wxStaticBox*, const TranslatableString&, int>(
            s, &ShuttleGui::StartStatic, &ShuttleGui::EndStatic, label, iProp)
    {
    }
};
