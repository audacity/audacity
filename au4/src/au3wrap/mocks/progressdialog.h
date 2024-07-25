/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "BasicUI.h" // For ProgressResult

using ProgressResult = BasicUI::ProgressResult;

class ProgressDialog : public BasicUI::ProgressDialog
{
public:
    ProgressDialog();

public:
    virtual ~ProgressDialog();

    void Reinit() override;

    void SetDialogTitle(const TranslatableString& title) override;

public:
    ProgressResult Poll(
        unsigned long long numerator, unsigned long long denominator, const TranslatableString& message = {}) override;

    void SetMessage(const TranslatableString& message) override;
};
