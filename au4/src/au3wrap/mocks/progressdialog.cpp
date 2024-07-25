/*
* Audacity: A Digital Audio Editor
*/

#include "progressdialog.h"

ProgressDialog::ProgressDialog()
{
}

ProgressDialog::~ProgressDialog()
{
}

void ProgressDialog::Reinit()
{
}

void ProgressDialog::SetDialogTitle(const TranslatableString& title)
{
    Q_UNUSED(title);
}

ProgressResult ProgressDialog::Poll(unsigned long long numerator, unsigned long long denominator, const TranslatableString& message)
{
    Q_UNUSED(numerator);
    Q_UNUSED(denominator);
    return ProgressResult::Success;
}

void ProgressDialog::SetMessage(const TranslatableString& message)
{
    Q_UNUSED(message);
}
