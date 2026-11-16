/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::projectscene {
class IPlayPositionViewController
{
public:
    virtual ~IPlayPositionViewController() = default;

    virtual void playPositionDecrease() = 0;
    virtual void playPositionIncrease() = 0;

    virtual void selectionExtendLeft() = 0;
    virtual void selectionExtendRight() = 0;
    virtual void selectionContractLeft() = 0;
    virtual void selectionContractRight() = 0;

    virtual void cursorToSelectionStart() = 0;
    virtual void cursorToSelectionEnd() = 0;
};
}
