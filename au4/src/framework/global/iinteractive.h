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
#ifndef MU_GLOBAL_IINTERACTIVE_H
#define MU_GLOBAL_IINTERACTIVE_H

#include "modularity/imoduleinterface.h"
#include "io/path.h"
#include "types/val.h"
#include "types/retval.h"
#include "types/uri.h"
#include "types/flags.h"
#include "progress.h"

namespace mu {
class IInteractive : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IInteractive)

public:
    virtual ~IInteractive() = default;

    // question
    enum class Button {
        NoButton,
        Ok,
        Continue,
        RestoreDefaults,
        Reset,
        Apply,
        Help,
        Discard,
        Cancel,
        Close,
        Ignore,
        Retry,
        Abort,
        NoToAll,
        No,
        YesToAll,
        Yes,
        Open,
        DontSave,
        SaveAll,
        Save,
        Next,
        Back,
        Select,
        Clear,
        Done,

        CustomButton,
    };
    using Buttons = std::vector<Button>;

    enum ButtonRole { // Keep updated with ButtonRole in buttonboxmodel.h
        AcceptRole,
        RejectRole,
        DestructiveRole,
        ResetRole,
        ApplyRole,
        RetryRole,
        HelpRole,
        ContinueRole,
        BackRole,
        CustomRole
    };

    struct ButtonData {
        int btn = int(Button::CustomButton);
        std::string text;
        bool accent = false;
        bool leftSide = false;
        ButtonRole role = ButtonRole::CustomRole;

        ButtonData(int btn, const std::string& text)
            : btn(btn), text(text) {}
        ButtonData(Button btn, const std::string& text)
            : btn(int(btn)), text(text) {}
        ButtonData(int btn, const std::string& text, bool accent, bool leftSide = false, ButtonRole role = ButtonRole::CustomRole)
            : btn(btn), text(text), accent(accent), leftSide(leftSide), role(role) {}
        ButtonData(Button btn, const std::string& text, bool accent, bool leftSide = false, ButtonRole role = ButtonRole::CustomRole)
            : btn(int(btn)), text(text), accent(accent), leftSide(leftSide), role(role) {}
    };
    using ButtonDatas = std::vector<ButtonData>;

    enum class TextFormat {
        Auto = 0,
        PlainText,
        RichText
    };

    struct Text {
        std::string text;
        TextFormat format = TextFormat::Auto;

        Text() = default;
        Text(const char* t)
            : text(t), format(TextFormat::Auto) {}
        Text(const std::string& t, const TextFormat& f = TextFormat::Auto)
            : text(t), format(f) {}
    };

    struct Result
    {
        Result() = default;
        Result(const int& button)
            : m_button(button) {}
        Result(const int& button, bool showAgain)
            : m_button(button), m_showAgain(showAgain) {}

        Button standardButton() const { return static_cast<Button>(m_button); }
        int button() const { return m_button; }

        bool showAgain() const { return m_showAgain; }

    private:
        int m_button = int(Button::NoButton);
        bool m_showAgain = true;
    };

    enum Option {
        NoOptions = 0x0,
        WithIcon = 0x1,
        WithDontShowAgainCheckBox = 0x2
    };
    DECLARE_FLAGS(Options, Option)

    virtual Result question(const std::string& title, const std::string& text, const Buttons& buttons, const Button& def = Button::NoButton,
                            const Options& options = {}) const = 0;

    virtual Result question(const std::string& title, const Text& text, const ButtonDatas& buttons, int defBtn = int(Button::NoButton),
                            const Options& options = {}) const = 0;

    virtual ButtonData buttonData(Button b) const = 0;

    // info
    virtual Result info(const std::string& title, const std::string& text, const Buttons& buttons = {}, int defBtn = int(Button::NoButton),
                        const Options& options = {}) const = 0;

    virtual Result info(const std::string& title, const Text& text, const ButtonDatas& buttons = {}, int defBtn = int(Button::NoButton),
                        const Options& options = {}) const = 0;

    // warning
    virtual Result warning(const std::string& title, const std::string& text, const Buttons& buttons = {},
                           const Button& def = Button::NoButton, const Options& options = { WithIcon }) const = 0;

    virtual Result warning(const std::string& title, const Text& text, const ButtonDatas& buttons = {}, int defBtn = int(Button::NoButton),
                           const Options& options = { WithIcon }) const = 0;

    virtual Result warning(const std::string& title, const Text& text, const std::string& detailedText, const ButtonDatas& buttons = {},
                           int defBtn = int(Button::NoButton), const Options& options = { WithIcon }) const = 0;

    // error
    virtual Result error(const std::string& title, const std::string& text, const Buttons& buttons = {},
                         const Button& def = Button::NoButton, const Options& options = { WithIcon }) const = 0;

    virtual Result error(const std::string& title, const Text& text, const ButtonDatas& buttons = {}, int defBtn = int(Button::NoButton),
                         const Options& options = { WithIcon }) const = 0;

    virtual Result error(const std::string& title, const Text& text, const std::string& detailedText, const ButtonDatas& buttons = {},
                         int defBtn = int(Button::NoButton), const Options& options = { WithIcon }) const = 0;

    // progress
    virtual Ret showProgress(const std::string& title, mu::Progress* progress) const = 0;

    // files
    virtual io::path_t selectOpeningFile(const QString& title, const io::path_t& dir, const std::vector<std::string>& filter) = 0;
    virtual io::path_t selectSavingFile(const QString& title, const io::path_t& path, const std::vector<std::string>& filter,
                                        bool confirmOverwrite = true) = 0;

    // dirs
    virtual io::path_t selectDirectory(const QString& title, const io::path_t& dir) = 0;
    virtual io::paths_t selectMultipleDirectories(const QString& title, const io::path_t& dir, const io::paths_t& selectedDirectories) = 0;

    // color
    virtual QColor selectColor(const QColor& color = Qt::white, const QString& title = "") = 0;

    // custom
    virtual RetVal<Val> open(const std::string& uri) const = 0;
    virtual RetVal<Val> open(const Uri& uri) const = 0;
    virtual RetVal<Val> open(const UriQuery& uri) const = 0;
    virtual RetVal<bool> isOpened(const std::string& uri) const = 0;
    virtual RetVal<bool> isOpened(const Uri& uri) const = 0;
    virtual RetVal<bool> isOpened(const UriQuery& uri) const = 0;
    virtual async::Channel<Uri> opened() const = 0;

    virtual void raise(const UriQuery& uri) = 0;

    virtual void close(const std::string& uri) = 0;
    virtual void close(const Uri& uri) = 0;
    virtual void close(const UriQuery& uri) = 0;
    virtual void closeAllDialogs() = 0;

    virtual ValCh<Uri> currentUri() const = 0;
    virtual std::vector<Uri> stack() const = 0;

    virtual Ret openUrl(const std::string& url) const = 0;
    virtual Ret openUrl(const QUrl& url) const = 0;

    /// Opens a file browser at the parent directory of filePath,
    /// and selects the file at filePath on OSs that support it
    virtual Ret revealInFileBrowser(const io::path_t& filePath) const = 0;
};
DECLARE_OPERATORS_FOR_FLAGS(IInteractive::Options)
}

#endif // MU_GLOBAL_IINTERACTIVE_H
