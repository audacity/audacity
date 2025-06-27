/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <suil/suil.h>

struct _GtkWidget;
class QWindow;
class QWidget;

namespace au::effects {
class Gtk2Ui
{
public:
    Gtk2Ui(SuilInstance&, std::function<void()> onClose, std::function<void(Qt::Key)> onKeyPressed, const QString& title);
    ~Gtk2Ui();

    // For internal use
public:
    struct WidthData {
        const int& minimumWidth;
        _GtkWidget* const window;
    };

    void close();
    void keyPressed(Qt::Key);

private:
    const std::function<void()> m_onClose;
    const std::function<void(Qt::Key)> m_onKeyPressed;
    const int m_minimumWidth;
    _GtkWidget* const m_pGtkWindow;
    WidthData m_widthData;
};
}
