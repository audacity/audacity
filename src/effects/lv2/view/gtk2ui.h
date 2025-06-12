/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <suil/suil.h>

struct _GtkWidget;
class QWindow;
class QWidget;

namespace au::effects {
class EventFilter;

class Gtk2Ui
{
public:
    struct Params {
        const QString& title;
        const int minimumWidth;
    };

    Gtk2Ui(SuilInstance&, std::function<void()> onClose, const Params& params);
    ~Gtk2Ui();

private:
    class EventFilter : public QObject
    {
    public:
        EventFilter(std::shared_ptr<QWidget>, std::function<void()> onClose);
        bool eventFilter(QObject* pObject, QEvent* pEvent) override;

    private:
        const std::shared_ptr<QWidget> m_qtWidget;
        const std::function<void()> m_onClose;
    };

    _GtkWidget* m_pGtkWindow = nullptr;
    std::shared_ptr<QWidget> m_qtWidget;
    std::unique_ptr<QWindow> m_qtWindow;
    std::unique_ptr<EventFilter> m_qtFilter;
};
}
