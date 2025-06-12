/*
 * Audacity: A Digital Audio Editor
 */

#include "gtk2ui.h"

#include <QVBoxLayout>
#include <QWindow>

// Qt defines the `signals` macro, which is the name of a struct member in some GTK header file ...
// Temporarily swap `signals` with something else.
#define MY_TEMP_VAR signals
#undef signals
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <gobject/gsignal.h>
#define signals MY_TEMP_VAR
#undef SIGNALS

namespace au::effects {
Gtk2Ui::EventFilter::EventFilter(std::shared_ptr<QWidget> qtWidget, std::function<void()> onClose)
    : QObject{nullptr}, m_qtWidget{std::move(qtWidget)}, m_onClose{std::move(onClose)}
{
    m_qtWidget->installEventFilter(this);
}

bool Gtk2Ui::EventFilter::eventFilter(QObject* pObject, QEvent* pEvent)
{
    if (pObject == static_cast<QObject*>(m_qtWidget.get())) {
        const QEvent::Type eventType = pEvent->type();
        switch (eventType) {
        case QEvent::Close: {
            // Defer widget close!
            m_qtWidget->removeEventFilter(this);
            m_onClose();
            pEvent->ignore();
            return true;
        }
        case QEvent::Resize: {
            // We will want to handle this if we ever manage to sync position of this GTK UI with that of the QML popup ...
        }
        default:
            break;
        }
    }

    return QObject::eventFilter(pObject, pEvent);
}

namespace {
void maybeInitGtkFramework(void)
{
    static bool gtkInited = false;
    if (!gtkInited) {
        ::gtk_init(nullptr, nullptr);
        gtkInited = true;
    }
}

static void onSizeRequest(GtkWidget*, GtkRequisition* req, gpointer userData)
{
    QWidget* const pQtWidget = static_cast<QWidget*>(userData);
    pQtWidget->setMinimumSize(req->width, req->height);
}

static void onSizeAllocate(GtkWidget*, GdkRectangle* rect, gpointer userData)
{
    QWidget* const pQtWidget = static_cast<QWidget*>(userData);
    pQtWidget->resize(rect->width, rect->height);
}
}

Gtk2Ui::Gtk2Ui(SuilInstance& suilInstance, std::function<void()> onClose, const Params& params)
{
    maybeInitGtkFramework();

    m_pGtkWindow = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_resizable(GTK_WINDOW(m_pGtkWindow), 0);

    // Create embeddable native window...
    GtkWidget* const pGtkWidget = static_cast<GtkWidget*>(suil_instance_get_widget(&suilInstance));

    // Add plugin widget into our new window container...
    gtk_container_add(GTK_CONTAINER(m_pGtkWindow), pGtkWidget);
    gtk_widget_show_all(m_pGtkWindow);

    // Embed native GTK+ window into a Qt widget...
    const WId wid = GDK_WINDOW_XID(gtk_widget_get_window(m_pGtkWindow));
    m_qtWindow.reset(QWindow::fromWinId(wid));

    // Create the new parent frame...
    m_qtWidget = std::make_shared<QWidget>(nullptr /* pParent */, Qt::Window);
    m_qtWidget->setAttribute(Qt::WA_QuitOnClose, false);
    QWidget* pQtContainer = QWidget::createWindowContainer(m_qtWindow.get());
    QVBoxLayout* pVBoxLayout = new QVBoxLayout();
    pVBoxLayout->setContentsMargins(0, 0, 0, 0);
    pVBoxLayout->setSpacing(0);
    pVBoxLayout->addWidget(pQtContainer);
    m_qtWidget->setLayout(pVBoxLayout);

    // Get initial window size...
    GtkAllocation alloc;
    gtk_widget_get_allocation(pGtkWidget, &alloc);
    m_qtWidget->resize(alloc.width, alloc.height);

    g_signal_connect(G_OBJECT(m_pGtkWindow), "size-request", G_CALLBACK(onSizeRequest), m_qtWidget.get());
    g_signal_connect(G_OBJECT(m_pGtkWindow), "size-allocate", G_CALLBACK(onSizeAllocate), m_qtWidget.get());

    m_qtWidget->show();
    m_qtWidget->raise();
    m_qtWidget->activateWindow();
    m_qtWidget->setWindowTitle(params.title);
    m_qtWidget->setMinimumWidth(params.minimumWidth);

    m_qtFilter = std::make_unique<EventFilter>(m_qtWidget, std::move(onClose));
}

Gtk2Ui::~Gtk2Ui()
{
    m_qtWidget->hide();
    if (m_qtWindow) {
        m_qtWindow->setParent(nullptr);
    }
    if (m_pGtkWindow) {
        gtk_widget_destroy(m_pGtkWindow);
    }
}
}     // namespace au::effects
