/*
 * Audacity: A Digital Audio Editor
 */

#include "gtk2ui.h"
#include "log.h"

#include "gtkheaders.h"

namespace au::effects {
namespace {
void maybeInitGtkFramework(void)
{
    static bool gtkInited = false;
    if (!gtkInited) {
        ::gtk_init(nullptr, nullptr);
        gtkInited = true;
    }
}

void onGtkClose(GtkWidget*, gpointer userData)
{
    static_cast<Gtk2Ui*>(userData)->close();
}

int minimumWidth(const QString& qTitle)
{
    const auto title = qTitle.toStdString();
    GtkWidget* const dummyLabel = gtk_label_new(title.c_str());
    PangoLayout* const layout = gtk_label_get_layout(GTK_LABEL(dummyLabel));
    int w = 0;
    if (layout) {
        int h = 0;
        pango_layout_get_pixel_size(layout, &w, &h);
    }
    gtk_widget_destroy(dummyLabel);
    return w + 150;
}

void onSizeAllocate(GtkWidget* widget, GdkRectangle* rect, gpointer userData)
{
    auto* const data = static_cast<Gtk2Ui::WidthData*>(userData);
    if (rect->width < data->minimumWidth) {
        gtk_window_set_default_size(GTK_WINDOW(data->window), data->minimumWidth, -1);
    }
}

void onKeyPressEvent(GtkWidget* widget, GdkEventKey* event, gpointer userData)
{
    auto* const ui = static_cast<Gtk2Ui*>(userData);
    switch (event->keyval) {
    case GDK_KEY_Escape:
        ui->keyPressed(Qt::Key_Escape);
        break;
    case GDK_KEY_space:
        ui->keyPressed(Qt::Key_Space);
        break;
    case GDK_KEY_Tab:
        ui->keyPressed(Qt::Key_Tab);
        break;
    }
}
} // namespace

Gtk2Ui::Gtk2Ui(SuilInstance& suilInstance, std::function<void()> onClose, std::function<void(Qt::Key)> onKeyPressed, const QString& title)
    : m_onClose{std::move(onClose)}, m_onKeyPressed{std::move(onKeyPressed)}, m_minimumWidth{minimumWidth(title)},
    m_pGtkWindow{gtk_window_new(GTK_WINDOW_TOPLEVEL)},
    m_widthData{m_minimumWidth, m_pGtkWindow}
{
    maybeInitGtkFramework();

    g_signal_connect(G_OBJECT(m_pGtkWindow), "destroy", G_CALLBACK(onGtkClose), this);
    g_signal_connect(G_OBJECT(m_pGtkWindow), "size-allocate", G_CALLBACK(onSizeAllocate), &m_widthData);
    g_signal_connect(G_OBJECT(m_pGtkWindow), "key-press-event", G_CALLBACK(onKeyPressEvent), this);

    gtk_container_add(GTK_CONTAINER(m_pGtkWindow), static_cast<GtkWidget*>(suil_instance_get_widget(&suilInstance)));
    gtk_window_set_title(GTK_WINDOW(m_pGtkWindow), title.toStdString().c_str());
    gtk_widget_show_all(m_pGtkWindow);
}

Gtk2Ui::~Gtk2Ui()
{
    if (m_pGtkWindow) {
        gtk_widget_destroy(m_pGtkWindow);
    }
}

void Gtk2Ui::close()
{
    IF_ASSERT_FAILED(m_onClose) {
        return;
    }
    m_onClose();
}

void Gtk2Ui::keyPressed(Qt::Key key)
{
    IF_ASSERT_FAILED(m_onKeyPressed) {
        return;
    }
    m_onKeyPressed(key);
}
}     // namespace au::effects
