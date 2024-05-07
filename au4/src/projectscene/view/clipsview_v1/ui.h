#ifndef UI_H
#define UI_H

#include <QGuiApplication>
#include <QColor>
#include <QFont>

namespace audacity {
class Theme
{
public:
    QColor backgroundColor1() const { return QColor("#2F353B"); }
    QColor fontColor1() const { return QColor("#F0F5FA"); }
    QColor strokeColor1() const { return QColor("#767B80"); }
    QColor strokeColor3() const { return QColor("#F0F5FA"); }

    QFont bodyFont() const
    {
        QFont font = QGuiApplication::font();
        font.setPixelSize(16);
        return font;
    }
};

class Ui
{
public:

    const Theme* theme() const { return &m_theme; }

private:

    Theme m_theme;
};
}

#endif // UI_H
