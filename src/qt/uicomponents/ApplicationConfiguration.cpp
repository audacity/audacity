#include <QFontDatabase>
#include <QGuiApplication>
#include "ApplicationConfiguration.h"

ApplicationConfiguration::ApplicationConfiguration()
{
   QFontDatabase::addApplicationFont(":/fonts/MusescoreIcon.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Bold.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-BoldItalic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Italic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Regular.ttf");

   m_timecodeFont = QFontDatabase::font("Lato", "Bold", 14);
   m_iconFont = QFontDatabase::font("MusescoreIcon", "", 12);
   m_bodyFont = QGuiApplication::font();
   m_bodyFont.setPixelSize(16);
}

QFont ApplicationConfiguration::IconFont() const
{
   return m_iconFont;
}

QFont ApplicationConfiguration::BodyFont() const
{
   return m_bodyFont;
}

QFont ApplicationConfiguration::TimecodeFont() const
{
   return m_timecodeFont;
}
