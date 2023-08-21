#include "UiThemeHandler.h"

#include <QFontDatabase>
#include <QGuiApplication>
#include <QtCore/QFileInfo>
#include <QtCore/QDirIterator>

UiThemeHandler::UiThemeHandler()
{
   m_timecodeFont = QFontDatabase::font("Lato", "Bold", 14);
   m_iconFont = QFontDatabase::font("MusescoreIcon", "", 12);
   m_bodyFont = QGuiApplication::font();
   m_bodyFont.setPixelSize(16);
}

QStringList UiThemeHandler::themeFiles()
{
   QStringList themes;
   QStringList excludeFiles{ "Theme.qml", "UiTheme.qml" };

   QDirIterator it(
      QString(":%1/Audacity/UiThemes")
         .arg(uithemes_QML_IMPORT_PREFIX),
      { "*.qml" },
      QDir::Files,
      QDirIterator::Subdirectories);
   while (it.hasNext())
   {
      auto path = it.next();
      auto fileName = QFileInfo(path).fileName();

      if (!excludeFiles.contains(fileName))
         themes << fileName;
   }

   return themes;
}

QFont UiThemeHandler::IconFont() const
{
   return m_iconFont;
}

QFont UiThemeHandler::BodyFont() const
{
   return m_bodyFont;
}

QFont UiThemeHandler::TimecodeFont() const
{
   return m_timecodeFont;
}
