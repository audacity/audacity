#include <QtCore/QString>
#include <QtGui/QGuiApplication>
#include <QtQml/QQmlEngine>
#include "TranslationManager.h"

TranslationManager::TranslationManager(QObject *parent)
   : QObject(parent)
{
}

void TranslationManager::ChangeLanguage(const QString& code)
{
   if (TranslationAvailable(code)) {
      (void) translator.load(QString(":/translations/%1").arg(code));

      qApp->installTranslator(&translator);
      qmlEngine(this)->retranslate();
   } else {
      qApp->removeTranslator(&translator);
      qmlEngine(this)->retranslate();
   }
}

bool TranslationManager::TranslationAvailable(const QString& code) const
{
   return (code == "de");
}
