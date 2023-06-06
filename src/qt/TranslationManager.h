#pragma once

#include <QtCore/QObject>
#include <QtCore/QTranslator>
#include <QtQml/qqmlregistration.h>

class QString;

class TranslationManager : public QObject
{
   Q_OBJECT
   QML_ELEMENT

public:
   explicit TranslationManager(QObject *parent = nullptr);
   virtual ~TranslationManager() = default;

   Q_INVOKABLE void ChangeLanguage(const QString& code);

private:
   bool TranslationAvailable(const QString& code) const;

private:
   QTranslator translator;
};
