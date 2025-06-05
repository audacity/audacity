/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QString>

namespace au::projectscene {
class Notepad : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString text READ text WRITE setText NOTIFY textChanged FINAL)
    Q_PROPERTY(QString file READ file WRITE setFile NOTIFY fileChanged FINAL)

public:
    explicit Notepad(QObject *parent = nullptr);
    ~Notepad();

    Q_INVOKABLE void newFile();
    Q_INVOKABLE void openFile();
    Q_INVOKABLE void saveAs();
    Q_INVOKABLE void save();
    Q_INVOKABLE void setText(const QString &text);
    Q_INVOKABLE void setFile(const QString &text);

    QString text() const;
    QString file() const;
    
signals:
    void fileChanged();
    void textChanged();
    void insertText(const QString &text);

private:
    QString currentFile;
    QString currentText;  
};
}
