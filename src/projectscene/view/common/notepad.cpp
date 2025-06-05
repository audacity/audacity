/*
* Audacity: A Digital Audio Editor
*/
#include "notepad.h"
#include <QFileDialog>
#include <QFile>
#include <QTextStream>
#include <QGuiApplication>
#include <QClipboard>
#include <QMessageBox>
#include <QDebug>

using namespace au::projectscene;

Notepad::Notepad(QObject *parent)
    : QObject(parent)
{
}

Notepad::~Notepad()
{
}

QString Notepad::text() const {
    return currentText;
}

QString Notepad::file() const {
    return currentFile;
}

void Notepad::newFile()
{
    setText("");
    setFile("");
    emit textChanged();
}

void Notepad::setText(const QString &text) {
    if (currentText != text) {
        currentText = text;
        emit textChanged();
    }
}

void Notepad::setFile(const QString &text) {
    if (currentFile != text) {
        currentFile = text;
        emit fileChanged();
    }
}

void Notepad::openFile()
{
    QString fileName = QFileDialog::getOpenFileName(
        nullptr,
        "Open File",
        "",
        "Text Files (*.txt);;All Files (*)"
    );
    if (fileName.isEmpty()) return;

    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QFile::Text)) {
        qWarning() << "Cannot open file:" << file.errorString();
        return;
    }

    setFile(fileName);
    QTextStream in(&file);
    setText(in.readAll());
    file.close();

    emit fileChanged();
}

void Notepad::saveAs()
{
    QString fileName = QFileDialog::getSaveFileName(
        nullptr,
        "Save As",
        "",
        "Text Files (*.txt);;All Files (*)"
        );
    if (fileName.isEmpty()) return;

    if (!fileName.endsWith(".txt", Qt::CaseInsensitive))
        fileName += ".txt";

    currentFile = fileName;
    save();
}

void Notepad::save() {
    if (currentFile.isEmpty()) {
        saveAs();
        return;
    }

    QFile file(currentFile);
    if (!file.open(QIODevice::WriteOnly | QFile::Text)) {
        qWarning() << "Cannot save file:" << file.errorString();
        return;
    }

    QTextStream out(&file);
    out << currentText;  
    file.close();
}
