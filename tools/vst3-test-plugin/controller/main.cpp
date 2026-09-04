/*
 * Audacity: A Digital Audio Editor
 *
 * Controller for the Audacity test VST3 plugin: installs the built bundle into the
 * platform's VST3 folder and drives the plugin's load behaviour by writing its
 * validation gate file (see ../vst3testplugin.cpp and ../README.md) - no more
 * editing the file by hand. Changes are written as you make them; there is no
 * separate "apply" step.
 *
 * The validation gate path is resolved exactly as the plugin does, so the two always agree.
 */

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>

#include <QtWidgets>

namespace fs = std::filesystem;

namespace {
constexpr int VALIDATION_GATE_LOAD = 1;
constexpr int VALIDATION_GATE_CRASH = -1;
constexpr int VALIDATION_GATE_REFUSE = 2;
constexpr int DEFAULT_DELAY_SECONDS = 180; // the 3 min plugin-load timeout

// Must mirror vst3testplugin.cpp validationGateFilePath().
fs::path validationGateFilePath()
{
    if (const char* env = std::getenv("AU_VST3_TEST_PLUGIN_VALIDATION_GATE_FILE"); env && *env) {
        return env;
    }
    return fs::temp_directory_path() / "au_vst3_test_plugin_validation_gate";
}

// Baked in at build time; the user can still browse to another bundle.
fs::path defaultBundlePath()
{
#ifdef AU_VST3_TEST_PLUGIN_BUNDLE
    return AU_VST3_TEST_PLUGIN_BUNDLE;
#else
    return {};
#endif
}

QString bundleName()
{
#ifdef AU_VST3_TEST_PLUGIN_NAME
    return QStringLiteral(AU_VST3_TEST_PLUGIN_NAME ".vst3");
#else
    return QStringLiteral("AuVst3TestPlugin.vst3");
#endif
}

// The user's VST3 folder for this platform.
fs::path vst3InstallDir()
{
#if defined(Q_OS_MACOS)
    return fs::path(QDir::homePath().toStdString()) / "Library" / "Audio" / "Plug-Ins" / "VST3";
#elif defined(Q_OS_WIN)
    QString common = qEnvironmentVariable("COMMONPROGRAMFILES");
    if (common.isEmpty()) {
        common = QStringLiteral("C:/Program Files/Common Files");
    }
    return fs::path(common.toStdString()) / "VST3";
#else
    return fs::path(QDir::homePath().toStdString()) / ".vst3";
#endif
}

// Symlink on POSIX (rebuilds stay live, like the README's `ln -s`), copy on Windows.
// Whatever is at the destination is removed first, symlink-aware so an existing link
// is never followed into the build output.
bool installBundle(const fs::path& src, const fs::path& dst, QString& error)
{
    std::error_code ec;
    if (!fs::exists(src, ec)) {
        error = QStringLiteral("Bundle not found: %1").arg(QString::fromStdString(src.string()));
        return false;
    }
    fs::create_directories(dst.parent_path(), ec);
    ec.clear();
    if (fs::is_symlink(fs::symlink_status(dst, ec))) {
        fs::remove(dst, ec);
    } else if (fs::exists(dst, ec)) {
        fs::remove_all(dst, ec);
    }
    if (ec) {
        error = QStringLiteral("Could not replace %1: %2")
                .arg(QString::fromStdString(dst.string()), QString::fromStdString(ec.message()));
        return false;
    }
#if defined(Q_OS_WIN)
    fs::copy(src, dst, fs::copy_options::recursive, ec);
#else
    fs::create_directory_symlink(src, dst, ec);
#endif
    if (ec) {
        error = QString::fromStdString(ec.message());
        return false;
    }
    return true;
}

struct ValidationGate {
    int code = VALIDATION_GATE_LOAD;
    int delaySeconds = 0;
};

// `<code> [delaySeconds]`, mirroring the plugin's readValidationGate(): a missing or malformed
// file means "load normally".
ValidationGate readValidationGate(const fs::path& path)
{
    std::ifstream in(path);
    ValidationGate gate;
    if (!in || !(in >> gate.code)) {
        return ValidationGate {};
    }
    if (!(in >> gate.delaySeconds) || gate.delaySeconds < 0) {
        gate.delaySeconds = 0;
    }
    return gate;
}

bool writeValidationGate(const fs::path& path, const ValidationGate& gate, QString& error)
{
    std::ofstream out(path, std::ios::trunc);
    if (!out) {
        error = QStringLiteral("Cannot write %1").arg(QString::fromStdString(path.string()));
        return false;
    }
    out << gate.code;
    if (gate.delaySeconds > 0) {
        out << ' ' << gate.delaySeconds;
    }
    out << '\n';
    return static_cast<bool>(out);
}

QString describe(const ValidationGate& gate)
{
    const QString what = gate.code == VALIDATION_GATE_CRASH ? QStringLiteral("crash")
                         : gate.code == VALIDATION_GATE_REFUSE ? QStringLiteral("refuse to load")
                         : QStringLiteral("load");
    if (gate.delaySeconds > 0) {
        return QStringLiteral("On its next load the plugin will %1 after %2 s.").arg(what).arg(gate.delaySeconds);
    }
    return QStringLiteral("On its next load the plugin will %1 immediately.").arg(what);
}
}

int main(int argc, char* argv[])
{
    QApplication app(argc, argv);

    QWidget window;
    window.setWindowTitle(QStringLiteral("Audacity test VST3 plugin - controller"));
    auto* layout = new QVBoxLayout(&window);

    // ---- Installation -------------------------------------------------------
    auto* installBox = new QGroupBox(QStringLiteral("Installation"));
    auto* installForm = new QFormLayout(installBox);
    auto* bundleEdit = new QLineEdit(QString::fromStdString(defaultBundlePath().string()));
    auto* browseButton = new QPushButton(QStringLiteral("Browse\u2026"));
    auto* bundleRow = new QHBoxLayout;
    bundleRow->addWidget(bundleEdit);
    bundleRow->addWidget(browseButton);
    installForm->addRow(QStringLiteral("Built bundle:"), bundleRow);
    const fs::path installDst = vst3InstallDir() / bundleName().toStdString();
    installForm->addRow(QStringLiteral("Install to:"), new QLabel(QString::fromStdString(installDst.string())));
    auto* installButton = new QPushButton(QStringLiteral("Install"));
    installForm->addRow(installButton);
    layout->addWidget(installBox);

    // ---- Load result: two independent choices, each in its own group -------------
    auto* loadBox = new QGroupBox(QStringLiteral("Load result"));
    auto* loadLayout = new QVBoxLayout(loadBox);

    auto* outcomeBox = new QGroupBox(QStringLiteral("Outcome"));
    auto* outcomeLayout = new QVBoxLayout(outcomeBox);
    auto* succeedRadio = new QRadioButton(QStringLiteral("Succeed"));
    auto* crashRadio = new QRadioButton(QStringLiteral("Crash"));
    auto* refuseRadio = new QRadioButton(QStringLiteral("Refuse to load"));
    // Radio buttons sharing a parent form one exclusive set by default; explicit
    // groups keep "outcome" and "when" independent.
    auto* outcomeGroup = new QButtonGroup(outcomeBox);
    for (auto* radio : { succeedRadio, crashRadio, refuseRadio }) {
        outcomeGroup->addButton(radio);
        outcomeLayout->addWidget(radio);
    }
    loadLayout->addWidget(outcomeBox);

    auto* whenBox = new QGroupBox(QStringLiteral("When"));
    auto* whenLayout = new QHBoxLayout(whenBox);
    auto* immediatelyRadio = new QRadioButton(QStringLiteral("immediately"));
    auto* afterRadio = new QRadioButton(QStringLiteral("after"));
    auto* delaySpin = new QSpinBox;
    delaySpin->setRange(1, 24 * 3600);
    delaySpin->setValue(DEFAULT_DELAY_SECONDS);
    delaySpin->setSuffix(QStringLiteral(" s"));
    auto* whenGroup = new QButtonGroup(whenBox);
    whenGroup->addButton(immediatelyRadio);
    whenGroup->addButton(afterRadio);
    whenLayout->addWidget(immediatelyRadio);
    whenLayout->addWidget(afterRadio);
    whenLayout->addWidget(delaySpin);
    whenLayout->addStretch();
    loadLayout->addWidget(whenBox);

    layout->addWidget(loadBox);

    auto* status = new QLabel;
    status->setWordWrap(true);
    layout->addWidget(status);

    QObject::connect(afterRadio, &QRadioButton::toggled, delaySpin, &QSpinBox::setEnabled);

    // Reflect the validation gate file as it currently is, before wiring the write-on-change
    // handlers, so opening the controller never rewrites the file by itself.
    const fs::path validationGatePath = validationGateFilePath();
    const ValidationGate current = readValidationGate(validationGatePath);
    (current.code == VALIDATION_GATE_CRASH ? crashRadio : current.code == VALIDATION_GATE_REFUSE ? refuseRadio : succeedRadio)->setChecked(
        true);
    if (current.delaySeconds > 0) {
        delaySpin->setValue(current.delaySeconds);
        afterRadio->setChecked(true);
    } else {
        immediatelyRadio->setChecked(true);
    }
    delaySpin->setEnabled(afterRadio->isChecked());
    status->setText(describe(current));

    // ---- Behaviour ----------------------------------------------------------
    const auto applyValidationGate = [&] {
        ValidationGate gate;
        gate.code
            = crashRadio->isChecked() ? VALIDATION_GATE_CRASH : refuseRadio->isChecked() ? VALIDATION_GATE_REFUSE : VALIDATION_GATE_LOAD;
        gate.delaySeconds = afterRadio->isChecked() ? delaySpin->value() : 0;
        QString error;
        if (writeValidationGate(validationGatePath, gate, error)) {
            status->setText(describe(gate));
        } else {
            status->setText(QStringLiteral("Failed: %1").arg(error));
        }
    };
    const auto onToggled = [&](QAbstractButton*, bool checked) {
        if (checked) {
            applyValidationGate();
        }
    };
    QObject::connect(outcomeGroup, &QButtonGroup::buttonToggled, onToggled);
    QObject::connect(whenGroup, &QButtonGroup::buttonToggled, onToggled);
    QObject::connect(delaySpin, &QSpinBox::valueChanged, [&](int) {
        if (afterRadio->isChecked()) {
            applyValidationGate();
        }
    });

    QObject::connect(browseButton, &QPushButton::clicked, [&] {
        const QString dir = QFileDialog::getExistingDirectory(
            &window, QStringLiteral("Select the built .vst3 bundle"), bundleEdit->text());
        if (!dir.isEmpty()) {
            bundleEdit->setText(dir);
        }
    });

    QObject::connect(installButton, &QPushButton::clicked, [&] {
        QString error;
        if (installBundle(bundleEdit->text().toStdString(), installDst, error)) {
            status->setText(QStringLiteral("Installed to %1").arg(QString::fromStdString(installDst.string())));
        } else {
            status->setText(QStringLiteral("Install failed: %1").arg(error));
        }
    });

    window.show();
    return app.exec();
}
