/*
 * Audacity: A Digital Audio Editor
 */
#include "algorithmsectionparameterlistmodel.h"

#include "abstractspectrogramsettingsmodel.h"
#include "./spectrogramviewutils.h"
#include "../spectrogramtypes.h"

#include "framework/global/translation.h"
#include "framework/global/log.h"

#include <limits>

namespace au::spectrogram {
namespace {
const Table<SpectrogramAlgorithm> algorithmTable {
    { 0, SpectrogramAlgorithm::Frequencies, muse::qtrc("spectrogram/preferences", "Frequencies") },
    { 1, SpectrogramAlgorithm::Reassignment, muse::qtrc("spectrogram/preferences", "Reassignment") },
    { 2, SpectrogramAlgorithm::Pitch, muse::qtrc("spectrogram/preferences", "Pitch") },
};

const Table<SpectrogramWindowType> windowTypeTable {
    { 0, SpectrogramWindowType::Rectangular, muse::qtrc("spectrogram/preferences", "Rectangular") },
    { 1, SpectrogramWindowType::Bartlett, muse::qtrc("spectrogram/preferences", "Bartlett") },
    { 2, SpectrogramWindowType::Hamming, muse::qtrc("spectrogram/preferences", "Hamming") },
    { 3, SpectrogramWindowType::Hann, muse::qtrc("spectrogram/preferences", "Hann") },
    { 4, SpectrogramWindowType::Blackman, muse::qtrc("spectrogram/preferences", "Blackman") },
    { 5, SpectrogramWindowType::BlackmanHarris, muse::qtrc("spectrogram/preferences", "Blackman-Harris") },
    { 6, SpectrogramWindowType::Welch, muse::qtrc("spectrogram/preferences", "Welch") },
    { 7, SpectrogramWindowType::Gaussian25, muse::qtrc("spectrogram/preferences", "Gaussian (a=0.25)") },
    { 8, SpectrogramWindowType::Gaussian35, muse::qtrc("spectrogram/preferences", "Gaussian (a=0.35)") },
    { 9, SpectrogramWindowType::Gaussian45, muse::qtrc("spectrogram/preferences", "Gaussian (a=0.45)") },
};

const Table<int> windowSizeTable {
    { 0, 8, muse::qtrc("spectrogram/preferences", "8 - most wideband") },
    { 1, 16, "16" },
    { 2, 32, "32" },
    { 3, 64, "64" },
    { 4, 128, "128" },
    { 5, 256, "256" },
    { 6, 512, "512" },
    { 7, 1024, "1024" },
    { 8, 2048, "2048" },
    { 9, 4096, "4096" },
    { 10, 8192, "8192" },
    { 11, 16384, "16384" },
    { 12, 32768, muse::qtrc("spectrogram/preferences", "32768 - most narrowband") },
};

const Table<int> zeroPaddingFactorTable {
    { 0, 1, "1" },
    { 1, 2, "2" },
    { 2, 4, "4" },
    { 3, 8, "8" },
    { 4, 16, "16" },
};
} // namespace

AlgorithmSectionParameterListModel::AlgorithmSectionParameterListModel(QObject* parent)
    : AbstractSectionParametersListModel(parent) {}

void AlgorithmSectionParameterListModel::onSettingsModelSet(AbstractSpectrogramSettingsModel&)
{
    const QList<int> roles { ControlCurrentIndexRole, ControlCurrentValueRole };
    CONNECT_SETTING_CHANGED(algorithmChanged, Control::Algorithm, roles);
    CONNECT_SETTING_CHANGED(windowSizeChanged, Control::WindowSize, roles);
    CONNECT_SETTING_CHANGED(windowTypeChanged, Control::WindowType, roles);
    CONNECT_SETTING_CHANGED(zeroPaddingFactorChanged, Control::ZeroPaddingFactor, roles);
}

QVariant AlgorithmSectionParameterListModel::data(const QModelIndex& index, int role) const
{
    if (!m_settingsModel) {
        return QVariant{};
    }

    const auto control = static_cast<Control>(index.row());
    switch (role) {
    case ControlLabelRole: return controlLabel(control);
    case ControlWidthRole: return controlWidth(control);
    case ControlPossibleValuesRole: return controlPossibleValues(control);
    case ControlCurrentIndexRole: return controlCurrentIndex(control);
    case ControlCurrentValueRole: return controlCurrentValue(control);
    default:
        assert(false);
        return QVariant{};
    }
}

QHash<int, QByteArray> AlgorithmSectionParameterListModel::roleNames() const
{
    return {
        { ControlLabelRole, "controlLabel" },
        { ControlWidthRole, "controlWidth" },
        { ControlPossibleValuesRole, "controlPossibleValues" },
        { ControlCurrentIndexRole, "controlCurrentIndex" },
        { ControlCurrentValueRole, "controlCurrentValue" }
    };
}

QString AlgorithmSectionParameterListModel::controlLabel(Control control) const
{
    switch (control) {
    case Algorithm:
        return muse::qtrc("spectrogram/preferences", "Algorithm");
    case WindowSize:
        return muse::qtrc("spectrogram/preferences", "Window size");
    case WindowType:
        return muse::qtrc("spectrogram/preferences", "Window type");
    case ZeroPaddingFactor:
        return muse::qtrc("spectrogram/preferences", "Zero padding factor");
    default:
        assert(false);
        return QString{};
    }
}

int AlgorithmSectionParameterListModel::controlWidth(Control control) const
{
    switch (control) {
    case Algorithm: return controlWidthL();
    case WindowSize: return controlWidthL();
    case WindowType: return controlWidthL();
    case ZeroPaddingFactor: return controlWidthS();
    default:
        assert(false);
        return 0;
    }
}

namespace {
template<typename T>
QVariantList toQVariantList(const QList<T>& list)
{
    QVariantList variantList;
    variantList.reserve(list.size());
    for (const T& item : list) {
        variantList.push_back(QVariant::fromValue(item));
    }
    return variantList;
}
}

QVariantList AlgorithmSectionParameterListModel::controlPossibleValues(Control control) const
{
    switch (control) {
    case Algorithm: return toQVariantList(propertyNames(algorithmTable));
    case WindowSize: return toQVariantList(propertyNames(windowSizeTable));
    case WindowType: return toQVariantList(propertyNames(windowTypeTable));
    case ZeroPaddingFactor: return toQVariantList(propertyNames(zeroPaddingFactorTable));
    default:
        assert(false);
        return {};
    }
}

int AlgorithmSectionParameterListModel::controlCurrentValue(Control control) const
{
    switch (control) {
    case Algorithm: {
        return m_settingsModel->algorithm();
    }
    case WindowSize: {
        return m_settingsModel->windowSize();
    }
    case WindowType: {
        return m_settingsModel->windowType();
    }
    case ZeroPaddingFactor: {
        return m_settingsModel->zeroPaddingFactor();
    }
    default:
        assert(false);
        return {};
    }
}

int AlgorithmSectionParameterListModel::controlCurrentIndex(Control control) const
{
    switch (control) {
    case Algorithm:
        return propertyIndex(algorithmTable, static_cast<SpectrogramAlgorithm>(m_settingsModel->algorithm()));
    case WindowSize:
        return propertyIndex(windowSizeTable, m_settingsModel->windowSize());
    case WindowType:
        return propertyIndex(windowTypeTable, static_cast<SpectrogramWindowType>(m_settingsModel->windowType()));
    case ZeroPaddingFactor:
        return propertyIndex(zeroPaddingFactorTable, m_settingsModel->zeroPaddingFactor());
    default:
        assert(false);
        return 0;
    }
}

bool AlgorithmSectionParameterListModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (role != ControlCurrentIndexRole) {
        return false;
    }

    IF_ASSERT_FAILED(m_settingsModel) {
        return false;
    }

    const auto control = static_cast<Control>(index.row());
    const auto currentIndex = value.toInt();

    switch (control) {
    case Algorithm:
        m_settingsModel->setAlgorithm(static_cast<int>(propertyValue(algorithmTable, currentIndex)));
        break;
    case WindowSize:
        m_settingsModel->setWindowSize(propertyValue(windowSizeTable, currentIndex));
        break;
    case WindowType:
        m_settingsModel->setWindowType(static_cast<int>(propertyValue(windowTypeTable, currentIndex)));
        break;
    case ZeroPaddingFactor:
        m_settingsModel->setZeroPaddingFactor(static_cast<int>(propertyValue(zeroPaddingFactorTable, currentIndex)));
        break;
    default:
        assert(false);
        return false;
    }

    return true;
}
}
