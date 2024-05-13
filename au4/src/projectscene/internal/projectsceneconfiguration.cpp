#include "projectsceneconfiguration.h"

using namespace au::projectscene;

void ProjectSceneConfiguration::init()
{
    //! TODO We need to determine if there is a difference between light and dark themes.
    //! We need to determine whether some colors will be taken from the theme
    m_waveStyle.blankBrush = QColor("#262c30");
    m_waveStyle.samplePen = QColor("#7386e5");
    m_waveStyle.sampleBrush = QColor("#abb6ef");
    m_waveStyle.rmsPen = QColor("#abb6ef");
    m_waveStyle.clippedPen = QColor("#abb6ef");
    m_waveStyle.highlight = QColor("#FF00FF");
}

const WaveStyle& ProjectSceneConfiguration::waveStyle() const
{
    return m_waveStyle;
}
