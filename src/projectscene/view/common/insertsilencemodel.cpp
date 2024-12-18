/*
* Audacity: A Digital Audio Editor
*/

#include "AudacityException.h"
#include "NumericConverterFormats.h"
#include "playback/iaudiooutput.h"
#include "translation.h"
#include "insertsilencemodel.h"

using namespace au::projectscene;

void InsertSilenceModel::init()
{
    muse::secs_t begin = selectionController()->dataSelectedStartTime();
    muse::secs_t end = selectionController()->dataSelectedEndTime();

    m_duration = end - begin;
    m_durationFormat = QString::fromStdString(configuration()->insertSilenceDurationFormat());

    if (muse::is_zero(m_duration)) {
        m_duration = configuration()->insertSilenceDuration().to_double();
    } else {
        m_durationFormat = QString::fromStdString(NumericConverterFormats::TimeAndSampleFormat().Translation().ToStdString());
    }

    emit durationChanged();
    emit durationFormatChanged();
}

int InsertSilenceModel::sampleRate() const
{
    return playback()->audioOutput()->sampleRate();
}

double au::projectscene::InsertSilenceModel::duration() const
{
    return m_duration;
}

void au::projectscene::InsertSilenceModel::setDuration(double duration)
{
    if (muse::is_equal(m_duration, duration)) {
        return;
    }
    m_duration = duration;
    emit durationChanged();
}

QString InsertSilenceModel::durationFormat() const
{
    return m_durationFormat;
}

void InsertSilenceModel::setDurationFormat(const QString& newDurationFormat)
{
    if (m_durationFormat == newDurationFormat) {
        return;
    }
    m_durationFormat = newDurationFormat;
    emit durationFormatChanged();
}

void InsertSilenceModel::apply()
{
    trackedit::TrackIdList trackIds = selectionController()->selectedTracks();
    muse::secs_t begin = selectionController()->dataSelectedStartTime();
    muse::secs_t end = selectionController()->dataSelectedEndTime();

    try {
        trackeditInteraction()->insertSilence(trackIds, begin, end, m_duration);
        selectionController()->setDataSelectedStartTime(begin, false);
        selectionController()->setDataSelectedEndTime(begin + m_duration, true);

        configuration()->setInsertSilenceDuration(m_duration);
        configuration()->setInsertSilenceDurationFormat(m_durationFormat.toStdString());
    }
    catch (::AudacityException& e) {
        if (const auto msg = dynamic_cast<MessageBoxException*>(&e)) {
            std::string title = muse::trc("projectscene/silence", "Action cannot be completed");
            interactive()->warning(title, msg->ErrorMessage().Translation().ToStdString());
        }
    }
}
