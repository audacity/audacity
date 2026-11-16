/*
* Audacity: A Digital Audio Editor
*/
#include "commandsearch.h"

#include <algorithm>
#include <string>
#include <vector>

#include <QStringList>

#include "framework/global/stringsearch.h"

using namespace au::macromanager;

namespace {
constexpr std::size_t MIN_WORD_SIZE_FOR_FUZZY_MATCH = 4;
constexpr std::size_t CHARS_PER_ERROR = 8;
constexpr double SIMILARITY_WEIGHT = 5.0;
constexpr double SUBTITLE_WEIGHT = 0.5;
constexpr double TEXT_START_WEIGHT = 2.0;

struct ScoredMatch {
    double score = 0.0;
    muse::FuzzyMatch match;
};

std::u32string lowered(const QString& text)
{
    std::u32string result = text.toStdU32String();
    std::ranges::transform(result, result.begin(), [](char32_t c) { return QChar::toLower(c); });
    return result;
}

std::optional<ScoredMatch> bestMatch(muse::FuzzyMatcher& matcher, const std::u32string& text, const std::u32string& word)
{
    const std::size_t maxDistance = word.size() >= MIN_WORD_SIZE_FOR_FUZZY_MATCH ? 1 + word.size() / CHARS_PER_ERROR : 0;
    const double inverseWordSize = 1.0 / word.size();

    std::optional<ScoredMatch> best;
    for (const muse::FuzzyMatch& match : matcher(text, word, maxDistance)) {
        double score = SIMILARITY_WEIGHT * (1.0 - match.editDistance * inverseWordSize);

        const bool startsWord = match.beginPos == 0 || text[match.beginPos - 1] == U' ';
        const bool endsWord = match.endPos == text.size() || text[match.endPos] == U' ';
        if (startsWord) {
            score += (endsWord ? 2.0 : 1.0) * inverseWordSize;
        }

        if (match.beginPos == 0) {
            score += TEXT_START_WEIGHT * inverseWordSize;
        }

        if (!best || best->score < score) {
            best = ScoredMatch { score, match };
        }
    }

    return best;
}

QString highlighted(const std::u32string& title, std::vector<muse::FuzzyMatch> matches)
{
    const auto escaped = [&title](std::size_t begin, std::size_t end) {
        return QString::fromStdU32String(title.substr(begin, end - begin)).toHtmlEscaped();
    };

    std::ranges::sort(matches, {}, &muse::FuzzyMatch::beginPos);

    QString result;
    std::size_t pos = 0;
    for (const muse::FuzzyMatch& match : matches) {
        const std::size_t begin = std::max(pos, match.beginPos);
        if (match.endPos <= begin) {
            continue;
        }

        result += escaped(pos, begin) + u"<b>" + escaped(begin, match.endPos) + u"</b>";
        pos = match.endPos;
    }

    return result + escaped(pos, title.size());
}
}

std::optional<CommandSearchResult> au::macromanager::searchCommand(const QString& query, const QString& title, const QString& subtitle)
{
    const std::u32string lowerTitle = lowered(title);
    const std::u32string lowerSubtitle = lowered(subtitle);

    muse::FuzzyMatcher matcher;
    double score = 0.0;
    std::vector<muse::FuzzyMatch> titleMatches;

    for (const QString& queryWord : query.split(u' ', Qt::SkipEmptyParts)) {
        const std::u32string word = lowered(queryWord);

        if (const std::optional<ScoredMatch> titleMatch = bestMatch(matcher, lowerTitle, word)) {
            score += titleMatch->score;
            titleMatches.push_back(titleMatch->match);
            continue;
        }

        const std::optional<ScoredMatch> subtitleMatch = bestMatch(matcher, lowerSubtitle, word);
        if (!subtitleMatch) {
            return std::nullopt;
        }

        score += SUBTITLE_WEIGHT * subtitleMatch->score;
    }

    return CommandSearchResult { score, highlighted(title.toStdU32String(), std::move(titleMatches)) };
}
