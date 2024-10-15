#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-only
# Audacity-CLA-applies
#
# Audacity
# A Digital Audio Editor
#
# Copyright (C) 2024 Audacity Limited
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

import io
import sys
import json
import requests

YOUTUBE_API_URL = "https://youtube.googleapis.com/youtube/v3"
MAX_NUMBER_OF_RESULT_ITEMS = 100

class PlaylistItem:
    def __init__(self):
        self.title = ""
        self.author = ""
        self.videoUrl = ""
        self.thumbnailUrl = ""
        self.durationSecs = 0

def videoDurationSecs(durationInIsoFormat):
    # Available ISO8601 duration format: P#Y#M#DT#H#M#S
    import re
    regexp = re.compile(r'('
                        r'P'
                        r'((?P<years>[0-9]+)Y)?'
                        r'((?P<months>[0-9]+)M)?'
                        r'((?P<days>[0-9]+)D)?'
                        r'T'
                        r'((?P<hours>[0-9]+)H)?'
                        r'((?P<minutes>[0-9]+)M)?'
                        r'((?P<seconds>[0-9]+)S)?'
                        r')')

    match = regexp.match(durationInIsoFormat)

    if not match:
        return 0

    hours = int(match.group("hours") or 0)
    minutes = int(match.group("minutes") or 0)
    seconds = int(match.group("seconds") or 0)

    return seconds + minutes * 60 + hours * 60 * 60

def parsePlaylistItemsIds(playlistDoc):
    result = []
    items = playlistDoc["items"]

    for item in items:
        snippetObj = item["snippet"]
        resourceIdObj = snippetObj["resourceId"]
        videoId = resourceIdObj["videoId"]

        result.append(videoId)

    return result

def parseVideosInfo(videosInfoDoc):
    result = []
    items = videosInfoDoc["items"]

    for item in items:
        snippetObj = item["snippet"]

        playlistItem = PlaylistItem()

        playlistItem.title = snippetObj["title"]
        playlistItem.author = snippetObj["channelTitle"]

        playlistItem.url = "https://www.youtube.com/watch?v=" + item["id"]

        thumbnailsObj = snippetObj["thumbnails"]
        thumbnailsMediumObj = thumbnailsObj["medium"]
        playlistItem.thumbnailUrl = thumbnailsMediumObj["url"]

        contentDetails = item["contentDetails"]
        durationInIsoFormat = contentDetails["duration"]
        playlistItem.durationSecs = videoDurationSecs(durationInIsoFormat)

        result.append(playlistItem)

    return result

YOUTUBE_API_KEY = sys.argv[1]
PLAYLIST_ID = sys.argv[2]
PLAYLIST_FILE = sys.argv[3]

print("=== Read json file ===")

json_file = open(PLAYLIST_FILE, "r+")
jsonDict = json.load(json_file)
json_file.close()

print("=== Get playlist items ===")

headers = {
    'Accept': 'application/json',
}

url = YOUTUBE_API_URL + f"/playlistItems?part=snippet&playlistId={PLAYLIST_ID}&key={YOUTUBE_API_KEY}&maxResults={MAX_NUMBER_OF_RESULT_ITEMS}"
r = requests.get(url, headers=headers)

playlist_items = json.loads(r.text)

print("=== Parse playlist items ===")

playlist_items_ids = parsePlaylistItemsIds(playlist_items)

print("=== Get videos info ===")

params = f"part=snippet,contentDetails&key={YOUTUBE_API_KEY}&maxResults={MAX_NUMBER_OF_RESULT_ITEMS}"
for item_id in playlist_items_ids:
    params += f"&id={item_id}"

url = YOUTUBE_API_URL + f"/videos?{params}"
r = requests.get(url, headers=headers)

playlist_videos_info = json.loads(r.text)

print("=== Parse videos info ===")

playlist = parseVideosInfo(playlist_videos_info)

for item in playlist:
    new_item = {"title": item.title,
                "author": item.author,
                "url": item.url,
                "thumbnailUrl": item.thumbnailUrl,
                "durationSecs": item.durationSecs }
    jsonDict["default"].append(new_item)

json_file = open(PLAYLIST_FILE, "w")
json_file.write(json.dumps(jsonDict, indent=4))
json_file.close()
