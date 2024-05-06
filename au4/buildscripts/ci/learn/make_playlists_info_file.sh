#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-Studio-CLA-applies
#
# MuseScore Studio
# Music Composition & Notation
#
# Copyright (C) 2021 MuseScore Limited
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

ARTIFACTS_DIR=build.artifacts

YOUTUBE_API_KEY=""
YOUTUBE_PLAYLIST_ID=""

while [[ "$#" -gt 0 ]]; do
    case $1 in
        --youtube_api_key) YOUTUBE_API_KEY="$2"; shift ;;
        --youtube_playlist_id) YOUTUBE_PLAYLIST_ID="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

echo "=== Make json file ==="

json=$(jq -n --argjson default [] \
             '$ARGS.named')

mkdir -p $ARTIFACTS_DIR
echo $json > $ARTIFACTS_DIR/playlist.json
cat $ARTIFACTS_DIR/playlist.json

echo "=== Make playlist for YouTube ==="

HERE="$(cd "$(dirname "$0")" && pwd)"
python3 $HERE/make_youtube_playlist_info.py ${YOUTUBE_API_KEY} ${YOUTUBE_PLAYLIST_ID} ${ARTIFACTS_DIR}/playlist.json
