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
echo "Notarize macOS .dmg"
trap 'echo Notarize failed; exit 1' ERR

set -o pipefail

ARTIFACTS_DIR="build.artifacts"
APPLE_USERNAME=""
APPLE_PASSWORD=""

# This information is public and can be extracted by anyone from the final .app file
APPLE_TEAM_ID="6EPAF2X3PR"

while [[ "$#" -gt 0 ]]; do
    case $1 in
        -u|--user) APPLE_USERNAME="$2"; shift ;;
        -p|--password) APPLE_PASSWORD="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

if [ -z "$APPLE_USERNAME" ]; then echo "error: not set APPLE_USERNAME"; exit 1; fi
if [ -z "$APPLE_PASSWORD" ]; then echo "error: not set APPLE_PASSWORD"; exit 1; fi

ARTIFACT_NAME="$(cat $ARTIFACTS_DIR/env/artifact_name.env)"
echo "ARTIFACT_NAME: $ARTIFACT_NAME"

echo "Uploading to Apple to notarize..."

for i in 1 2 3; do
    c=0
    xcrun notarytool submit \
        --apple-id $APPLE_USERNAME \
        --team-id $APPLE_TEAM_ID \
        --password $APPLE_PASSWORD \
        --wait $ARTIFACTS_DIR/$ARTIFACT_NAME \
        2>&1 | tee $ARTIFACTS_DIR/notarytool_submit_output.${i}.txt \
        || c=$?

    # Show log
    submission_id=$(cat $ARTIFACTS_DIR/notarytool_submit_output.${i}.txt | awk '/id: / { print $2;exit; }')
    xcrun notarytool log $submission_id \
        --apple-id $APPLE_USERNAME \
        --team-id $APPLE_TEAM_ID \
        --password $APPLE_PASSWORD \
        notarytool_log_output.${i}.json \
        && cat notarytool_log_output.${i}.json \
        || echo "Failed to get notarytool log"

    if [ $c -eq 0 ]; then break; fi
    if [ $i -eq 3 ]; then
        echo "notarytool failed; exiting after 3 retries."
        exit 1
    fi
    echo "notarytool failed; retrying in 30s"
    sleep 30
done

echo "Stapling and running packaging up"
xcrun stapler staple $ARTIFACTS_DIR/$ARTIFACT_NAME
echo "Staple finished!"
xcrun stapler validate $ARTIFACTS_DIR/$ARTIFACT_NAME
