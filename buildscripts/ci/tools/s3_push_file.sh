#!/usr/bin/env bash
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

S3_KEY=""
S3_SECRET=""
S3_URL=""

FILE_PATH=""

while [[ "$#" -gt 0 ]]; do
    case $1 in
        --s3_key) S3_KEY="$2"; shift ;;
        --s3_secret) S3_SECRET="$2"; shift ;;
        --s3_url) S3_URL="$2"; shift ;;
        --file_path) FILE_PATH="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

bash ./buildscripts/ci/tools/s3_install.sh --s3_key ${S3_KEY} --s3_secret ${S3_SECRET}

echo "=== Publish to S3 ==="

s3cmd put --acl-public --guess-mime-type $FILE_PATH "$S3_URL"
