#!/bin/bash
# SPDX-License-Identifier: GPL-3.0-only
# Audacity-CLA-applies
#
# Audacity
# A Digital Audio Editor
#
# Copyright (C) 2026 Audacity
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

BUILD_TOOLS=$HOME/build_tools
ENV_FILE=$BUILD_TOOLS/environment.sh

source $ENV_FILE

trap 'code=$?; echo "Error: command \`$BASH_COMMAND\` exited with code $code." >&2; exit 1' ERR

command -v s3cmd >/dev/null 2>&1 || { echo "error: not found 's3cmd'" >&2; exit 1; }

echo "s3cmd: $(s3cmd --version)"

echo "Updating translation on s3..."
SCRIPT_PATH=$(dirname $0)
python3 $SCRIPT_PATH/s3_packandsend.py
echo "Translation updated"
