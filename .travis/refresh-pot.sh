#!/bin/bash

set -o errexit
set -o nounset

# We'll refresh the .pot files only when pushing to this branch
RPOT_PROCESS_BRANCH='master'

RPOT_REPOSITORY_OWNER='audacity'
RPOT_REPOSITORY_NAME='audacity'
RPOT_COMMIT_AUTHOR_NAME='Audacity TravisCI Bot'
RPOT_COMMIT_AUTHOR_EMAIL='travisci-bot@audacityteam.org'
RPOT_COMMIT_NAME_BASE='Automatic update of .pot files'
RPOT_COMMIT_NAME="[skip ci] ${RPOT_COMMIT_NAME_BASE}"

if test "${TRAVIS_PULL_REQUEST:-}" != 'false'; then
    printf '%s: skipping because it''s a pull request.\n' "${RPOT_COMMIT_NAME_BASE}"
    exit 0
fi

if test "${TRAVIS_BRANCH:-}" != "${RPOT_PROCESS_BRANCH}"; then
    printf '%s: skipping because pushing to "%s" instead of "%s".\n' "${RPOT_COMMIT_NAME_BASE}" "${TRAVIS_BRANCH:-}" "${RPOT_PROCESS_BRANCH}"
    exit 0
fi

if test "${TRAVIS_REPO_SLUG:-}" != "${RPOT_REPOSITORY_OWNER}/${RPOT_REPOSITORY_NAME}"; then
    printf '%s: skipping because repository is "%s" instead of "%s/%s".\n' "${RPOT_COMMIT_NAME_BASE}" "${TRAVIS_REPO_SLUG:-}" "${RPOT_REPOSITORY_OWNER}" "${RPOT_REPOSITORY_NAME}"
    exit 0
fi

if test -z "${GITHUB_ACCESS_TOKEN:-}"; then
    printf '%s: skipping because GITHUB_ACCESS_TOKEN is not available
To create it:
 - go to https://github.com/settings/tokens/new
 - create a new token
 - sudo apt update
 - sudo apt install -y build-essential ruby ruby-dev
 - sudo gem install travis
 - travis encrypt --repo %s/%s GITHUB_ACCESS_TOKEN=<YOUR_ACCESS_TOKEN>
 - Add to the env setting of:
   secure: "encrypted string"
' "${RPOT_COMMIT_NAME_BASE}" "${RPOT_REPOSITORY_OWNER}" "${RPOT_REPOSITORY_NAME}"
    exit 0
fi

printf '%s: checking out %s\n' "${RPOT_COMMIT_NAME_BASE}" "${TRAVIS_BRANCH}"
git checkout -qf "${TRAVIS_BRANCH}"

RPOT_TMPDIR=$(mktemp -d)
cleanup () {
    rm -rf "$RPOT_TMPDIR" || true
}
trap cleanup EXIT

printf '%s: updating languages\n' "${RPOT_COMMIT_NAME_BASE}"
cd "${TRAVIS_BUILD_DIR}/locale"
grep -v -E '^"POT-Creation-Date: [0-9]{4}-[0-9]{2}-[0-9]{2}' audacity.pot > "${RPOT_TMPDIR}/audacity.pot-pre"
./update_po_files.sh
grep -v -E '^"POT-Creation-Date: [0-9]{4}-[0-9]{2}-[0-9]{2}' audacity.pot > "${RPOT_TMPDIR}/audacity.pot-post"

printf '%s: checking changes\n' "${RPOT_COMMIT_NAME_BASE}"
RPOT_CHANGES_DETECTED=0
if test -n "$(diff "${RPOT_TMPDIR}/audacity.pot-pre" "${RPOT_TMPDIR}/audacity.pot-pre")"; then
    printf -- '- changes detected in audacity.pot\n'
    git add --all "${TRAVIS_BUILD_DIR}/locale/audacity.pot"
    RPOT_CHANGES_DETECTED=1
fi

if test ${RPOT_CHANGES_DETECTED} -eq 0; then
    printf '%s: skipping because assets are already up-to-date\n' "${RPOT_COMMIT_NAME_BASE}"
    exit 0
fi
    
printf '%s: commiting and pushing changes.\n' "${RPOT_COMMIT_NAME_BASE}"
git config user.name "${RPOT_COMMIT_AUTHOR_NAME}"
git config user.email "${RPOT_COMMIT_AUTHOR_EMAIL}"
git commit -m "${RPOT_COMMIT_NAME}"
git remote add deploy "https://${GITHUB_ACCESS_TOKEN}@github.com/${RPOT_REPOSITORY_OWNER}/${RPOT_REPOSITORY_NAME}.git"
git push deploy "${RPOT_PROCESS_BRANCH}" -vvv
printf '%s: repository updated.\n' "${RPOT_COMMIT_NAME_BASE}"
