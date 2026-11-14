#!/usr/bin/env python3
# **********************************************************************
#
#  Audacity: A Digital Audio Editor
#
# **********************************************************************

import json
import subprocess

# Run gh cache list command and capture output
result = subprocess.run(
    ['gh', 'cache', 'list', '--limit', '100', '--json', 'id,key,ref',
        '--sort', 'created_at', '--order', 'desc'],
    capture_output=True,
    text=True,
    check=True
)

# Parse JSON output and store in variable
cache_data = json.loads(result.stdout)

entry_keys_seen = set()
entries_to_delete = []

for cache in cache_data:
    cache_id = cache['id']
    cache_key = cache['key']
    cache_ref = cache['ref']
    print(
        f'\033[1;34mConsidering cache entry with id \033[1;36m{cache_id}\033[1;34m, key \033[1;36m{cache_key}\033[1;34m, ref \033[1;36m{cache_ref}\033[0m')

    # Only consider ccache entries
    if not '-ccache-' in cache_key:
        print('  Skipping: not a ccache entry')
        continue

    # Get the part before `-ccache-`
    cache_type = cache_key.split('-ccache-')[0]

    # Keep only the most recently created entry per (ref, type) combination
    entry_key = (cache_ref, cache_type)
    if entry_key in entry_keys_seen:
        print('  \033[31mMarking for deletion: superseded by newer entry\033[0m')
        entries_to_delete.append(cache)
        continue
    entry_keys_seen.add(entry_key)

    # For pull request refs, check if the PR is still open
    if cache_ref.startswith('refs/pull/'):
        pr_number = cache_ref.split('/')[2]
        pr_result = subprocess.run(
            ['gh', 'pr', 'view', pr_number, '--json', 'state'],
            capture_output=True,
            text=True,
            check=False
        )
        if pr_result.returncode != 0:
            print(
                '  \033[31mMarking for deletion: pull request not found\033[0m')
            entries_to_delete.append(cache)
            continue
        pr_data = json.loads(pr_result.stdout)
        if pr_data['state'] != 'OPEN':
            print(
                '  \033[31mMarking for deletion: pull request not open\033[0m')
            entries_to_delete.append(cache)
            continue

    print('  \033[32mKeeping this cache entry\033[0m')

# Print which entries will be deleted
print(f"\n\033[1;34mTotal cache entries: {len(cache_data)}\033[0m")
print(f"\033[1;34mEntries to delete: {len(entries_to_delete)}\033[0m")

for entry in entries_to_delete:
    print(
        f"  Deleting cache entry with id {entry['id']}, key {entry['key']}, ref {entry['ref']}")
    subprocess.run(['gh', 'cache', 'delete', str(entry['id'])],
                   check=False)

print("\033[1;32mCleanup complete.\033[0m")
