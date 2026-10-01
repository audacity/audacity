#!/usr/bin/env python3
"""Local project server for running several Audacity instances on one project.

Speaks the same HTTP API as audio.com's project sync (the subset that
au3-cloud-audiocom calls to save, list and open projects), so that Audacity's
existing sync code can be used against it. It runs on this machine only and
never contacts audio.com. Authentication is accepted unconditionally.

State (projects, snapshots, project blobs and audio blocks) is kept under
--data-dir and survives restarts.
"""

import argparse
import json
import os
import re
import sys
import threading
import time
import uuid
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qs, urlsplit

USERNAME = "local"


def default_data_dir():
    if sys.platform == "win32":
        base = Path(os.environ.get("LOCALAPPDATA", Path.home() / "AppData/Local"))
    elif sys.platform == "darwin":
        base = Path.home() / "Library/Application Support"
    else:
        base = Path(os.environ.get("XDG_DATA_HOME", Path.home() / ".local/share"))
    return base / "audacity-multiinstance-server"


def now():
    return int(time.time())


class Store:
    """Projects and snapshots in a JSON file; blobs and blocks as files."""

    def __init__(self, data_dir: Path):
        self.dir = data_dir
        self.blocks_dir = data_dir / "blocks"
        self.files_dir = data_dir / "files"
        self.mixdowns_dir = data_dir / "mixdowns"
        for d in (self.blocks_dir, self.files_dir, self.mixdowns_dir):
            d.mkdir(parents=True, exist_ok=True)
        self.db_path = data_dir / "db.json"
        self.lock = threading.RLock()
        self.db = json.loads(self.db_path.read_text()) if self.db_path.exists() else {"projects": {}, "snapshots": {}}

    def save(self):
        tmp = self.db_path.with_suffix(".tmp")
        tmp.write_text(json.dumps(self.db, indent=1))
        tmp.replace(self.db_path)

    def has_block(self, block_hash):
        return (self.blocks_dir / block_hash.upper()).exists()

    def block_path(self, block_hash):
        return self.blocks_dir / block_hash.upper()


class Api:
    def __init__(self, store: Store, base_url: str):
        self.store = store
        self.base = base_url

    # --- JSON shapes, as parsed by sync/CloudSyncDTO.cpp ---

    def snapshot_info(self, snapshot, expand=False):
        info = {
            "id": snapshot["id"],
            "parent_id": snapshot["parent_id"],
            "date_created": snapshot["created"],
            "date_updated": snapshot["updated"],
            "date_synced": snapshot["synced"],
            "file_size": snapshot["file_size"],
            "blocks_size": snapshot["blocks_size"],
        }
        if expand:
            info["file_url"] = f"{self.base}/download/file/{snapshot['id']}"
            info["blocks"] = [{"hash": h, "url": f"{self.base}/download/block/{h}"} for h in snapshot["blocks"]]
        return info

    def project_info(self, project):
        snapshots = self.store.db["snapshots"]
        info = {
            "id": project["id"],
            "username": USERNAME,
            "author_name": USERNAME,
            "slug": project["id"],
            "name": project["name"],
            "details": "",
            "date_created": project["created"],
            "date_updated": project["updated"],
            "size": sum(snapshots[s]["file_size"] + snapshots[s]["blocks_size"]
                        for s in project["snapshots"] if s in snapshots),
        }
        if project["head"] in snapshots:
            info["head"] = self.snapshot_info(snapshots[project["head"]])
        synced = [s for s in project["snapshots"] if s in snapshots and snapshots[s]["synced"] > 0]
        if synced:
            info["latest_synced_snapshot_id"] = synced[-1]
        return info

    def upload_urls(self, kind, key):
        return {
            "id": key,
            "url": f"{self.base}/upload/{kind}/{key}",
            "success": f"{self.base}/upload/{kind}/{key}/success",
            "fail": f"{self.base}/upload/{kind}/{key}/fail",
        }

    def sync_state(self, snapshot):
        return {
            "file": self.upload_urls("file", snapshot["id"]),
            "mixdown": self.upload_urls("mixdown", snapshot["id"]),
            "blocks": [self.upload_urls("block", h) for h in snapshot["blocks"] if not self.store.has_block(h)],
        }

    # --- Endpoints ---

    def create_snapshot(self, project, form):
        """Shared by POST /project and POST /project/{id}/snapshot."""
        store = self.store
        snapshot = {
            "id": uuid.uuid4().hex,
            "project_id": project["id"],
            "parent_id": project["head"] or "",
            "created": now(),
            "updated": now(),
            "synced": 0,
            "file_size": 0,
            "blocks_size": 0,
            "blocks": [h.upper() for h in form.get("blocks", [])],
        }
        store.db["snapshots"][snapshot["id"]] = snapshot
        project["snapshots"].append(snapshot["id"])
        project["head"] = snapshot["id"]
        project["updated"] = now()
        store.save()
        return {
            "project": self.project_info(project),
            "snapshot": self.snapshot_info(snapshot),
            "sync": self.sync_state(snapshot),
        }

    def post_project(self, form):
        with self.store.lock:
            project = {
                "id": uuid.uuid4().hex,
                "name": form.get("name") or "Untitled",
                "created": now(),
                "updated": now(),
                "head": "",
                "snapshots": [],
            }
            self.store.db["projects"][project["id"]] = project
            return 200, self.create_snapshot(project, form)

    def post_snapshot(self, project_id, form):
        with self.store.lock:
            project = self.store.db["projects"].get(project_id)
            if project is None:
                return 404, {"error": "project not found"}
            # Optimistic concurrency: the client must be based on the current head
            if not form.get("force") and form.get("head_snapshot_id", "") != project["head"]:
                return 409, {"error": "project version conflict", "head": project["head"]}
            return 200, self.create_snapshot(project, form)

    def get_projects(self, query):
        page = int(query.get("page", ["1"])[0])
        per_page = int(query.get("per-page", ["20"])[0])
        term = query.get("q", [""])[0].lower()
        with self.store.lock:
            projects = sorted(self.store.db["projects"].values(), key=lambda p: p["updated"], reverse=True)
            projects = [p for p in projects if term in p["name"].lower()]
            start = (page - 1) * per_page
            items = [self.project_info(p) for p in projects[start:start + per_page]]
            return 200, {
                "items": items,
                "pagination": {
                    "total": len(projects),
                    "pages": max(1, -(-len(projects) // per_page)),
                    "page": page,
                    "size": per_page,
                },
            }

    def get_project(self, project_id):
        with self.store.lock:
            project = self.store.db["projects"].get(project_id)
            if project is None:
                return 404, {"error": "project not found"}
            return 200, self.project_info(project)

    def get_snapshot(self, project_id, snapshot_id):
        with self.store.lock:
            snapshot = self.store.db["snapshots"].get(snapshot_id)
            if snapshot is None or snapshot["project_id"] != project_id:
                return 404, {"error": "snapshot not found"}
            return 200, self.snapshot_info(snapshot, expand=True)

    def get_sync(self, project_id, snapshot_id):
        with self.store.lock:
            snapshot = self.store.db["snapshots"].get(snapshot_id)
            if snapshot is None or snapshot["project_id"] != project_id:
                return 404, {"error": "snapshot not found"}
            return 200, self.sync_state(snapshot)

    def post_sync(self, project_id, snapshot_id):
        with self.store.lock:
            snapshot = self.store.db["snapshots"].get(snapshot_id)
            if snapshot is None or snapshot["project_id"] != project_id:
                return 404, {"error": "snapshot not found"}
            missing = [h for h in snapshot["blocks"] if not self.store.has_block(h)]
            # A snapshot only becomes openable once complete. Not 409/422: the
            # client reports those as a version conflict
            if missing or not (self.store.files_dir / snapshot_id).exists():
                return 400, {"error": "snapshot incomplete", "missing_blocks": len(missing)}
            snapshot["synced"] = now()
            snapshot["updated"] = now()
            snapshot["blocks_size"] = sum(self.store.block_path(h).stat().st_size for h in set(snapshot["blocks"]))
            self.store.save()
            return 200, {}

    def delete_snapshot(self, project_id, snapshot_id):
        with self.store.lock:
            project = self.store.db["projects"].get(project_id)
            snapshot = self.store.db["snapshots"].pop(snapshot_id, None)
            if project is None or snapshot is None:
                return 404, {"error": "not found"}
            project["snapshots"] = [s for s in project["snapshots"] if s != snapshot_id]
            if project["head"] == snapshot_id:
                project["head"] = snapshot["parent_id"]
            self.store.save()
            return 200, {}

    def put_upload(self, kind, key, body):
        with self.store.lock:
            if kind == "block":
                self.store.block_path(key).write_bytes(body)
            elif kind == "file":
                (self.store.files_dir / key).write_bytes(body)
                snapshot = self.store.db["snapshots"].get(key)
                if snapshot:
                    snapshot["file_size"] = len(body)
                    self.store.save()
            elif kind == "mixdown":
                (self.store.mixdowns_dir / key).write_bytes(body)
            else:
                return 404, None
            return 200, None

    def get_download(self, kind, key):
        path = self.store.block_path(key) if kind == "block" else self.store.files_dir / key if kind == "file" else None
        if path is None or not path.exists():
            return 404, None
        return 200, path.read_bytes()


ROUTES = [
    ("POST", r"/auth/token", "auth_token"),
    ("GET", r"/me", "me"),
    ("GET", r"/project", "projects"),
    ("POST", r"/project", "create_project"),
    ("GET", r"/project/(?P<pid>\w+)", "project"),
    ("POST", r"/project/(?P<pid>\w+)/snapshot", "create_snapshot"),
    ("GET", r"/project/(?P<pid>\w+)/snapshot/(?P<sid>\w+)", "snapshot"),
    ("DELETE", r"/project/(?P<pid>\w+)/snapshot/(?P<sid>\w+)", "delete_snapshot"),
    ("GET", r"/project/(?P<pid>\w+)/snapshot/(?P<sid>\w+)/sync", "get_sync"),
    ("POST", r"/project/(?P<pid>\w+)/snapshot/(?P<sid>\w+)/sync", "post_sync"),
    ("POST", r"/project/(?P<pid>\w+)/network-stats", "ok"),
    ("PUT", r"/upload/(?P<kind>\w+)/(?P<key>\w+)", "upload"),
    ("POST", r"/upload/(?P<kind>\w+)/(?P<key>\w+)/(success|fail)", "ok"),
    ("GET", r"/download/(?P<kind>\w+)/(?P<key>\w+)", "download"),
    ("GET", r"/audacity/task/pending", "task_poll"),
]


def make_handler(api: Api, verbose: bool):
    class Handler(BaseHTTPRequestHandler):
        protocol_version = "HTTP/1.1"

        def log_message(self, fmt, *args):
            if verbose:
                super().log_message(fmt, *args)

        def _body(self):
            length = int(self.headers.get("Content-Length", 0))
            return self.rfile.read(length) if length else b""

        def _json_body(self):
            body = self._body()
            return json.loads(body) if body else {}

        def _send(self, code, payload):
            if payload is None:
                data, ctype = b"", "application/json"
            elif isinstance(payload, bytes):
                data, ctype = payload, "application/octet-stream"
            else:
                data, ctype = json.dumps(payload).encode(), "application/json"
            self.send_response(code)
            self.send_header("Content-Type", ctype)
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def _dispatch(self, method):
            url = urlsplit(self.path)
            for route_method, pattern, name in ROUTES:
                if route_method != method:
                    continue
                match = re.fullmatch(pattern, url.path)
                if match:
                    self._send(*self._handle(name, match.groupdict(), parse_qs(url.query)))
                    return
            self._body()
            self._send(404, {"error": f"no route for {method} {url.path}"})

        def _handle(self, name, params, query):
            if name == "auth_token":
                self._body()
                return 200, {"token_type": "Bearer", "access_token": "local", "expires_in": 365 * 24 * 3600,
                             "refresh_token": "local"}
            if name == "me":
                return 200, {"id": "1", "username": USERNAME, "avatar": "", "profile": {"name": "Local user"}}
            if name == "projects":
                return api.get_projects(query)
            if name == "create_project":
                return api.post_project(self._json_body())
            if name == "project":
                return api.get_project(params["pid"])
            if name == "create_snapshot":
                return api.post_snapshot(params["pid"], self._json_body())
            if name == "snapshot":
                return api.get_snapshot(params["pid"], params["sid"])
            if name == "delete_snapshot":
                return api.delete_snapshot(params["pid"], params["sid"])
            if name == "get_sync":
                return api.get_sync(params["pid"], params["sid"])
            if name == "post_sync":
                self._body()
                return api.post_sync(params["pid"], params["sid"])
            if name == "upload":
                return api.put_upload(params["kind"], params["key"], self._body())
            if name == "download":
                return api.get_download(params["kind"], params["key"])
            if name == "task_poll":
                return 200, {"polling": {"interval": 3600, "stop": True}, "tasks": []}
            self._body()
            return 200, {}

        def do_GET(self):
            self._dispatch("GET")

        def do_POST(self):
            self._dispatch("POST")

        def do_PUT(self):
            self._dispatch("PUT")

        def do_DELETE(self):
            self._dispatch("DELETE")

    return Handler


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--port", type=int, default=8090)
    parser.add_argument("--data-dir", type=Path, default=default_data_dir())
    parser.add_argument("--verbose", action="store_true", help="log every request")
    args = parser.parse_args()

    base_url = f"http://127.0.0.1:{args.port}"
    api = Api(Store(args.data_dir), base_url)
    # Loopback only: there is no authentication
    server = ThreadingHTTPServer(("127.0.0.1", args.port), make_handler(api, args.verbose))
    print(f"Multi-instance server on {base_url}, data in {args.data_dir}", flush=True)
    server.serve_forever()


if __name__ == "__main__":
    main()
