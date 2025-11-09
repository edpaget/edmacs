#!/usr/bin/env python3
"""Approval hook for claude-repl.

This script acts as a bridge between Claude Code and the Emacs approval system.
It forwards tool use requests to the approval server via Unix socket and returns
the approval decision.
"""

import socket
import sys
import json


def main():
    if len(sys.argv) < 2:
        sys.stderr.write("Usage: approval-hook.py <socket-path>\n")
        sys.exit(1)

    socket_path = sys.argv[1]

    try:
        # Read request from stdin
        request_data = sys.stdin.read()

        # Connect to approval server
        sock = socket.socket(socket.AF_UNIX)
        sock.connect(socket_path)

        # Send request
        sock.sendall(request_data.encode())

        # Read response in chunks until we have complete JSON
        chunks = []
        while True:
            chunk = sock.recv(4096)
            if not chunk:
                # Connection closed
                break

            chunks.append(chunk)

            # Try to parse as complete JSON
            try:
                json.loads(b''.join(chunks))
                # Successfully parsed - we have the complete message
                break
            except json.JSONDecodeError:
                # Incomplete JSON, keep reading
                continue

        sock.close()

        # Write response to stdout
        response = b''.join(chunks).decode()
        sys.stdout.write(response)
        sys.stdout.flush()

    except Exception as e:
        sys.stderr.write(f"Error in approval hook: {e}\n")
        sys.exit(1)


if __name__ == "__main__":
    main()
