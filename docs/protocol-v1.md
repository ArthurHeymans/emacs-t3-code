# t3e NDJSON protocol v1

`t3-code.el` communicates with a version-matched companion process over UTF-8, newline-delimited JSON on stdin/stdout. Raw Effect RPC frames and raw T3 domain schemas are not part of this protocol.

## Framing and limits

- Each record is one JSON object followed by `\n`.
- The default maximum input line is 1 MiB. An oversized line terminates the bridge connection.
- Unknown object fields are ignored.
- Unknown record kinds are diagnosed and ignored unless the bridge sends `fatal`.
- Bridge stderr is diagnostic-only, redacted by the bridge, and bounded by the client.
- Protocol stdout must contain no logging or non-protocol readiness text.

## Handshake

Client:

```json
{"kind":"hello","protocolVersion":1,"client":{"name":"t3-code.el","version":"0.1.0"},"environment":{"id":"local","endpoint":"http://127.0.0.1:3773","generation":1}}
```

Bridge:

```json
{"kind":"ready","protocolVersion":1,"bridgeVersion":"…","pinnedT3Version":"…","serverVersion":"…","environmentId":"local","capabilities":{}}
```

The client refuses a different protocol version or environment ID. Commands and subscriptions queue until `ready`.

## Requests

```json
{"kind":"request","id":"request-1","operation":"server.getConfig","input":{}}
{"kind":"response","id":"request-1","result":{}}
{"kind":"response","id":"request-1","error":{"code":"…","message":"…","details":{}}}
{"kind":"cancel","id":"request-1"}
```

Request IDs are unique within an environment process generation. Cancellation is explicit. Mutating requests require stable semantic command IDs in their normalized input and are never retried automatically after an ambiguous disconnect.

## Subscriptions

```json
{"kind":"subscribe","subscriptionId":"shell:null","stream":"shell","identity":null,"resumeSequence":41}
{"kind":"unsubscribe","subscriptionId":"shell:null"}
```

Bridge output:

```json
{"kind":"snapshot","subscriptionId":"shell:null","generation":2,"sequence":42,"payload":{}}
{"kind":"event","subscriptionId":"shell:null","generation":2,"sequence":43,"payload":{}}
{"kind":"synchronized","subscriptionId":"shell:null","generation":2,"sequence":43}
```

A snapshot is authoritative. Events are ordered. Duplicate sequences are ignored. A gap causes the client to unsubscribe and request an authoritative snapshot without a resume sequence. Records from stale connection generations are ignored.

Subscriptions are reference counted in Emacs. Killing one view releases its reference but does not disconnect the shared environment.

## Connection state and fatal errors

```json
{"kind":"state","phase":"authenticating","retryAt":"…","message":"…"}
{"kind":"fatal","code":"protocol-mismatch","message":"…","stderrExcerpt":"…"}
```

Safe state phases include `connecting`, `authenticating`, `snapshot`, `resuming`, `ready`, `repairing`, and `retrying`. A fatal record is not retried by the protocol layer.

## Normalized shell projection

The M0/M1 shell snapshot payload is:

```json
{
  "projects": [{
    "id": "project-id",
    "name": "owner/name",
    "root": "/server/path",
    "threads": [{
      "id": "thread-id",
      "title": "…",
      "status": "running|waiting-approval|idle|failed",
      "provider": "codex",
      "model": "gpt-5.3",
      "worktree": "root-or-display-name",
      "settled": false,
      "parentThreadId": null,
      "relationshipToParent": null,
      "branch": "feature/x",
      "updatedAt": "2026-06-20T00:00:00.000Z",
      "pinned": false,
      "snoozedUntil": null,
      "unread": false,
      "additions": 1,
      "deletions": 2
    }]
  }],
  "truncated": false
}
```

This is a view model, not a serialization of T3 contracts. During M1, shell `event` records carry the same replacement projection shape as snapshots, allowing the bridge to reduce and coalesce raw server updates without moving T3 reducer schemas into Elisp. An empty `projects` array is authoritative. `unread` marks a latest run that completed after the server-tracked last visit (false when the server does not track visits). Future fields remain optional. Future semantic item kinds must have bounded generic rendering in Emacs. When truncated, newer bridges also report `omittedSettledCount`, `omittedOtherCount`, and `omittedProjectCount`; consumers must treat absent counts from older bridges as unknown, not zero.

## Normalized thread projection

A `thread` subscription uses the thread ID as its identity and emits replacement payloads:

```json
{
  "thread": {
    "id": "thread-id",
    "title": "…",
    "status": "running|waiting-approval|idle|failed",
    "provider": "codex",
    "model": "gpt-5.3",
    "modelSelection": { "instanceId": "codex", "model": "gpt-5.3" },
    "activeRunModel": null,
    "activeRunProvider": null,
    "hasStartedSession": false,
    "projectId": "project-id",
    "worktree": "root",
    "worktreePath": null,
    "branch": null,
    "runtimeMode": "full-access",
    "interactionMode": "default",
    "activeRunId": null,
    "tokenUsage": { "usedTokens": 1200, "maxTokens": 128000 }
  },
  "items": [{
    "id": "item-id",
    "type": "assistant_message",
    "status": "completed",
    "label": "Assistant",
    "title": null,
    "text": "…",
    "detail": null,
    "streaming": false,
    "actionId": null,
    "runId": "run-id",
    "runStatus": "completed",
    "runOrdinal": 1,
    "presentation": "message"
  }],
  "attention": [],
  "pendingRequestCount": 0,
  "queued": [{ "runId": "run-2", "position": 1, "held": false, "text": "…", "truncated": false }],
  "hasOlderHistory": false,
  "truncated": false
}
```

The bridge reduces raw thread events and emits complete normalized replacements. It retains at most 100 recent visible items and 256,000 UTF-8 bytes of text/detail per payload. Per-field text is capped at 32,000 characters, display labels are single-line and bounded, and final payloads are trimmed below a 700,000-byte target. Arbitrary dynamic-tool input/output is omitted because it may contain secrets. A missing/deleted thread uses `thread: null` with a bounded `error` or `deleted: true` marker.

All protocol output records have a hard 900,000-byte encoded limit, below Emacs's 1 MiB input ceiling. Shell projections retain at most 50 projects and 2,000 threads and are further byte-trimmed. Working threads and their parent chain are selected first, then other unsettled threads, then the most recently updated settled threads. `truncated: true` tells renderers that authoritative state was intentionally omitted; an omitted non-settled count can still occur if the hard bounds are exhausted. Subscription failures and unexpected normal stream completion retry after a bounded delay with a fresh authoritative snapshot; unsubscribing interrupts the retry loop.

### Optional section metadata

Bridges advertising `threadSections: true` add `runId`, `runStatus`, `runOrdinal` and `presentation` to normalized items. Group only by explicit `runId`; a user item lacking that field in T3 is associated using the run's `userMessageId`, never adjacent timeline position. Missing metadata remains valid for older bridges. Runless items render independently.

`presentation: "work"` identifies earlier assistant commentary in a completed run; the last assistant item remains `"message"`. This follows T3's last-assistant-per-run presentation, not a provider-declared final-answer flag. Running, interrupted or unclassified messages remain readable. Other tools are grouped by their existing normalized item type. No attempt-level fold metadata is currently exposed.

`pendingRequestCount` counts all pending runtime requests. `attention` separately retains up to 20 pending approval/user-input items available in the authoritative projection, even outside the 100-item timeline window, with text capped to 2,000 UTF-8 bytes per item and no detail body. The count can exceed the supplied details; consumers must not invent missing requests or assume the list is complete. Existing frame limits still apply. A `user_input_request` action ID is not an approval ID; it is answered through `thread.command` with `runtime-request.respond` and `answers` (see below).

`file_change` items carry the changed `path`. Pending `user_input_request` items carry structured `questions` (`id`, `header`, `question`, `multiSelect`, `allowCustomAnswer`, and up to 32 `options` with `label`, `description` and the exact `value` to answer with). `tokenUsage` is the provider's latest context report, or null. `queued` lists up to 50 queued follow-up messages in queue order; `held` marks a queue paused after a restart, and `truncated` marks `text` that is only a bounded preview. Clients must not offer a preview as the starting point for `queued-run.edit`, which replaces the whole message.

`hasOlderHistory` is true when items older than the first supplied item exist, either because the bridge trimmed its window or because the server's bounded snapshot has more history. A bridge advertising `threadLifecycle: true` answers `thread.history` with `{threadId, beforeItemId}` by returning `{items, hasMore}`: up to 100 chronological items immediately before `beforeItemId`, normalized and bounded like the live window. Clients page backwards from their oldest loaded item, which must come from the live window or an earlier page; the request needs a live subscription to the thread. Rows still in the server's bounded window are served directly; older ones come from the server's history pages, whose opaque cursors stay inside the bridge. An unknown `beforeItemId` fails with `history-unavailable` rather than reporting the end of history. Live replacements never include history pages, so a client that has loaded pages keeps items that later slide out of the live window.

Folds are client-local state, not lifecycle commands. Folding never stops a run, and expanding cannot recover text omitted by the bridge's bounds. Resource log/control streams are not in this protocol slice.

## Normalized thread mutations

Pairing requests both `orchestration:read` and `orchestration:operate`. Mutations are request/response operations with caller-supplied stable `commandId` values:

- `thread.send`: dispatch a user message in `auto`, `queue`, `steer`, `restart`, or `start` mode without implicitly changing thread policy.
- `thread.interrupt`: interrupt the active run, if any.
- `thread.approval.respond`: `accept`, `acceptForSession`, `decline`, or `cancel` a normalized approval `actionId`.
- `thread.runtimeMode.set`: select `approval-required`, `auto-accept-edits`, `auto`, or `full-access`.
- `thread.interactionMode.set`: select `default` or `plan`.
- `thread.settled.set`: settle or reactivate the thread.
- `thread.snooze.set`: set or clear an ISO timestamp.
- `thread.modelSelection.set`: set the instance-routed `{instanceId, model, options?}` selection for subsequent turns. The bridge selects the appropriate provider-switch or same-provider command; the server validates whether the session permits the change.

A bridge advertising `modelSelection: true` also supports `model.catalog` (a bounded request result with provider instance IDs, display names, availability, model slugs and select/boolean option descriptors). The thread projection carries the selected `modelSelection` and the active run's `activeRunProvider` / `activeRunModel` (when present). `hasStartedSession` helps clients explain providers that require a new thread for model changes. An older bridge has none of these fields or operations; clients must not offer selection without the capability.

### Thread lifecycle

A bridge advertising `threadLifecycle: true` also supports:

- `thread.create`: `{commandId, projectId, title?, text, modelSelection, runtimeMode, interactionMode, workspaceStrategy}` launches a thread with its first message and returns `{threadId}`. `workspaceStrategy` is `{type: "root"}`, `{type: "worktree", baseRef, branch?, startFromOrigin?}` or `{type: "existing_worktree", worktreePath, branch?}`. Without a title the server generates one.
- `thread.fork`: `{threadId, commandId, targetThreadId, runId}` forks at a run, or at the latest stable point when `runId` is null, and returns `{threadId}`. The client supplies `targetThreadId` so an ambiguous retry cannot create two forks.
- `thread.command`: `{command}` dispatches one allowlisted orchestration command verbatim after validating it against the version-matched command schema: `thread.archive`, `thread.unarchive`, `thread.delete`, `thread.pin`, `thread.unpin`, `thread.visit`, `thread.mark-unread`, `thread.metadata.update` (title or `regenerateTitle` only), `queued-run.cancel`, `queued-run.edit`, `queued-run.reorder`, `queued-message.promote-to-steer`, `queue.resume`, `runtime-request.respond` and `thread.user-input.dismiss`. User-input answers map question IDs to the option `value` (or custom text), with arrays for multi-select questions. Anything else fails with `unsupported-command`.
- `thread.history`: see above.
- `thread.search`: `{query, limit?}` returns `{matches: [{threadId, projectId, source, snippet}]}`.
- `threads.archived`: returns up to 500 archived threads, newest first, as `{threads: [{id, title, projectId, projectName, provider, model, updatedAt}]}`.

A bridge advertising `composerCompletion: true` supports `provider.commands` (`{instanceId}` → `{slashCommands: [{name, description}], skills: [{name, description, userInvocationOnly}]}`) and `project.searchEntries` (`{cwd, query, limit?}` → `{entries: [{path, kind}], truncated}`).

Request failures are returned as bounded response errors and do not terminate the shared bridge process. Thread creation, sends and other operations needing bridge-side state keep dedicated normalized operations; only the allowlisted commands above cross the boundary in their T3 form.

## Backpressure and coalescing

A production bridge may coalesce replaceable status and streaming-text updates by semantic identity. The client bounds pre-ready queues to `t3-code-max-queued-records`. The bridge must apply its own bounded queue and flow-control policy. It must not discard requests, responses, approvals, checkpoints, queue mutations, synchronized boundaries, or terminal bytes. Terminal flow control is outside the M0 protocol implementation.
