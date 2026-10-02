#!/usr/bin/env node

// Deterministic protocol-v1 bridge used by ERT integration tests and t3-code-demo.
// It deliberately models only the normalized stdio contract, not raw T3 RPC.

import readline from "node:readline";

const subscriptions = new Map();
// Lets interactive smoke tests point the fake project at a real directory.
const root = process.env.T3E_FAKE_ROOT ?? "/work/resolved-rs";
let generation = 0;

const write = (record) => process.stdout.write(`${JSON.stringify(record)}\n`);

const shellPayload = {
  projects: [
    {
      id: "project-resolved",
      name: "arthur/resolved-rs",
      root: root,
      threads: [
        {
          id: "thread-doh",
          title: "add doh fallback resolver",
          status: "waiting-approval",
          provider: "codex",
          model: "gpt-5.3",
          worktree: "wt:doh",
          path: `${root}/.worktrees/doh`,
          branch: "feature/doh",
          updatedAt: "2026-06-20T00:05:00.000Z",
          pinned: true,
          snoozedUntil: null,
          unread: false,
          settled: false,
          parentThreadId: null,
          relationshipToParent: null,
          additions: 221,
          deletions: 1,
        },
        {
          id: "thread-dns-flake",
          title: "fix flaky dns test under load",
          status: "idle",
          provider: "pi",
          model: "claude-opus",
          worktree: "wt:dns-flake",
          path: `${root}/.worktrees/dns-flake`,
          branch: "fix/dns-flake",
          updatedAt: "2026-06-19T10:00:00.000Z",
          pinned: false,
          snoozedUntil: null,
          unread: true,
          settled: true,
          parentThreadId: null,
          relationshipToParent: null,
          additions: 142,
          deletions: 38,
        },
        {
          id: "thread-doh-agent",
          title: "research cloudflare endpoint behavior",
          status: "idle",
          provider: "pi",
          model: "gpt-5.6-sol",
          worktree: "wt:doh",
          path: `${root}/.worktrees/doh`,
          branch: "feature/doh",
          updatedAt: "2026-06-20T00:04:00.000Z",
          pinned: false,
          snoozedUntil: null,
          unread: false,
          settled: false,
          parentThreadId: "thread-doh",
          relationshipToParent: "subagent",
          additions: 0,
          deletions: 0,
        },
      ],
    },
  ],
  truncated: false,
};

const threadPayload = {
  thread: {
    id: "thread-doh",
    projectId: "project-resolved",
    title: "add doh fallback resolver",
    status: "waiting-approval",
    provider: "codex",
    model: "gpt-5.3",
    modelSelection: {
      instanceId: "codex",
      model: "gpt-5.3",
      options: [{ id: "reasoningEffort", value: "medium" }],
    },
    hasStartedSession: true,
    worktree: "wt:doh",
    worktreePath: `${root}/.worktrees/doh`,
    branch: "feature/doh",
    runtimeMode: "full-access",
    interactionMode: "default",
    activeRunId: "run-doh-1",
    tokenUsage: { usedTokens: 91_000, maxTokens: 128_000 },
  },
  items: [
    {
      id: "item-user-1",
      type: "user_message",
      status: "completed",
      label: "You",
      title: null,
      text: "Add a DNS-over-HTTPS fallback resolver and keep the existing UDP path fast.",
      detail: null,
      streaming: false,
    },
    {
      id: "item-reasoning-1",
      type: "reasoning",
      status: "completed",
      label: "Thinking",
      title: "Resolver design",
      text: "I will isolate fallback policy from transport selection.",
      detail: null,
      streaming: false,
    },
    {
      id: "item-command-1",
      type: "command_execution",
      status: "completed",
      label: "Command",
      title: "Run focused tests",
      text: "cargo test resolver::doh",
      detail: "test resolver::doh ... ok\n\ntest result: ok. 1 passed; 0 failed",
      streaming: false,
    },
    {
      id: "item-edit-1",
      type: "file_change",
      status: "completed",
      label: "File change",
      title: null,
      text: "src/resolver/doh.rs",
      path: "src/resolver/doh.rs",
      detail: "@@ -1,3 +1,4 @@\n use std::net::IpAddr;\n+use reqwest::Client;\n-pub fn resolve() {}\n+pub fn resolve(client: &Client) {}",
      streaming: false,
    },
    {
      id: "item-assistant-1",
      type: "assistant_message",
      status: "completed",
      label: "Assistant",
      title: null,
      text: "Implemented the fallback resolver in `src/resolver/doh.rs:1` and added **focused** coverage.\n\n```rust\npub fn resolve(client: &Client) {}\n```",
      detail: null,
      streaming: false,
    },
    {
      id: "item-approval-1",
      type: "approval_request",
      status: "waiting",
      label: "Approval needed",
      title: null,
      text: "Allow the integration test to contact the local fixture server?",
      detail: null,
      streaming: false,
      actionId: "request-approval-1",
    },
  ],
  truncated: false,
};

// Mirror optional section metadata from the version-matched production bridge.
threadPayload.items = threadPayload.items.map((item) => ({
  ...item,
  runId: "run-doh-1",
  runStatus: "waiting",
  runOrdinal: 1,
  presentation: "message",
}));
threadPayload.items.push({
  id: "item-input-1",
  type: "user_input_request",
  status: "waiting",
  label: "Input needed",
  title: null,
  text: "Transport: Which fallback should run first?",
  detail: null,
  streaming: false,
  actionId: "request-input-1",
  runId: "run-doh-1",
  runStatus: "waiting",
  runOrdinal: 1,
  presentation: "message",
  questions: [
    {
      id: "transport",
      header: "Transport",
      question: "Which fallback should run first?",
      multiSelect: false,
      allowCustomAnswer: true,
      options: [
        { label: "DoH", description: "HTTPS to a public resolver", value: "doh" },
        { label: "TCP", description: "Plain DNS over TCP", value: "tcp" },
      ],
    },
  ],
});
threadPayload.pendingRequestCount = 2;
threadPayload.hasOlderHistory = true;
threadPayload.queued = [
  { runId: "run-doh-2", position: 1, held: false, text: "Afterwards, document the fallback." },
];

const olderItems = [
  {
    id: "item-old-user",
    type: "user_message",
    status: "completed",
    label: "You",
    title: null,
    text: "Sketch how resolution currently works.",
    detail: null,
    streaming: false,
    runId: "run-doh-0",
    runStatus: "completed",
    runOrdinal: 0,
    presentation: "message",
  },
  {
    id: "item-old-answer",
    type: "assistant_message",
    status: "completed",
    label: "Assistant",
    title: null,
    text: "Resolution goes through a single UDP transport today.",
    detail: null,
    streaming: false,
    runId: "run-doh-0",
    runStatus: "completed",
    runOrdinal: 0,
    presentation: "message",
  },
];

const results = {
  "model.catalog": {
    providers: [
      {
        instanceId: "codex",
        name: "Codex",
        available: true,
        models: [
          {
            slug: "gpt-5.3",
            name: "GPT-5.3",
            options: [
              {
                id: "reasoningEffort",
                label: "Reasoning effort",
                type: "select",
                choices: [
                  { id: "low", label: "Low", isDefault: false },
                  { id: "medium", label: "Medium", isDefault: true },
                  { id: "high", label: "High", isDefault: false },
                ],
              },
            ],
          },
        ],
      },
    ],
    truncated: false,
  },
  "thread.history": { items: olderItems, hasMore: false },
  "thread.search": {
    matches: [
      {
        threadId: "thread-dns-flake",
        projectId: "project-resolved",
        source: "message",
        snippet: "flaky dns test under load",
      },
    ],
  },
  "threads.archived": {
    threads: [
      {
        id: "thread-archived",
        title: "old resolver experiment",
        projectId: "project-resolved",
        projectName: "arthur/resolved-rs",
        provider: "codex",
        model: "gpt-5.3",
        updatedAt: "2026-05-01T00:00:00.000Z",
      },
    ],
  },
  "provider.commands": {
    slashCommands: [{ name: "review", description: "Review the current changes" }],
    skills: [{ name: "deploy", description: "Ship a release", userInvocationOnly: false }],
  },
  "project.searchEntries": {
    entries: [
      { path: "src/resolver/doh.rs", kind: "file" },
      { path: "src/resolver/udp.rs", kind: "file" },
    ],
    truncated: false,
  },
};
threadPayload.attention = threadPayload.items.filter((item) =>
  ["approval_request", "user_input_request"].includes(item.type),
);

const subscriptionPayload = (subscription) => {
  if (subscription.stream === "shell") return shellPayload;
  if (subscription.stream === "thread" && subscription.identity === "thread-doh") {
    return threadPayload;
  }
  return {
    thread: null,
    items: [],
    truncated: false,
    error: `unknown thread: ${String(subscription.identity)}`,
  };
};

const sendSubscription = (subscriptionId) => {
  const subscription = subscriptions.get(subscriptionId);
  if (!subscription) return;
  const snapshotSequence = (subscription.resumeSequence ?? 9) + 1;
  write({
    kind: "snapshot",
    subscriptionId,
    generation,
    sequence: snapshotSequence,
    payload: subscriptionPayload(subscription),
  });
  write({
    kind: "synchronized",
    subscriptionId,
    generation,
    sequence: snapshotSequence,
  });
  write({
    kind: "event",
    subscriptionId,
    generation,
    sequence: snapshotSequence + 1,
    payload: subscriptionPayload(subscription),
  });
};

const rl = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
rl.on("line", (line) => {
  let message;
  try {
    message = JSON.parse(line);
  } catch {
    write({ kind: "fatal", message: "malformed client JSON" });
    return;
  }

  switch (message.kind) {
    case "hello":
      generation = message.environment?.generation ?? 0;
      write({
        kind: "ready",
        protocolVersion: Number(process.env.T3E_FAKE_PROTOCOL_VERSION ?? 1),
        bridgeVersion: "0.1.0-fake",
        pinnedT3Version: "fake",
        serverVersion: "fake",
        environmentId: message.environment?.id,
        capabilities: {
          shell: true,
          threads: true,
          mutations: true,
          threadSections: true,
          modelSelection: true,
          threadLifecycle: true,
          composerCompletion: true,
          terminal: false,
        },
      });
      break;
    case "request":
      if (message.operation === "never") {
        break;
      }
      if (message.operation === "server.getConfig") {
        write({
          kind: "response",
          id: message.id,
          result: {
            capabilities: { shell: true, threads: true, mutations: true },
            serverVersion: "fake",
          },
        });
      } else if (message.operation in results) {
        write({ kind: "response", id: message.id, result: results[message.operation] });
      } else if (message.operation === "thread.create") {
        write({ kind: "response", id: message.id, result: { threadId: "thread-doh" } });
      } else if (message.operation === "thread.fork") {
        write({
          kind: "response",
          id: message.id,
          result: { threadId: message.input?.targetThreadId },
        });
      } else if (message.operation.startsWith("thread.")) {
        write({
          kind: "response",
          id: message.id,
          result: { accepted: true, operation: message.operation, input: message.input },
        });
      } else {
        write({
          kind: "response",
          id: message.id,
          error: { code: "unsupported-operation", message: message.operation },
        });
      }
      break;
    case "cancel":
      break;
    case "subscribe":
      subscriptions.set(message.subscriptionId, message);
      sendSubscription(message.subscriptionId);
      break;
    case "unsubscribe":
      subscriptions.delete(message.subscriptionId);
      break;
    default:
      write({ kind: "fatal", message: `unknown client message: ${message.kind}` });
  }
});
