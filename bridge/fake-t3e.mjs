#!/usr/bin/env node

// Deterministic protocol-v1 bridge used by ERT integration tests and t3-code-demo.
// It deliberately models only the normalized stdio contract, not raw T3 RPC.

import readline from "node:readline";

const subscriptions = new Map();
let generation = 0;

const write = (record) => process.stdout.write(`${JSON.stringify(record)}\n`);

const shellPayload = {
  projects: [
    {
      id: "project-resolved",
      name: "arthur/resolved-rs",
      root: "/work/resolved-rs",
      threads: [
        {
          id: "thread-doh",
          title: "add doh fallback resolver",
          status: "waiting-approval",
          provider: "codex",
          model: "gpt-5.3",
          worktree: "wt:doh",
          path: "/work/resolved-rs/.worktrees/doh",
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
          path: "/work/resolved-rs/.worktrees/dns-flake",
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
          path: "/work/resolved-rs/.worktrees/doh",
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
    title: "add doh fallback resolver",
    status: "waiting-approval",
    provider: "codex",
    model: "gpt-5.3",
    worktree: "wt:doh",
    worktreePath: "/work/resolved-rs/.worktrees/doh",
    runtimeMode: "full-access",
    interactionMode: "default",
    activeRunId: null,
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
      id: "item-assistant-1",
      type: "assistant_message",
      status: "completed",
      label: "Assistant",
      title: null,
      text: "Implemented the fallback resolver and added focused coverage.",
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
threadPayload.pendingRequestCount = 1;
threadPayload.attention = threadPayload.items.filter((item) => item.type === "approval_request");

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
