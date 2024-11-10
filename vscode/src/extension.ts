import * as path from "path";
import {
  workspace,
  ExtensionContext,
  TextDocument,
  WorkspaceFolder,
  Uri,
  commands,
  env,
} from "vscode";

import {
  ExecutableOptions,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";
import {
  CodingStylePDFUrl,
  OpenCodingStyleDocCommandName,
  RestartServerCommandName,
  StartServerCommandName,
  StopServerCommandName,
} from "./constants";

// Let's have one instance of the Language Server per workspace
const clients: Map<string, LanguageClient | null> = new Map();

// This is the entrypoint to our extension
export async function activate(context: ExtensionContext) {
  workspace.onDidOpenTextDocument(
    async (document: TextDocument) => await activeServer(context, document)
  );
  workspace.textDocuments.forEach(
    async (document: TextDocument) => await activeServer(context, document)
  );

  // Stop the server from any workspace folders that are removed.
  workspace.onDidChangeWorkspaceFolders((event) => {
    for (const folder of event.removed) {
      const client = clients.get(folder.uri.toString());
      if (client) {
        const uri = folder.uri.toString();
        clients.delete(uri);
        client.stop();
      }
    }
  });

  const restartCmd = commands.registerCommand(
    RestartServerCommandName,
    async () => {
      for (const langClient of clients.values()) {
        langClient?.info("Stopping Lambdananas");
        await langClient?.stop();
        langClient?.info("Restarting Lambdananas");
        langClient?.start();
      }
    }
  );

  const stopCmd = commands.registerCommand(StopServerCommandName, async () => {
    for (const langClient of clients.values()) {
      langClient?.info("Stopping Lambdananas");
      await langClient?.stop();
      langClient?.info("Lambdananas stopped");
    }
  });

  const startCmd = commands.registerCommand(
    StartServerCommandName,
    async () => {
      for (const langClient of clients.values()) {
        langClient?.info("Starting Lambdananas");
        langClient?.start();
        langClient?.info("Server Lambdananas");
      }
    }
  );

  const codingStyleDoc = commands.registerCommand(
    OpenCodingStyleDocCommandName,
    async () => {
      env.openExternal(Uri.parse(CodingStylePDFUrl));
    }
  );

  context.subscriptions.push(startCmd);
  context.subscriptions.push(stopCmd);
  context.subscriptions.push(restartCmd);
  context.subscriptions.push(codingStyleDoc);
}

async function activeServer(context: ExtensionContext, document: TextDocument) {
  // We are only interested in Haskell files.
  if (
    document.languageId !== "haskell" ||
    (document.uri.scheme !== "file" && document.uri.scheme !== "untitled")
  ) {
    return;
  }

  const uri = document.uri;
  const folder = workspace.getWorkspaceFolder(uri);

  activateForFolder(context, uri, folder);
}

export function activateForFolder(
  context: ExtensionContext,
  uri: Uri,
  folder?: WorkspaceFolder
) {
  const clientKey = folder ? folder.uri.toString() : uri.toString();
  if (clients.has(clientKey)) {
    return;
  }
  // Set the key to null to prevent multiple servers being launched at once
  clients.set(clientKey, null);

  const exeOptions: ExecutableOptions = {
    cwd: folder ? folder.uri.fsPath : path.dirname(uri.fsPath),
  };
  const executable = { command: "lambdananas-language-server", exeOptions };
  const serverOptions: ServerOptions = {
    run: executable,
    debug: executable,
    transport: TransportKind.stdio,
  };

  const clientName = "Lambdananas" + (folder ? ` (${folder.name})` : "");
  const pattern = folder ? `${folder.uri.fsPath}/**/*` : "**/*";
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "haskell", pattern }],
    synchronize: {},
    diagnosticCollectionName: clientName,
    // Launch the server in the directory of the workspace folder.
    workspaceFolder: folder,
  };

  // Create the language client and start the client.
  const client = new LanguageClient(
    "haskell",
    clientName,
    serverOptions,
    clientOptions
  );
  client.registerProposedFeatures();
  client.start().catch((e) => client.error(e));
  clients.set(clientKey, client);
}

export async function deactivate() {
  const promises: Thenable<void>[] = [];
  for (const client of clients.values()) {
    if (client) {
      promises.push(client.stop());
    }
  }
  await Promise.all(promises);
}
