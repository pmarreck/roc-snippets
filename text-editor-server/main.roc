app [Model, server] { pf: platform "https://github.com/roc-lang/basic-webserver/releases/download/0.9.0/taU2jQuBf-wB8EJb0hAkrYLYOGacUU5Y9reiHG45IY4.tar.br" }

import pf.Stdout
import pf.SQLite3
import pf.Http exposing [Request, Response]
import pf.Utc

import "text-editor-template.html" as templateBase : Str

dbPath = "text-editor.db"

Model : {}

defaultContent =
    """
    <p>Hello Editor!</p>
    <p>Some initial <strong>bold</strong> text</p>
    <p><br /></p>
    """

server = {init: Task.ok {}, respond}

respond : Request, Model -> Task Response _
respond = \req, _ ->

    # Log request datetime, method and url
    datetime = Utc.now! |> Utc.toIso8601Str

    Stdout.line! "$(datetime) $(Http.methodToStr req.method) $(req.url)"

    when req.method is
        Post ->
            bodyStr = req.body |> Str.fromUtf8 |> Result.withDefault "<p>invalid utf8<p>"
            when updateContent bodyStr |> Task.result! is
                Err (ErrUpdatingContent msg) -> serverErr msg
                Ok {} -> serverOk (Str.toUtf8 "content updated")
                Err _ -> serverErr "unexpected error"

        _ ->
            when getContent |> Task.result! is
                Ok content -> serverOk (fullPage content)
                Err (ErrGettingContent msg)
                | Err (ErrCreatingTable msg) -> serverErr msg

serverOk : List U8 -> Task Response []
serverOk = \body ->
    Task.ok {
        status: 200,
        headers: [
            { name: "Content-Type", value: "text/html" },
        ],
        body,
    }

serverErr : Str -> Task Response []
serverErr = \msg ->
    Task.ok {
        status: 500,
        headers: [
            { name: "Content-Type", value: "text/plain" },
        ],
        body: Str.toUtf8 msg,
    }

fullPage : Str -> List U8
fullPage = \editorContent ->
    templateBase
    |> Str.replaceFirst "{{editorContent}}" editorContent
    |> Str.toUtf8

getContent : Task Str [ErrGettingContent Str, ErrCreatingTable Str]
getContent =

    # create the table if it doesn't exist
    createEditorContentTable!

    result =
        {
            path: dbPath,
            query:
            """
            SELECT id, content
            FROM EditorContent
            ORDER BY id DESC
            LIMIT 1;
            """,
            bindings: [],
        }
            |> SQLite3.execute
            |> Task.result!

    when result is
        Ok [] -> Task.ok defaultContent
        Ok [[Integer _, String latestContent]] -> Task.ok latestContent
        Ok _ -> Task.err (ErrGettingContent "unexpected values returned, expected Integer and String")
        Err (SQLError _ msg) -> Task.err (ErrGettingContent msg)

createEditorContentTable : Task {} [ErrCreatingTable Str]_
createEditorContentTable =
    {
        path: dbPath,
        query:
        """
        CREATE TABLE IF NOT EXISTS EditorContent (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            content TEXT
        );
        """,
        bindings: [],
    }
    |> SQLite3.execute
    |> Task.map \_ -> {}
    |> Task.mapErr \SQLError _ msg -> ErrCreatingTable msg

updateContent : Str -> Task {} [ErrUpdatingContent Str]_
updateContent = \content ->

    Stdout.line! "UPDATING CONTENT\n----\n$(content)\n----"

    {
        path: dbPath,
        query:
        """
        INSERT INTO EditorContent (content)
        VALUES (:content);
        """,
        bindings: [{ name: ":content", value: String content }],
    }
    |> SQLite3.execute
    |> Task.map \_ -> {}
    |> Task.mapErr \SQLError _ msg -> ErrUpdatingContent msg
