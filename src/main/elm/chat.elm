import Bootstrap.Html exposing (..)
import Graphics.Element exposing (show)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Shorthand exposing (..)
import Http
import Json.Decode as Json exposing ((:=), string)
import Signal exposing ((<~))
import Task exposing (Task, andThen)
import Time exposing (second)
import StartApp

type alias ChatMessage = (String, String)

mb : Signal.Mailbox (List ChatMessage)
mb = Signal.mailbox []

(:~) a b = attribute a b

setup : List Html
setup =
    let
        urlBase = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/"
        css = urlBase ++ "css/bootstrap.min.css"
        theme = urlBase ++ "css/bootstrap-theme.min.css"
        js = urlBase ++ "js/bootstrap.min.js"
        jQueryUrl = "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"
        includeCss url = node "link" [ attribute "rel" "stylesheet", attribute "href" url] []
        includeJs url = node "script" [ attribute "src" url ] []
    in
        [ includeCss css
        , includeCss theme
        , includeJs jQueryUrl
        , includeJs js
        ]

messageLog : List (String, String) -> Html
messageLog msgs =
    let toTable = List.map (\(name, msg) -> tr[][td [][text name], td [][text msg]] )
    in table [ "width" :~ "100%" ]
        [ thead [] [ tr [][ th [ "width" :~ "10"][ text "name" ], th [][] ] ]
        , tbody [] <| toTable msgs
        ]

view : List ChatMessage -> Html
view msgs =
   div [ "class" :~ "container" ]
     (setup ++
       [ div [ class "jumbotron" ]
             [ h1 [] [ text "Welcome to the Chat Room!" ] ]
       , row_
         [ colXs_ 8
            [ messageLog msgs
            , Html.form [ "class" :~ "inline"]
                        [ input [ "placeholder" :~ "your name"
                                , "size" :~ "8" ] []
                        , input [ "placeholder" :~ "something to say"
                                , "size" :~ "40" ] []
                        , button [ "class" :~ "btn btn-primary"] [ text "abc" ]
                        ]
            ]
         , colXs_ 4 [ h3 [] [ text "Users" ] ]
         ]
       ])

main : Signal Html
main = view <~ mb.signal

getChatMessages : Task Http.Error (List (Int, String))
getChatMessages =
    let
      authorDecoder = Json.object1 identity ("id" := Json.int)
      messageDecoder = Json.object2 (,) ("author" := authorDecoder) ("message" := string)
      messagesDecoder = Json.list messageDecoder
    in
      Http.get messagesDecoder ("/api/messages.json")

getUserNames : List (Int, String) -> Task Http.Error (List String)
getUserNames userNames =
    let
      userIds = List.map fst userNames
      userNameDecoder = Json.object1 identity ("name" := string)
    in
      List.map (\userId -> (Http.get userNameDecoder ("/api/users/"++toString(userId)++".json"))) userIds |> Task.sequence

composeLogLines : List String -> List (Int, String) -> Task Http.Error (List (String, String))
composeLogLines userNames msgList =
    Task.succeed <| List.map2 (,) (List.map snd msgList) userNames

port asyncFetchChatMessagesTask : Signal (Task Http.Error ())
port asyncFetchChatMessagesTask =
    (\_ -> (getChatMessages
      `andThen` (\chatMessages ->
      getUserNames chatMessages
      `andThen` (\userNames ->
      composeLogLines userNames chatMessages)))
      `andThen`
      Signal.send mb.address)
    <~ (Time.every <| 3 * second)

