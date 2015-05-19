import Bootstrap.Html exposing (..)
import Graphics.Element exposing (show)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,on,targetValue)
import Html.Shorthand exposing (..)
import Http
import Json.Decode as Json exposing ((:=), string)
--import Json.Encode exposing (..)
import Signal exposing ((<~), (~))
import Task exposing (Task, andThen)
import Time exposing (second)
import StartApp

{-
===================
Types
===================
-}
type alias ChatMessage = (String, String)

{-
=============================
MailBoxes
=============================
-}
mbUpdateView : Signal.Mailbox (List ChatMessage)
mbUpdateView = Signal.mailbox []

mbSubmitButtonClicked : Signal.Mailbox (String, String)
mbSubmitButtonClicked = Signal.mailbox ("", "")

{-
===================
Common functions
===================
-}
(:~) a b = attribute a b

zip = List.map2 (,)

{-
===================
Construct View
===================
-}
setup : List Html
setup =
    let
        urlBase = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/"
        css = urlBase ++ "css/bootstrap.min.css"
        theme = urlBase ++ "css/bootstrap-theme.min.css"
        js = urlBase ++ "js/bootstrap.min.js"
        jQuery = "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"
        includeCss url = node "link" [ attribute "rel" "stylesheet", attribute "href" url] []
        includeJs url = node "script" [ attribute "src" url ] []
    in
        [ includeCss css
        , includeCss theme
        , includeJs jQuery
        , includeJs js
        ]

messageLog : List (String, String) -> Html
messageLog msgs =
    let toTable = List.map (\(name, msg) -> tr[][td [ "width" :~ "15%" ][text name], td [ "width" :~ "75%" ][text msg]] )
    in table [ "width" :~ "100%" ]
        [ thead [] []
        , tbody [] <| toTable msgs
        ]

view : Signal.Address String -> Signal.Address String -> String -> String -> List ChatMessage -> Html
view address1 address2 name message msgs =
   div [ "class" :~ "container" ]
     (setup ++
       [ div [ class "jumbotron" ]
             [ h1 [] [ text "Welcome to the Chat Room!" ] ]
       , row_
         [ colXs_ 8
            [ div []
              [ input [ value name
                      , "placeholder" :~ "your name"
                      , "size" :~ "20"
                      , on "input" targetValue (Signal.message address1) ] []
              , input [ value message
                      , "placeholder" :~ "something to say"
                      , "size" :~ "40"
                      , on "input" targetValue (Signal.message address2) ] []
              , button [ "class" :~ "btn btn-primary"
                       , on "click"
                         targetValue
                         (\_->Signal.message mbSubmitButtonClicked.address (name, message))  ] [ text "abc" ]
              ]
            , messageLog msgs
            ]
         ,  colXs_ 4 [ h3 [] [ text "Users" ] ]
       ]])

main : Signal Html
main =
  let
    mb1 = Signal.mailbox ""
    mb2 = Signal.mailbox ""
  in
    view mb1.address mb2.address <~ mb1.signal ~ mb2.signal ~ mbUpdateView.signal

{-
=============================
Tasks for Chat API Call
=============================
-}
messageDecoder = Json.object2 (,) ("name" := string) ("message" := string)

getChatMessages : Task Http.Error (List (String, String))
getChatMessages =
    let
      messagesDecoder = Json.list messageDecoder
    in
      Http.get messagesDecoder ("/api/messages.json")

composeLogLines : List (String, String) -> Task Http.Error (List (String, String))
composeLogLines msgList =
    Task.succeed msgList

updateView = Signal.send mbUpdateView.address

fetchChatMessages : Task Http.Error ()
fetchChatMessages =
    getChatMessages
    `andThen` \chatMessages ->
    composeLogLines chatMessages
    `andThen`
    updateView

port p1 : Signal (Task Http.Error ())
port p1 = (\_ -> fetchChatMessages) <~ (Time.every <| 3 * second)

composeChatMessage name msg =
    "{ \"name\":\""++name++"\", \"message\":\""++msg++"\"}"

postChatMessage : ChatMessage -> Task Http.Error (String, String)
postChatMessage (name, msg) =
  let request =
        { verb = "POST"
        , headers = [("Content-Type", "application/json")]
        , url = "/api/messages.json"
        , body = Http.string <| composeChatMessage name msg
        }
  in
     Http.fromJson
       messageDecoder
       (Http.send Http.defaultSettings request)

port p2 : Signal (Task Http.Error ())
port p2 =
    (\chatMessage ->
      postChatMessage chatMessage
      `andThen`
      (\_->fetchChatMessages))
    <~ mbSubmitButtonClicked.signal
