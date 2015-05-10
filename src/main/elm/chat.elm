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

(:~) a b = attribute a b

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

messageLog msgs =
    let toTable = List.map (\(name, msg) -> tr[][td [][text name], td [][text msg]] )
    in table [ "width" :~ "100%" ]
        [ thead [] [ tr [][ th [ "width" :~ "10"][ text "name" ], th [][] ] ]
        , tbody [] <| toTable msgs
        ]

view msgs =
   div [ "class" :~ "container" ]
     (setup ++
       [ div [ class "jumbotron" ]
             [ h1 [] [ text "Welcome to the Chat Room!" ] ]
       , row_
         [ colXs_ 8
            [ messageLog msgs
            , Html.form [ "class" :~ "inline"]
                        [ input [ "placeholder" :~ "your name" ] []
                        , input [ "placeholder" :~ "something to say"
                                , "size" :~ "40" ] []
                        , button [ "class" :~ "btn btn-primary"] [ text "abc" ]
                        ]
            ]
         , colXs_ 4 [ h3 [] [ text "Users" ] ]
         ]
       ])

type alias Comment = (String, String)

commentsDecoder =
    let comment = 
      Json.object2 (,) ("saying" := string) ("dateCreated" := string)
    in 
      Json.list comment
         
main = view <~ mb.signal

port fetchComments : Signal (Task Http.Error ())
port fetchComments =
    (\_ -> Http.get commentsDecoder ("/api/comments.json") `andThen` asyncLoopback)
    <~ (Time.every <| 3 * second)

mb : Signal.Mailbox (List Comment)
mb = Signal.mailbox []

asyncLoopback : List Comment -> Task Http.Error ()
asyncLoopback comments =
    Signal.send mb.address comments
