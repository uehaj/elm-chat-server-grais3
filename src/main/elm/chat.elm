import Graphics.Element exposing (show)
import Http
import Json.Decode as Json exposing ((:=), string)
import Signal exposing ((<~))
import Html exposing (Html)
import Task exposing (Task, andThen)
import Time exposing (second)

type alias Comment = (String, String)

commentsDecoder =
    let comment = 
      Json.object2 (,) ("saying" := string) ("dateCreated" := string)
    in 
      Json.list comment
         
main = show <~ mb.signal

port fetchComments : Signal (Task Http.Error ())
port fetchComments =
    (\_ -> Http.get commentsDecoder ("/api/comments.json") `andThen` asyncLoopback)
    <~ (Time.every <| 3 * second)

mb : Signal.Mailbox (List Comment)
mb = Signal.mailbox []

asyncLoopback : List Comment -> Task Http.Error ()
asyncLoopback comments =
    Signal.send mb.address comments
