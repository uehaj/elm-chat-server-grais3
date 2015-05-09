import Graphics.Element exposing (show)
import Http
import Json.Decode as Json exposing ((:=), string)
import Html exposing (Html)
import Task exposing (Task, andThen)

commentsDecoder =
    let comment = 
      Json.object2 (,) ("saying" := string) ("dateCreated" := string)
    in 
      Json.list comment
         
main = Signal.map show mb.signal

port fetchComments : Task Http.Error ()
port fetchComments =
    Http.get commentsDecoder ("/api/comments.json") `andThen` asyncLoopback

mb : Signal.Mailbox (List (String, String))
mb = Signal.mailbox []

asyncLoopback : List (String, String) -> Task Http.Error ()
asyncLoopback comments =
    Signal.send mb.address comments
