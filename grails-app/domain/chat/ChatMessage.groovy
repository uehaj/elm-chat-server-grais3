package chat
import grails.rest.*

class ChatMessage {
    Date dateCreated
    User author
    String message

    static constraints = {
        author()
        message()
    }
}
