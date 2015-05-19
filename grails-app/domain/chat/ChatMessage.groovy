package chat
import grails.rest.*

class ChatMessage {
    Date dateCreated
    String name
    String message

    static constraints = {
        name()
        message()
    }
}
