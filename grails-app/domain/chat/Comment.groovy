package chat
import grails.rest.*

class Comment {
    Date dateCreated
    String saying

    static constraints = {
        saying notNull:false
    }
}
