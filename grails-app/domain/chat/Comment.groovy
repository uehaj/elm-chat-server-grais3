package chat
import grails.rest.*

@Resource(uri='/comments', formats=['json', 'xml'])
class Comment {
    Date dateCreated
    String saying

    static constraints = {
        saying notNull:false
    }
}
