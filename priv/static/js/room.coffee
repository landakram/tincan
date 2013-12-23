class Room
    @messageTemplate:
        "<li class='message'>" +
            "<span class='date'><%= new Date().toLocaleTimeString() %>:</span>" +
            " <span class='message-text'><%- message %></span>" +
        "</li>"

    constructor: (@roomName) ->
        @messageTemplate = _.template Room.messageTemplate
        $('#send-form').on 'submit', (e) =>
            e.preventDefault()
            if $('#message-input').val()
                data = $('#send-form').serialize()
                $.post "/#{roomName}/send", data
                $('input[name="message"]').val ""
        @listen()

    listen: ->
        @source = new EventSource("/#{@roomName}/stream")
        @source.addEventListener 'message', (event) =>
            @renderMessage event.data
        , false
    
    renderMessage: (message) ->
        $message = $(@messageTemplate {message: message})
        $message.prependTo '#message-list'

$('document').ready ->
    roomName = location.href.substr (location.href.lastIndexOf('/') + 1)
    room = new Room(roomName)
