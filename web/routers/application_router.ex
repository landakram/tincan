defmodule ApplicationRouter do
  use Dynamo.Router

  prepare do
    # Pick which parts of the request you want to fetch
    # You can comment the line below if you don't need
    # any of them or move them to a forwarded router
    conn = conn.fetch([:cookies, :params, :body])
    conn.assign :layout, "main"
  end

  # It is common to break your Dynamo into many
  # routers, forwarding the requests between them:
  # forward "/posts", to: PostsRouter

  get "/:room" do
    conn = conn.assign(:room, conn.params[:room])
    render conn, "room.html"
  end

  get "/:room/stream" do
    conn = conn.resp_content_type "text/event-stream"
    conn = conn.send_chunked 200
    room = conn.params[:room]
    client = Exredis.start
    client_sub = Exredis.Sub.start

    pid = self
    client_sub |> Exredis.Sub.subscribe room, fn message ->
      pid <- message
    end

    client |> Exredis.Api.publish room, "Someone has joined the room."
    client |> Exredis.stop
    subscribe_loop conn, pid, client_sub
  end

  post "/:room/send" do
    client = Exredis.start
    message = conn.params[:message]
    room = conn.params[:room]
    client |> Exredis.Api.publish room, message
    client |> Exredis.stop
    conn.resp 200, "Success"
  end

  defp subscribe_loop(conn, pid, client_sub) do
    receive do 
      {:message, _key, message, _sender} ->
        case conn.chunk "data: #{message}\n\n" do
          {:ok, conn} ->
            subscribe_loop conn, pid, client_sub
          {:error, :closed} ->
            client_sub |> Exredis.stop
        end
    end
  end
end
