defmodule ApplicationRouter do
  use Dynamo.Router

  prepare do
    conn = conn.fetch([:cookies, :params, :body])
    conn.assign :layout, "main"
  end

  get "/" do
    conn |> redirect to: "/room"
  end

  get "/:room" do
    conn = conn.assign(:room, conn.params[:room])
    render conn, "room.html"
  end

  get "/:room/stream" do
    conn = conn.resp_content_type "text/event-stream"
    conn = conn.send_chunked 200
    room = conn.params[:room]
    client = redis_start
    client_sub = redis_sub_start

    pid = self
    client_sub |> Exredis.Sub.subscribe room, fn message ->
      pid <- message
    end

    spawn fn -> 
      keep_alive pid, conn
    end

    client |> Exredis.Api.publish room, "Someone has joined the room."
    client |> Exredis.stop
    subscribe_loop conn, pid, client_sub
    conn
  end

  post "/:room/send" do
    client = redis_start
    message = conn.params[:message]
    room = conn.params[:room]
    client |> Exredis.Api.publish room, message
    client |> Exredis.stop
    conn.resp 200, "Success"
  end

  @doc "Works around Heroku's 55 second request timeout by pinging the client."
  defp keep_alive(pid, conn) do
    await conn, 50000, &on_wake_up(&1, &2), &on_time_out(&1)
    case conn.chunk "ka\n\n" do
      {:ok, conn} ->
        keep_alive pid, conn
      {:error, _reason} -> 
        pid <- {:exit, self}
    end
  end
  defp on_wake_up(_arg1, _arg2) do
  end
  defp on_time_out(_arg1) do
  end

  defp subscribe_loop(conn, pid, client_sub) do
    receive do 
      {:message, _key, message, _sender} ->
        case conn.chunk "data: #{message}\n\n" do
          {:ok, conn} ->
            subscribe_loop conn, pid, client_sub
          {:error, _reason} ->
            client_sub |> Exredis.stop
        end
      {:exit, _sender} ->
        client_sub |> Exredis.stop
    end
  end

  defp redis_start do
    host = System.get_env("REDIS_HOST") |> String.to_char_list!
    port = System.get_env("REDIS_PORT") |> String.to_char_list! |> :string.to_integer |> elem 0
    password = System.get_env("REDIS_PASSWORD") |> String.to_char_list!
    Exredis.start host, port, 0, password
  end

  defp redis_sub_start do
    host = System.get_env("REDIS_HOST") |> String.to_char_list!
    port = System.get_env("REDIS_PORT") |> String.to_char_list! |> :string.to_integer |> elem 0
    password = System.get_env("REDIS_PASSWORD") |> String.to_char_list!
    Exredis.Sub.start host, port, password
  end
end
