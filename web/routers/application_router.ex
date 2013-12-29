defmodule ApplicationRouter do
  use Dynamo.Router

  alias Tincan.Registry
  alias Tincan.Room

  prepare do
    conn = conn.fetch([:cookies, :params, :body])
    conn.assign :layout, "main"
  end

  get "/" do
    conn |> redirect to: "/room"
  end

  get "/:room" do
    room = conn.params[:room]
    case :chatrooms |> Registry.get_room(room) do
      nil -> :chatrooms |> Registry.create_room(room)
      _ -> 
    end
    conn = conn.assign(:room, room)
    render conn, "room.html"
  end

  get "/:room/stream" do
    conn = conn.resp_content_type "text/event-stream"
    conn = conn.send_chunked 200
    room = conn.params[:room]
  
    room_pid = :chatrooms |> Registry.get_room(room)
    room_pid |> Room.add_member(self)

    pid = self
    spawn fn -> 
      keep_alive pid, conn
    end

    subscribe_loop conn, pid, room_pid
    conn
  end
  defp subscribe_loop(conn, pid, room_pid) do
    receive do 
      {:message, message} ->
        case conn.chunk "data: #{message}\n\n" do
          {:ok, conn} ->
            subscribe_loop conn, pid, room_pid
          {:error, _reason} ->
            room_pid |> Room.remove_member(pid)
        end
    end
  end

  post "/:room/send" do
    message = conn.params[:message]
    room = conn.params[:room]
    :chatrooms 
    |> Registry.get_room(room)
    |> Room.send_message([from: self, message: message])
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
end
