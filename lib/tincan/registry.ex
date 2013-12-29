defmodule Tincan.Registry do
  use GenServer.Behaviour

  def init(rooms) do
    {:ok, rooms}
  end

  def handle_call({:get_room, room_name}, _from, rooms) do
    room_pid = rooms |> Dict.get(room_name)
    {:reply, room_pid, rooms}
  end

  def handle_cast({:create_room, room_name, pid}, rooms) do
    {:noreply, rooms |> Dict.put(room_name, pid)}
  end

  def handle_cast({:delete_room, room_name}, rooms) do
    {:noreply, rooms |> Dict.delete(room_name)}
  end

  ## Convenience functions 

  def get_room(chatrooms, room_name) do
    chatrooms |> :gen_server.call({:get_room, room_name})
  end

  def create_room(chatrooms, room_name) do
    {:ok, pid} = :gen_server.start_link(Tincan.Room, {room_name, HashSet.new}, [])
    chatrooms |> :gen_server.cast({:create_room, room_name, pid})
  end

  def delete_room(chatrooms, room_name) do
    chatrooms |> :gen_server.cast({:delete_room, room_name})
  end
end
