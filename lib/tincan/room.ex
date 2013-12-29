defmodule Tincan.Room do
  use GenServer.Behaviour

  def init({name, members}) do
    {:ok, {name, members}}
  end

  def handle_call(:get_members, _from, {name, members}) do
    {:reply, members, {name, members}}
  end

  def handle_cast({:add_member, member}, {name, members}) do
    :gen_server.cast(self, 
      {:send_message, 
        [from: member, 
         message: "Someone has joined the room."]})
    {:noreply, {name, members |> Set.put(member)}}
  end

  def handle_cast({:send_message, [from: pid, message: message]}, {name, members}) do
    members |> Set.delete(pid) |> pmap fn(member) -> 
      member <- {:message, message}
    end
    {:noreply, {name, members}}
  end

  def handle_cast({:remove_member, member}, {name, members}) do
    new_members = members |> Set.delete(member)
    if Set.size(new_members) == 0 do
      :chatrooms |> Tincan.RoomRegistry.delete_room(name)
      {:stop, :normal, {name, members}}
    else
      {:noreply, {name, new_members}}
    end
  end

  def send_message(room, payload) do
    room |> :gen_server.cast({:send_message, payload})
  end

  def add_member(room, member) do
    room |> :gen_server.cast({:add_member, member})
  end

  def remove_member(room, member) do
    room |> :gen_server.cast({:remove_member, member})
  end

  defp pmap(collection, f) do
    pid = self
    collection |> Enum.map(fn(el) -> 
      spawn_link fn -> 
        pid <- f.(el)
      end
    end) |> Enum.map (fn(pid) ->
      receive do result -> result end
    end)
  end
end
