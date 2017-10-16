defmodule AMOC.LoadHelper do

  use Wocky.Repo.Model

  def get_user(id) do
    Wocky.User
    |> order_by(:created_at)
    |> offset(^id)
    |> limit(1)
    |> Repo.all()
    |> hd()
  end

end
