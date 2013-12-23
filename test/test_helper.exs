Dynamo.under_test(Tincan.Dynamo)
Dynamo.Loader.enable
ExUnit.start

defmodule Tincan.TestCase do
  use ExUnit.CaseTemplate

  # Enable code reloading on test cases
  setup do
    Dynamo.Loader.enable
    :ok
  end
end
