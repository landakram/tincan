defmodule Tincan.Mixfile do
  use Mix.Project

  def project do
    [ app: :tincan,
      version: "0.0.1",
      build_per_environment: true,
      dynamos: [Tincan.Dynamo],
      compilers: [:elixir, :dynamo, :app],
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:cowboy, :dynamo],
      mod: { Tincan, [] } ]
  end

  defp deps do
    [ { :cowboy, github: "extend/cowboy" },
      { :exredis, github: "artemeff/exredis" },
      { :dynamo, "~> 0.1.0-dev", github: "elixir-lang/dynamo" } ]
  end
end
