defmodule BloomExample.MixProject do
  use Mix.Project

  def project do
    [
      app: :bloom_example,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:erbloom, "~> 2.1.0-rc.2"}
    ]
  end
end
