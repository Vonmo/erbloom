defmodule BloomExampleTest do
  use ExUnit.Case
  doctest BloomExample

  test "test bloom lib" do
    key = "binkeyfortest"
    {:ok, ref} = :bloom.new(10, 80)
    :ok = :bloom.set(ref, key)
    true = :bloom.check(ref, key)
    false = :bloom.check(ref, "unknown_key")
  end
end
