ExUnit.start()

defmodule ChisanbopTest do
  use ExUnit.Case

  test "valid?/3 returns true for valid representations" do
    assert Chisanbop.valid?("0000000000") == true
    assert Chisanbop.valid?("1111111111") == true
    assert Chisanbop.valid?("0011001110") == true
    assert Chisanbop.valid?("0011011110") == true
    assert Chisanbop.valid?("0011101110") == true
  end

  test "valid?/3 returns false for invalid representations" do
    assert Chisanbop.valid?("0000000100") == false
    assert Chisanbop.valid?("0010000000") == false
    assert Chisanbop.valid?("0000111101") == false
    assert Chisanbop.valid?("1011110000") == false
    assert Chisanbop.valid?("0100000100") == false
    assert Chisanbop.valid?("0100010100") == false
    assert Chisanbop.valid?("0100100100") == false
    assert Chisanbop.valid?("000000!000") == false
  end

  test "to_integer/1 returns the right value for valid representations" do
    assert Chisanbop.to_integer("0000000000") == 0
    assert Chisanbop.to_integer("1111111111") == 99
    assert Chisanbop.to_integer("0011001110") == 23
    assert Chisanbop.to_integer("0011011110") == 28
    assert Chisanbop.to_integer("0011101110") == 73
    assert Chisanbop.to_integer("0011111110") == 78
  end
end

defmodule CountWithFingersTest do
  use ExUnit.Case

  test "decode/1 returns the right value for a valid representation" do
    assert CountWithFingers.decode("0011001110") == 23
  end

  test "decode/1 returns -1 for an invalid representation" do
    assert CountWithFingers.decode("0100000100") == -1
  end
end
