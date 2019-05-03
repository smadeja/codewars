ExUnit.start()

defmodule BrainLuckInterpreterTest do
  use ExUnit.Case

  describe "interpret/6" do
    test "navigates, writes, and reads the memory" do
      program =
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.\
>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.--.<+++."

      assert BrainLuckInterpreter.interpret(program) == "HECK"
    end

    test "ignores whitespace" do
      program =
        """
          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.
          >+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.
          --.
          <+++.
        """

      assert BrainLuckInterpreter.interpret(program) == "HECK"
    end

    test "reads input" do
      program = ",>,>,>,<<<.>.>.>."
      assert BrainLuckInterpreter.interpret(program, "BORK") == "BORK"
    end

    test "loops" do
      program = ",[.[-],]"
      assert BrainLuckInterpreter.interpret(program, "BORK" <> <<0>>) == "BORK"
    end
  end
end

defmodule BrainLuckInterpreter.MemoryTest do
  alias BrainLuckInterpreter.Memory
  use ExUnit.Case

  describe "at/2" do
    test "returns the rigth value when it exists in the representation" do
      memory = [0, 8, 7]

      assert Memory.at(memory, 0) == 7
      assert Memory.at(memory, 1) == 8
      assert Memory.at(memory, 2) == 0
    end

    test "returns 0 for uninitialised values" do
      memory = [0, 8, 7]

      assert Memory.at(memory, 3) == 0
      assert Memory.at(memory, 5) == 0
    end

    test "raises when index is out of bounds" do
      memory = [0, 8, 7]

      assert_raise Memory.IndexOutOfBounds, fn ->
        Memory.at(memory, -1)
      end
    end
  end

  describe "increment_at/2" do
    test "increments existing values" do
      memory = [0, 8, 7] |> Memory.increment_at(0) |> Memory.increment_at(2)

      assert Memory.at(memory, 0) == 8
      assert Memory.at(memory, 1) == 8
      assert Memory.at(memory, 2) == 1
    end

    test "increments uninitialised values" do
      memory =
        [0, 8, 7]
        |> Memory.increment_at(3)
        |> Memory.increment_at(5)
        |> Memory.increment_at(3)

      assert Memory.at(memory, 3) == 2
      assert Memory.at(memory, 4) == 0
      assert Memory.at(memory, 5) == 1

      assert memory == [1, 0, 2, 0, 8, 7]
    end

    test "values wrap around at 256" do
      memory = [255] |> Memory.increment_at(0)
      assert Memory.at(memory, 0) == 0
    end

    test "raises when index is out of bounds" do
      memory = [0, 8, 7]

      assert_raise Memory.IndexOutOfBounds, fn ->
        Memory.increment_at(memory, -1)
      end
    end
  end

  describe "decrement_at/2" do
    test "decrements existing values" do
      memory = [7, 3, 2] |> Memory.decrement_at(0) |> Memory.decrement_at(2)

      assert Memory.at(memory, 0) == 1
      assert Memory.at(memory, 1) == 3
      assert Memory.at(memory, 2) == 6
    end

    test "decrements uninitialised values" do
      memory =
        [7, 3, 2]
        |> Memory.decrement_at(3)
        |> Memory.decrement_at(5)
        |> Memory.decrement_at(3)

      assert Memory.at(memory, 3) == 254
      assert Memory.at(memory, 4) == 0
      assert Memory.at(memory, 5) == 255

      assert memory == [255, 0, 254, 7, 3, 2]
    end

    test "values wrap around at 256" do
      memory = [0] |> Memory.decrement_at(0)
      assert Memory.at(memory, 0) == 255
    end

    test "raises when index is out of bounds" do
      memory = [7, 3, 2]

      assert_raise Memory.IndexOutOfBounds, fn ->
        Memory.decrement_at(memory, -1)
      end
    end
  end

  describe "store_at/2" do
    test "overwrites existing values" do
      memory = [0, 8, 7] |> Memory.store_at(0, 2) |> Memory.store_at(2, 3)

      assert Memory.at(memory, 0) == 2
      assert Memory.at(memory, 1) == 8
      assert Memory.at(memory, 2) == 3
    end

    test "writes to uninitialised memory cells" do
      memory =
        [0, 8, 7]
        |> Memory.store_at(3, 2)
        |> Memory.store_at(5, 3)

      assert Memory.at(memory, 3) == 2
      assert Memory.at(memory, 4) == 0
      assert Memory.at(memory, 5) == 3

      assert memory == [3, 0, 2, 0, 8, 7]
    end

    test "values wrap around at 256" do
      memory = [0] |> Memory.store_at(0, 279)
      assert Memory.at(memory, 0) == 23

      memory = [0] |> Memory.store_at(0, -249)
      assert Memory.at(memory, 0) == 7
    end

    test "raises when index is out of bounds" do
      memory = [0, 8, 7]

      assert_raise Memory.IndexOutOfBounds, fn ->
        Memory.store_at(memory, -1, 23)
      end
    end
  end
end

defmodule BrainLuckInterpreter.BracketMatchingTest do
  alias BrainLuckInterpreter.BracketMatching
  use ExUnit.Case

  describe "find_matching_bracket/2" do
    test "finds the closing bracket" do
      assert BracketMatching.find_matching_bracket(",+[-.,+]", 2) == 7
      assert BracketMatching.find_matching_bracket(",[.[-],]", 1) == 7
      assert BracketMatching.find_matching_bracket(",>,<[>[->+>+<<]>>[-<<+>>]<<<-]>>.", 4) == 29
    end

    test "finds the opening bracket" do
      assert BracketMatching.find_matching_bracket(",+[-.,+]", 7) == 2
      assert BracketMatching.find_matching_bracket(",[.[-],]", 7) == 1
      assert BracketMatching.find_matching_bracket(",>,<[>[->+>+<<]>>[-<<+>>]<<<-]>>.", 29) == 4
    end

    test "returns nil when there's no matching closing bracket" do
      assert BracketMatching.find_matching_bracket(",+[-.,+", 2) == nil
      assert BracketMatching.find_matching_bracket(",[.[-],", 1) == nil
      assert BracketMatching.find_matching_bracket(",>,<[>[->+>+<<]>>[-<<+>>]<<<->>.", 4) == nil
    end

    test "returns nil when there's no matching opening bracket" do
      assert BracketMatching.find_matching_bracket(",+-.,+]", 6) == nil
      assert BracketMatching.find_matching_bracket(",.[-],]", 6) == nil
      assert BracketMatching.find_matching_bracket(",>,<>[->+>+<<]>>[-<<+>>]<<<-]>>.", 28) == nil
    end
  end
end

defmodule BrainluckTest do
  use ExUnit.Case

  test "echoes until byte 255 is encoutered" do
    assert Brainluck.brain_luck(",+[-.,+]", "Codewars" <> << 255 >>) == "Codewars"
  end

  test "echoes until byte 0 is encoutered" do
    assert Brainluck.brain_luck(",[.[-],]", "Codewars" <> << 0 >>) == "Codewars"
  end

  test "multiplies two numbers" do
    assert Brainluck.brain_luck(",>,<[>[->+>+<<]>>[-<<+>>]<<<-]>>.", << 8, 9 >>) == << 72 >>
  end
end
