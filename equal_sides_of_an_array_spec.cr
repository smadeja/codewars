require "spec"
require "./equal_sides_of_an_array"

describe "ArraySplitter" do
  describe ".sum_of_all_but_first" do
    it "returns the sum of all but the first element" do
      ArraySplitter.sum_of_all_but_first([3, 5, 7, 11]).should eq(23)
    end

    it "returns zero for a one–element array" do
      ArraySplitter.sum_of_all_but_first([3]).should eq(0)
    end

    it "raises an ArgumentError exception for an empty array" do
      expect_raises(ArgumentError) do
        ArraySplitter.sum_of_all_but_first([] of Int32)
      end
    end
  end

  describe ".split_array" do
    it "returns the index of the split if one exists" do
      ArraySplitter.split_array([1, 2, 3, 4, 3, 2, 1]).should eq(3)
      ArraySplitter.split_array([1, 100, 50, -51, 1, 1]).should eq(1)
      ArraySplitter.split_array([20, 10, -80, 10, 10, 15, 35]).should eq(0)
      ArraySplitter.split_array([10, -80, 10, 10, 15, 35, 20]).should eq(6)
      ArraySplitter.split_array([0, 0, 0, 0, 0]).should eq(0)
      ArraySplitter.split_array([23]).should eq(0)
    end

    it "returns nil if no split is possible" do
      ArraySplitter.split_array([-20, -4, 20, -1, -10, -4, 20, -1]).should eq(nil)
      ArraySplitter.split_array((1..100).to_a).should eq(nil)
      ArraySplitter.split_array((-100..-1).to_a).should eq(nil)
    end

    it "raises an ArgumentError for an empty array" do
      expect_raises(ArgumentError) do
        ArraySplitter.split_array([] of Int32)
      end
    end
  end
end
