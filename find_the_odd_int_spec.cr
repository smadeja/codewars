require "spec"
require "./find_the_odd_int"

describe "#find_it" do
  it "finds the odd int" do
    find_it([20, 1, -1, 2, -2, 3, 3, 5, 5, 1, 2, 4, 20, 4, -1, -2, 5]).should eq(5)
    find_it([1, 1, 2, -2, 5, 2, 4, 4, -1, -2, 5]).should eq(-1)
    find_it([20, 1, 1, 2, 2, 3, 3, 5, 5, 4, 20, 4, 5]).should eq(5)
    find_it([10]).should eq(10)
    find_it([1, 1, 1, 1, 1, 1, 10, 1, 1, 1, 1]).should eq(10)

    expect_raises(IndexError) do
      find_it([] of Int32)
    end
  end
end
