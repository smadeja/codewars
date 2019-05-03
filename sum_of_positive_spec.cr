require "spec"
require "./sum_of_positive"

describe "#positive_sum" do
  it "correctly sum all the positive numbers in the array" do
    positive_sum([1, 2, 3, 4, 5]).should eq 15
    positive_sum([1, -2, 3, 4, 5]).should eq 13
    positive_sum([-1, 2, 3, 4, -5]).should eq 9
    positive_sum([] of Int32).should eq 0
    positive_sum([-1, -2, -3, -4, -5]).should eq 0
  end
end
