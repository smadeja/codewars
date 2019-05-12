require "spec"
require "./find_the_parity_outlier"

describe OutlierFinder do
  describe ".find_outlier" do
    it "finds the outlier" do
      OutlierFinder.find_outlier([2, 2, 3]).should eq(3)
      OutlierFinder.find_outlier([3, 3, 2]).should eq(2)
      OutlierFinder.find_outlier([3, 2, 2]).should eq(3)
      OutlierFinder.find_outlier([2, 3, 3]).should eq(2)
      OutlierFinder.find_outlier([2, 3, 2]).should eq(3)
      OutlierFinder.find_outlier([3, 2, 3]).should eq(2)

      OutlierFinder.find_outlier([2, 4, 0, 100, 4, 11, 2602, 36]).should eq(11)
      OutlierFinder.find_outlier([160, 3, 1719, 19, 11, 13, -21]).should eq(160)
    end

    it "returns nil if there's no outlier to be found" do
      OutlierFinder.find_outlier([2, 2, 2]).should eq(nil)
      OutlierFinder.find_outlier([3, 3, 3]).should eq(nil)
    end
  end
end
