module OutlierFinder
  def self.find_outlier(integers : Array(Int32 | Int64)) : (Int32 | Int64 | Nil)
    last_even = nil
    last_odd = nil

    integers.each_with_index do |integer, index|
      if integer.even?
        if last_even
          # Looking for the odd one
          return last_odd || find_odd(integers[index + 1, integers.size - index - 1])
        else
          last_even = integer
        end
      else
        if last_odd
          # Looking for the even one
          return last_even || find_even(integers[index + 1, integers.size - index - 1])
        else
          last_odd = integer
        end
      end
    end
  end

  private def self.find_even(integers : Array(Int32 | Int64)) : (Int32 | Int64 | Nil)
    integers.find { |integer| integer.even? }
  end

  private def self.find_odd(integers : Array(Int32 | Int64)) : (Int32 | Int64 | Nil)
    integers.find { |integer| integer.odd? }
  end
end

def find_outlier(integers : Array(Int32 | Int64)) : (Int32 | Int64 | Nil)
  OutlierFinder.find_outlier(integers)
end
