def positive_sum(array : Array(Int32)) : Int32
  array.reduce(0) do |acc, value|
    value > 0 ? acc + value : acc
  end
end
