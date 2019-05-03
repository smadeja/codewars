def positive_sum(array)
  array.reduce(0) do |acc, value|
    value > 0 ? acc + value : acc
  end
end
