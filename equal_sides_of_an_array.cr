module ArraySplitter
  def self.split_array(array : Array(Int32)) : Int32?
    if array.empty?
      raise ArgumentError.new
    end

    index = 0
    left_sum = 0
    right_sum = sum_of_all_but_first(array)

    loop do
      if left_sum == right_sum
        return index
      elsif index == array.size - 1
        return nil
      end

      index += 1
      left_sum += array[index - 1]

      if index < array.size
        right_sum -= array[index]
      end
    end
  end

  def self.sum_of_all_but_first(array : Array(Int32)) : Int32
    if array.empty?
      raise ArgumentError.new
    end

    acc = 0

    # The following would work in a newer version of Crystal
    # array.each(start: 1, count: array.size - 1) do |value|
    #   acc += value
    # end

    # A workaround for the missing feature
    (1..(array.size - 1)).each do |index|
      acc += array[index]
    end

    acc
  end
end

def find_even_index(array : Array(Int32)) : Int32
  ArraySplitter.split_array(array) || -1
end
